#' Get cue rate for a species
#'
#' \code{cue_rate} calculates the cue rate for the supplied species, given the desired model,
#' ordinal day, and time since sunrise.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 9. Can also use string "best" for best model chosen by AIC.
#' @param od Ordinal day, numeric digit or vector
#' @param tssr Time since sunrise, numeric digit or vector
#' @param pairwise If FALSE (default), returns a cue rate for every combination of OD and TSSR supplied;
#'   if TRUE, returns cue rate for each OD/TSSR pair (and so length(od) must equal length(tssr))
#' @param quantiles Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL
#' @param samples Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000
#'
#' @importFrom stats median
#' @importFrom rappdirs app_dir
#'
#' @return Numeric cue rate for species
#'
#' @examples
#'\dontrun{
#' # Get the cue rate for American Robin ("AMRO"), using the best model
#' #   on June 1 (OD = 153), 1 hour after sunrise.
#' cue_rate(species = "AMRO",
#'          model = "best",
#'          od = 153,
#'          tssr = 1)
#'
#' # Same as previous example, but this time with uncertainty, for model 7
#' cue_rate(species = "AMRO",
#'          model = 7,
#'          od = 153,
#'          tssr = 1,
#'          quantiles = c(0.025, 0.975))
#'
#' # Cue rate for multiple species, multiple days, multiple models
#' cue_rate(species = c("AMRO", "AMGO", "BCCH", "SCTA"),
#'          model = c(1,4,6,7,8),
#'          od = seq(90, 180, by = 2),
#'          tssr = 1,
#'          quantiles = c(0.025, 0.975))
#' }
#' @export
#'

cue_rate <- function(species = NULL,
                     model = NULL,
                     od = NULL,
                     tssr = NULL,
                     pairwise = FALSE,
                     quantiles = NULL,
                     samples = 1000)
{
  rem_vcv_list <- NULL
  rm(rem_vcv_list)

  # Do initial data checking
  check_data_exists()

  if (!is.null(species))
  {
    check_valid_species(species = species, mod = "rem")
  }

  if (!is.null(model))
  {
    check_valid_model(model = model, mod = "rem")
  }

  if (pairwise)
  {
    if (length(od) != length(tssr))
    {
      stop("Pairwise set to TRUE but OD and TSSR are not the same length.")
    }
  }

  sp_covars <- covariates_removal(species = species)
  if (!is.null(od))
  {
    od_range <- range(sp_covars$OD)

    if (any(od < od_range[1]) || any(od > od_range[2]))
    {
      warning(paste0("You are providing some OD values that are outside the training values of [", od_range[1], ",", od_range[2], "] for species ", species))
    }
  }

  if (!is.null(tssr))
  {
    tssr_range <- range(sp_covars$TSSR)

    if (any(tssr < tssr_range[1]) || any(tssr > tssr_range[2]))
    {
      warning(paste0("You are providing some TSSR values that are outside the training values of [", tssr_range[1], ",", tssr_range[2], "] for species ", species))
    }
  }

  if (isFALSE(pairwise))
  {
    tssr_values <- rep(tssr, each = length(od))

    sim_data <- data.frame(Intercept = rep(1, times = length(tssr_values)),
                           TSSR = tssr_values,
                           OD = rep(od, length(tssr)))
  }else
  {
    sim_data <- data.frame(Intercept = rep(1, times = length(tssr)),
                           TSSR = tssr,
                           OD = od)
  }

  design <- sim_data
  tssr_median <- stats::median(covariates_removal(species = species)$TSSR)
  design$TSSR <- (design$TSSR - tssr_median) / 24
  design$TSSR2 <- design$TSSR ^ 2

  od_sp_median <- stats::median(covariates_removal(species = species)$OD)
  design$OD <- (design$OD - od_sp_median) / 365
  design$OD2 <- design$OD ^ 2
  design <- design[, c("Intercept", "TSSR", "TSSR2", "OD", "OD2")]

  coefficients <- coef_removal(species = species,
                               model = model)
  coefficients <- as.numeric(coefficients[, c("Intercept","TSSR","TSSR2","OD","OD2")])

  if (is.null(quantiles))
  {
    coefficients[which(is.na(coefficients))] <- 0
    phi <- exp(as.matrix(design) %*% coefficients)

    sim_data$CR_est <- phi
    sim_data <- sim_data[, c("TSSR", "OD", "CR_est")]

    return(sim_data)
  }else
  {
    load(paste0(rappdirs::app_dir(appname = "napops")$data(),
                "/rem_vcv.rda"))
    vcv <- rem_vcv_list[[model]][[species]]
    bootstrap_df <- bootstrap(vcv = vcv,
                              coefficients = coefficients,
                              design = design,
                              quantiles = quantiles,
                              samples = samples,
                              model = "rem")

    return(cbind(sim_data[, c("TSSR", "OD")], bootstrap_df))
  }
}
