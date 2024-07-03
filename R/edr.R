#' Get effective detection radius for a species
#'
#' \code{edr} calculates the effective detection radius (EDR) for the supplied species, given the desired model,
#' roadside status, and forest coverage.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 5. Can also use string "best" for best model chosen by AIC.
#' @param road Survey roadside status, boolean TRUE or FALSE
#' @param forest Forest coverage, proportion between 0 and 1
#' @param pairwise If FALSE (default), returns an effective detection radius for
#' every combination of Road and Forest supplied; if TRUE, returns an effective detection
#' radius for each Road/Forest pair (and so length(road) must equal length(forest))
#' @param quantiles Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL
#' @param samples Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000
#'
#' @importFrom rappdirs app_dir
#'
#' @return Numeric effective detection radius for species
#'
#' @examples
#' \dontrun{
#' # Get the effective detection radius for American Robin ("AMRO"), using the best model
#' #   for a roadside survey with 100% forest coverage
#' edr(species = "AMRO",
#'     model = "best",
#'     road = TRUE,
#'     forest = 1.0)
#'
#' # Same as previous example, but this time with uncertainty, for model 4
#' edr(species = "AMRO",
#'     model = 4,
#'     road = TRUE,
#'     forest = 1.0,
#'     quantiles = c(0.025, 0.975))
#'
#' # Effective detection radius for multiple species, multiple forest coverage, multiple models
#' edr(species = c("AMRO", "AMGO", "BCCH", "SCTA"),
#'     model = c(1,4,5),
#'     road = TRUE,
#'     forest = seq(0, 1, by = 0.1),
#'     quantiles = c(0.025, 0.975))
#'}
#' @export
#'

edr <- function(species = NULL,
                model = NULL,
                road = NULL,
                forest = NULL,
                pairwise = FALSE,
                quantiles = NULL,
                samples = 1000)
{
  dis_vcv_list <- NULL
  rm(dis_vcv_list)

  napops_dir <- NULL
  rm(napops_dir)

  roadside <- NULL
  rm(roadside)

  # Do initial data checking
  check_data_exists()

  if (!is.null(species))
  {
    check_valid_species(species = species, mod = "dis")
  }

  if (!is.null(model))
  {
    check_valid_model(model = model, mod = "dis")
  }

  if (pairwise)
  {
    if (length(road) != length(forest))
    {
      stop("Pairwise set to TRUE but Road and Forest are not the same length.")
    }
  }

  sp_covars <- covariates_distance(species = species)

  if (!is.null(forest))
  {
    if (any(forest > 1) || any(forest < 0))
    {
      stop("Forest coverage values must be between 0 and 1.")
    }

    forest_range <- range(sp_covars$Forest)
    if (any(forest < forest_range[1]) || any(forest > forest_range[2]))
    {
      warning(paste0("You are providing some Forest Coverage values that are outside the training values of [", forest_range[1], ",", forest_range[2], "] for species ", species))
    }
  }

  if (isFALSE(pairwise))
  {
    forest_values <- rep(forest, each = length(road))

    sim_data <- data.frame(Intercept = rep(1, times = length(forest_values)),
                           Forest = forest_values,
                           Road = as.integer(rep(road, length(forest))))
  }else
  {
    sim_data <- data.frame(Intercept = rep(1, times = length(forest)),
                           Forest = forest,
                           Road = as.integer(road))
  }
  sim_data$RoadForest <- sim_data$Road * sim_data$Forest

  design <- sim_data

  coefficients <- coef_distance(species = species,
                                model = model)
  coefficients <- as.numeric(coefficients[, c("Intercept","Forest","Road","RoadForest")])

  if (is.null(quantiles))
  {
    coefficients[which(is.na(coefficients))] <- 0
    tau <- exp(as.matrix(design) %*% coefficients)

    sim_data$EDR_est <- tau
    sim_data <- sim_data[, c("Road", "Forest", "EDR_est")]

    return(sim_data)
  }else
  {
    load(paste0(rappdirs::app_dir(appname = "napops")$data(),
                "/dis_vcv.rda"))
    vcv <- dis_vcv_list[[model]][[species]]
    bootstrap_df <- bootstrap(vcv = vcv,
                              coefficients = coefficients,
                              design = design,
                              quantiles = quantiles,
                              samples = samples,
                              model = "dis")

    return(cbind(sim_data[, c("Road", "Forest")], bootstrap_df))
  }













  if (is.null(species))
  {
    stop("No argument passed for species\n")
  }

  distance <- read.csv(file = paste0(napops_dir$data(), "/distance.csv"))
  species <- toupper(species)

  if (isFALSE(species %in% distance$Species))
  {
    stop(paste0("Species ", species, " does not exist in NA-POPS database.\n"))
  }

  dis_sp <- distance[which(distance$Species == species), ]

  model_number <- NULL
  if (model == "best")
  {
    model_number <- which(dis_sp$aic == min(dis_sp$aic))
    dis_sp <- dis_sp[which(dis_sp$model == model_number), ]
  }else if (model == "full")
  {
    model_number <- 5
    dis_sp <- dis_sp[which(dis_sp$model == 5), ]
  }else if (is.numeric(model))
  {
    if (model < 1 || model > 5)
    {
      stop(paste0(model, " is an invalid model."))
    }else
    {
      model_number <- model
      dis_sp <- dis_sp[which(dis_sp$model == model), ]
    }
  }else
  {
    stop(paste0(model, " is an invalid model."))
  }

  # Return EDR
  if (model_number == 1)
  {
    return(exp(dis_sp$intercept))
  }else if (model_number == 2)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }
    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road))
  }else if (model_number == 3)
  {
    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 forest * dis_sp$forest))
  }else if (model_number == 4)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }

    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road +
                 forest * dis_sp$forest))
  }else if (model_number == 5)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }

    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road +
                 forest * dis_sp$forest +
                 (as.numeric(roadside) * forest * dis_sp$roadforest)))
  }
}
