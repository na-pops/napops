#' Get vector of first derivatives for probability of availability
#'
#' \code{avail_fd} returns the first derivative of the probability of availability function,
#'   with respect to each parameter,
#'   evaluated at the maximum likelihood estimates for the given species x model combination.
#'   This is useful for propagating the variance in the detectability function through to
#'   a model of counts, if detectability is to be modelled separately from the counts.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 9. Can also use string "best" for best model chosen by AIC.
#' @param od Ordinal day, numeric digit or vector
#' @param tssr Time since sunrise, numeric digit or vector
#' @param time Maximum survey time in minutes, numeric digit or vector
#' @param pairwise If FALSE (default), returns a cue rate for every combination of OD and TSSR supplied;
#'   if TRUE, returns probability of availability for each OD/TSSR pair (and so length(od) must equal length(tssr))
#'
#' @return Matrix of first derivatives with respect to each parameter, evaluated at MLE
#'
#' @examples
#' \dontrun{
#' # Get the probability of availability for American Robin ("AMRO"), using the best model
#' #   on June 1 (OD = 153), 1 hour after sunrise, for a survey of 5 minutes.
#' avail(species = "AMRO",
#'       model = "best",
#'       od = 153,
#'       tssr = 1,
#'       time = 5)
#'
#' # Same as previous example, but this time with uncertainty, for model 7
#' avail(species = "AMRO",
#'       model = 7,
#'       od = 153,
#'       tssr = 1,
#'       time = 5,
#'       quantiles = c(0.025, 0.975))
#'
#' # Availability for multiple species, multiple days, multiple models
#' avail(species = c("AMRO", "AMGO", "BCCH", "SCTA"),
#'       model = c(1,4,6,7,8),
#'       od = seq(90, 180, by = 2),
#'       tssr = 1,
#'       time = 5,
#'       quantiles = c(0.025, 0.975))
#'
#'  }
#'
#' @export
#'

avail_fd <- function(species = NULL,
                  model = NULL,
                  od = NULL,
                  tssr = NULL,
                  time = NULL,
                  pairwise = FALSE)
{
  # All the error checking is done in cue_rate, which will be called in
  #   this function. Only parameter to check is the time parameter.

  if (is.null(time))
  {
    stop("Maximum survey time in minutes must be supplied.")
  }

  sim_data <- data.frame(Intercept = rep(1, times = length(tssr)),
                         TSSR = tssr,
                         OD = od)

  design <- sim_data
  tssr_median <- stats::median(covariates_removal(species = species)$TSSR)
  design$TSSR <- (design$TSSR - tssr_median) / 24
  design$TSSR2 <- design$TSSR ^ 2

  od_sp_median <- stats::median(covariates_removal(species = species)$OD)
  design$OD <- (design$OD - od_sp_median) / 365
  design$OD2 <- design$OD ^ 2
  design <- design[, c("Intercept", "TSSR", "TSSR2", "OD", "OD2")]

  # Need these lines of code to figure out which coefficients to drop
  coefficients <- coef_removal(species = species,
                               model = model)
  coefficients <- as.numeric(coefficients[, c("Intercept","TSSR","TSSR2","OD","OD2")])
  to_drop <- which(is.na(coefficients))

  coefficients[which(is.na(coefficients))] <- 0
  fd_base <- (time * exp(as.matrix(design) %*% coefficients))/(exp(time * exp(as.matrix(design) %*% coefficients)) - 1)

  if (length(to_drop) > 0)
  {
    fd_matrix <- as.matrix(design[, -to_drop])
  }else{
    fd_matrix <- as.matrix(design)
  }

  for (i in 1:ncol(fd_matrix))
  {
    fd_matrix[,i] <- fd_matrix[,i] * fd_base
  }

  return(fd_matrix)
}
