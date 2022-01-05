#' Get probability of availability for species
#'
#' \code{avail} calculates the probability that the specified species will give
#'   a cue within a maximum time range, given the day of the year and time of day.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 9.
#' @param od Ordinal day, numeric digit or vector
#' @param tssr Time since sunrise, numeric digit or vector
#' @param time Maximum survey time in minutes, numeric digit or vector
#' @param pairwise If FALSE (default), returns a cue rate for every combination of OD and TSSR supplied;
#'   if TRUE, returns probability of availability for each OD/TSSR pair (and so length(od) must equal length(tssr))
#' @param quantiles Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL
#' @param samples Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000
#'
#' @return Probability of availability
#'
#' @examples
#'
#' @export
#'

avail <- function(species = NULL,
                  model = NULL,
                  od = NULL,
                  tssr = NULL,
                  time = NULL,
                  pairwise = FALSE,
                  quantiles = NULL,
                  samples = 1000)
{
  # All the error checking is done in cue_rate, which will be called in
  #   this function. Only parameter to check is the time parameter.

  if (is.null(time))
  {
    stop("Maximum survey time in minutes must be supplied.")
  }

  cr_df <- cue_rate(species = species,
                    model = model,
                    od = od,
                    tssr = tssr,
                    pairwise = pairwise,
                    quantiles = quantiles,
                    samples = samples)

  p <- 1 - exp(-time * cr_df[, 3:ncol(cr_df)])

  names(p)[1] <- "p_est"

  if (ncol(p) > 1)
  {
    names(p)[2:ncol(p)] <- paste0("p_", quantiles * 100)
  }

  p <- cbind(cr_df[, 1:2], p)

  return(p)
}
