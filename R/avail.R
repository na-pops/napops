#' Get probability of availability for species
#'
#' \code{avail} calculates the probability that the specified species will give
#'   a cue within a maximum time range, given the day of the year and time of day.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Which model to use? Defaults to the "best" model,
#'   but will accept the string "best" (for the best model as chosen by AIC),
#'   "full" (for the full model with all covariates), or any numeric digit
#'   in the range (1,9) corresponding to models 1 - 9.
#' @param od Ordinal day, numeric digit in the range (1,365)
#' @param tssr Time since sunrise, numeric digit in the range (-10,10)
#' @param time Maximum survey time in minutes
#'
#' @return Probability of availability
#'
#' @export
#'

avail <- function(species = NULL,
                  model = "best",
                  od = NULL,
                  tssr = NULL,
                  time = NULL)
{
  # All the error checking is done in cue_rate, which will be called in
  #   this function. Only parameter to check is the time parameter.

  if (is.null(time))
  {
    stop("Maximum survey time in minutes must be supplied.")
  }

  p <- tryCatch(
    {
      1 - exp(-time * cue_rate(species, model, od, tssr))
    },
    error = function(cond)
    {
      stop(cond)
    }
  )
  return(p)
}
