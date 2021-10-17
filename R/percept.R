#' Get conditional probability of perceiving a bird, given availability
#'
#' \code{percept} calculates the conditional probability that a bird is perceived
#'   by an observer, given that it gives a cue, dependent on the roadside status
#'   of a survey, forest coverage, and maximum survey distance.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Which model to use? Defaults to the "best" model,
#'   but will accept the string "best" (for the best model as chosen by AIC),
#'   "full" (for the full model with all covariates), or any numeric digit
#'   in the range (1,5) corresponding to models 1 - 5.
#' @param roadside Survey roadside status, boolean TRUE or FALSE
#' @param forest Forest coverage, proportion between 0 and 1
#' @param distance Maximum survey distance in metres
#'
#' @return Probability of perceptibility
#'
#' @export
#'

percept <- function(species = NULL,
                    model = "best",
                    roadside = NULL,
                    forest = NULL,
                    distance = NULL)
{
  # All the error checking is done in edr, which will be called in
  #   this function. Only parameter to check is the distance parameter.

  if (is.null(distance))
  {
    stop("Maximum survey distance in metres must be supplied.")
  }

  tau <- tryCatch(
    {
      edr(species, model, roadside, forest)
    },
    error = function(cond)
    {
      stop(cond)
    }
  )

  q <- (pi * tau^2 * (1 - exp(-distance^2 / tau^2))) /
    (pi * distance^2)

  return(q)
}
