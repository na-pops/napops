#' Get conditional probability of perceiving a bird, given the bird gives a cue
#'
#' \code{percept} calculates the conditional probability that a bird is perceived
#'   by an observer, given that it gives a cue, dependent on the roadside status
#'   of a survey, forest coverage, and maximum survey distance.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 5. Can also use string "best" for best model chosen by AIC.
#' @param road Survey roadside status, boolean TRUE or FALSE
#' @param forest Forest coverage, proportion between 0 and 1
#' @param distance Distance to bird in metres
#' @param pairwise If FALSE (default), returns perceptibility for every combination of Road and Forest supplied;
#'   if TRUE, returns perceptibility for each Road/Forest pair (and so length(road) must equal length(forest))
#' @param quantiles Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL
#' @param samples Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000
#'
#' @return Probability of perceptibility
#'
#' @examples
#'
#' \dontrun{
#' # Get the perceptibility for American Robin ("AMRO"), using the best model
#' #   for a roadside survey with 100% forest coverage, bird is 100m away
#' percept(species = "AMRO",
#'         model = "best",
#'         road = TRUE,
#'         forest = 1.0,
#'         distance = 100)
#'
#' # Same as previous example, but this time with uncertainty, for model 4
#' percept(species = "AMRO",
#'         model = 4,
#'         road = TRUE,
#'         forest = 1.0,
#'         distance = 100)
#'         quantiles = c(0.025, 0.975))
#'
#' # Effective detection radius for multiple species, multiple forest coverage, multiple models
#' percept(species = c("AMRO", "AMGO", "BCCH", "SCTA"),
#'         model = c(1,4,5),
#'         road = TRUE,
#'         forest = seq(0, 1, by = 0.1),
#'         distance = 100)
#'         quantiles = c(0.025, 0.975))
#' }
#' @export
#'

percept <- function(species = NULL,
                    model = NULL,
                    road = NULL,
                    forest = NULL,
                    distance = NULL,
                    pairwise = FALSE,
                    quantiles = NULL,
                    samples = 1000)
{
  # All the error checking is done in edr, which will be called in
  #   this function. Only parameter to check is the distance parameter.

  edr_df <- edr(species = species,
                model = model,
                road = road,
                forest = forest,
                pairwise = pairwise,
                quantiles = quantiles,
                samples = samples)

  tau <- edr_df[, 3:ncol(edr_df)]

  q <- (pi * tau^2 * (1 - exp(-distance^2 / tau^2))) /
    (pi * distance^2)

  names(q)[1] <- "q_est"

  if (ncol(q) > 1)
  {
    names(q)[2:ncol(q)] <- paste0("q_", quantiles * 100)
  }

  q <- cbind(edr_df[, 1:2], q)

  return(q)
}
