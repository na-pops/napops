#' Get vector of first derivatives for probability of perceptibility
#'
#' \code{percept_fd} returns the first derivative of the probability of perceptibility function,
#'   with respect to each parameter,
#'   evaluated at the maximum likelihood estimates for the given species x model combination.
#'   This is useful for propagating the variance in the detectability function through to
#'   a model of counts, if detectability is to be modelled separately from the counts.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 5. Can also use string "best" for best model chosen by AIC.
#' @param road Survey roadside status, boolean TRUE or FALSE
#' @param forest Forest coverage, proportion between 0 and 1
#' @param distance Distance to bird in metres
#' @param pairwise If FALSE (default), returns perceptibility for every combination of Road and Forest supplied;
#'   if TRUE, returns perceptibility for each Road/Forest pair (and so length(road) must equal length(forest))
#'
#' @return Matrix of first derivatives with respect to each parameter, evaluated at MLE
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

percept_fd <- function(species = NULL,
                    model = NULL,
                    road = NULL,
                    forest = NULL,
                    distance = NULL,
                    pairwise = FALSE)
{
  # All the error checking is done in edr, which will be called in
  #   this function. Only parameter to check is the distance parameter.

  # edr_vector <- as.numeric(edr(species = species,
  #               model = model,
  #               road = road,
  #               forest = forest,
  #               pairwise = pairwise,
  #               quantiles = quantiles,
  #               samples = samples)[, 3])

  sim_data <- data.frame(Intercept = rep(1, times = length(forest)),
                         Forest = forest,
                         Road = as.integer(road))
  sim_data$RoadForest <- sim_data$Forest * sim_data$Road

  design <- sim_data

  fd_matrix <- as.matrix(sim_data)
  coefficients <- coef_distance(species = species,
                                model = model)
  coefficients <- as.numeric(coefficients[, c("Intercept","Forest","Road","RoadForest")])
  to_drop <- which(is.na(coefficients))

  coefficients[which(is.na(coefficients))] <- 0

  lin_vector <- as.matrix(design) %*% coefficients

  fd_base <- (((exp(-2 * lin_vector) * (2 * pi * exp(2 * lin_vector))) *
                 (1 - exp(distance^2 * (-exp(-2 * lin_vector)))) -
                 (2 * pi * distance^2 * exp(distance^2 * (-exp(-2 * lin_vector))))) /
                (pi * (1 - exp(distance^2 * (-exp(-2 * lin_vector)))))) - (log(pi^2 * distance^2))

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
