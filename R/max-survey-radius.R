#' Get maximum radius for distance survey method
#'
#' \code{max_survey_radius} returns the maximum survey radius, in metres, for the specified survey code
#'
#' @param code Alphabetical code for survey method.
#'
#' @return Numerical value of maximum survey radius, in metres
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' # Return maximum survey radius for code "A"
#' max_survey_radius(code = "A")
#'
#' # If you provide a survey code that doesn't exist, you'll get an error
#' max_survey_radius(code = "QH")
#' }
#' @export
#'

max_survey_radius <- function(code = NULL)
{
  if (is.null(code))
  {
    stop("Survey method code not entered.")
  }
  survey <- utils::read.csv(system.file("survey",
                                        "distance_lookup.csv",
                                        package = "napops"))

  code <- toupper(code)

  if (isFALSE(toupper(code) %in% survey$Method))
  {
    stop("Invalid survey code. Use covariates_distance() to get valid survey codes.")
  }
  survey <- survey[!duplicated(survey$Method),]

  indices <- match(code, survey$Method)

  return(survey[indices, "Max_Distance"])
}
