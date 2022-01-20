#' Get maximum time for removal survey method
#'
#' \code{max_survey_time} returns the maximum survey time, in minutes, for the specified survey code
#'
#' @param code Alphabetical code for survey method.
#'
#' @return Numerical value of maximum survey time, in minutes
#'
#' @examples
#'
#'
#' @export
#'

max_survey_time <- function(code = NULL)
{
  if (is.null(code))
  {
    stop("Survey method code not entered.")
  }
  survey <- utils::read.csv(system.file("survey",
                                        "time_lookup.csv",
                                        package = "napops"))

  code <- toupper(code)

  if (isFALSE(toupper(code) %in% survey$Method))
  {
    stop("Invalid survey code. Use covariates_removal() to get valid survey codes.")
  }
  survey <- survey[!duplicated(survey$Method),]

  return(survey[survey$Method == code, ncol(survey)])
}
