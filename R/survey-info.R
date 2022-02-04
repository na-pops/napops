#' Get attributes for survey method
#'
#' \code{survey_info} returns a list of attributes associated with the survey method code.
#'
#' @param code Alphabetical code for survey method.
#' @param model "rem" for removal model, "dis" for distance model
#'
#' @return List with the following entries:
#'   \item{Method}{Alphabetical code for the survey code}
#'   \item{Model}{"rem" or "dis" depending on user's entry}
#'   \item{Description}{Description of the distance or time bins in this method}
#'   \item{Max_Interval}{Maximum time or distance considered for this method}
#'   \item{Units}{"Minutes" if removal model selected, "Metres" if distance model selected}
#'
#' @importFrom utils read.csv
#'
#' @examples
#'
#' # Look up survey information for removal survey method "R"
#' survey_info(code = "R", model = "rem")
#'
#' # Look up survey information for distance survey method "AA"
#' survey_info(code = "AA", model = "dis")
#' @export
#'

survey_info <- function(code = NULL,
                        model = NULL)
{
  if (is.null(code))
  {
    stop("Survey method code not entered.")
  }

  if (is.null(model) || isFALSE(model %in% c("rem", "dis")))
  {
    stop("Enter \"dis\" or \"rem\" for the model.")
  }

  if (model == "rem")
  {
    survey <- utils::read.csv(system.file("survey",
                                          "time_lookup.csv",
                                          package = "napops"))
  }
  if (model == "dis")
  {
    survey <- utils::read.csv(system.file("survey",
                                          "distance_lookup.csv",
                                          package = "napops"))
  }

  code <- toupper(code)

  if (isFALSE(toupper(code) %in% survey$Method))
  {
    stop("Invalid survey code. Use covariates_removal() or covariates_distance() to get valid survey codes.")
  }

  survey <- survey[!duplicated(survey$Method),]

  to_return <- list(Method = toupper(code),
                    Model = model,
                    Description = survey[survey$Method == code, "Description"],
                    Max_Interval = survey[survey$Method == code, ncol(survey)])

  if (model == "rem")
  {
    to_return$Units <- "Minutes"
  }else
  {
    to_return$Units <- "Metres"
  }

  return(to_return)
}
