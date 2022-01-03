#' Check for existance of NA-POPS data
#'
#' \code{check_data_exists} is an internal function that checks for the existance of
#'   NA-POPS data on the user's machine
#'
#' @importFrom rappdirs app_dir
#'
#' @return None
#'

check_data_exists <- function()
{
  napops_dir <- rappdirs::app_dir(appname = "napops")

  # Check if app directory exists yet
  if (isFALSE(file.exists(paste0(napops_dir$data(), "/date.txt"))))
  {
    stop("NA-POPS data does not yet exist locally. Use fetch_data() to download the most recent NA-POPS results.")
  }
}
