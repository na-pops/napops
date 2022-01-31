#' Check for updated NA-POPS results
#'
#' \code{check_for_updates} checks the NA-POPS Github repository to see if there
#'   are updated NA-POPS results compared to what is stored locally
#'
#' @importFrom rappdirs app_dir
#'
#' @return None
#'
#' @examples
#'
#' # Check for updates to NA-POPS results
#' check_for_updates()
#'
#' @export
#'

check_for_updates <- function()
{
  napops_dir <- rappdirs::app_dir(appname = "napops")

  # Check if app directory exists yet
  if (isFALSE(file.exists(paste0(napops_dir$data(), "/date.txt"))))
  {
    message("NA-POPS data does not yet exist locally. Use fetch_data() to download the most recent NA-POPS results.")
    return()
  }

  # Load the local data date
  local_date <- as.Date(readChar(paste0(napops_dir$data(), "/date.txt"), nchars = 16))

  remote_url <- "https://raw.githubusercontent.com/na-pops/results/master/date.txt"
  remote_date <- as.Date(readChar(remote_url, nchars = 16))

  if (local_date < remote_date)
  {
    message("A new version of NA-POPS results are available to download! Use fetch_data() to download the most recent NA-POPS results.")
  }else
  {
    message("You are using the most recent NA-POPS results.")
  }
}
