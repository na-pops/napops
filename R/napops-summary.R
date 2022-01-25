#' Get summary statistics for the NA-POPS project
#'
#' \code{napops_summary} returns a list of summary statistics for the entire NA-POPS project.
#'
#' @return List with the following entries:
#'   \item{Date}{Date that the models were run (functions as the version)}
#'   \item{Observations}{Number of landbird observations}
#'   \item{Samples}{Total number of point counts analysed}
#'   \item{Samples_Removal}{Total number of point counts with removal sampling}
#'   \item{Samples_Distance}{Total number of point counts with distance sampling}
#'   \item{Projects}{Total number of projects compiled in NA-POPS database}
#'   \item{Species}{Total number of species that have cue rates or effective detection radius derived}
#'
#' @examples
#'
#' # This function can simply be called with
#' summary <- napops_summary()
#'
#' @export
#'

napops_summary <- function()
{
  napops_dir <- rappdirs::app_dir(appname = "napops")

  # Check if app directory exists yet
  if (isFALSE(file.exists(paste0(napops_dir$data(), "/date.txt"))))
  {
    message("NA-POPS data does not yet exist locally. Use fetch_data() to download the most recent NA-POPS results.")
    return()
  }

  load(paste0(napops_dir$data(), "/summary_statistics.rda"))

  local_date <- as.Date(readChar(paste0(napops_dir$data(), "/date.txt"), nchars = 16))

  return(list(Date = local_date,
              Observations = summary_stats$n_observations,
              Samples = summary_stats$n_samples,
              Samples_Removal = summary_stats$n_rem_samples,
              Samples_Distance = summary_stats$n_dis_samples,
              Projects = summary_stats$total_projects,
              Species = summary_stats$n_species))
}
