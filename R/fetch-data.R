#' Fetch NA-POPS modelling results
#'
#' \code{fetch_data} downloads all NA-POPS results from the Github repository and assembles
#' the SQLite databae. A package-specific directory is created on the user's computer
#' (see documentation of \code{rappdirs::appdir} for details of where this
#' directory lives), and NA-POPS data is saved to that directory for use by other functions.
#'
#' @param quiet Logical: should download progress be suppressed? Defaults to TRUE
#'
#' @importFrom rappdirs app_dir
#' @importFrom utils download.file
#' @importFrom progress progress_bar
#' @importFrom dplyr bind_rows
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @return None
#'
#' @examples
#'
#' # Fetch NA-POPS data from Github repository and save to disk
#' fetch_data()
#'
#' # If you'd like to see details of the download, you can specify quiet = FALSE
#' fetch_data(quiet = FALSE)
#'
#' @export
#'

fetch_data <- function(quiet = TRUE)
{
  napops_dir <- rappdirs::app_dir(appname = "napops")

  # Create napops directory on disk if it doesn't exist
  if (isFALSE(file.exists(napops_dir$data())))
  {
    message(paste0("Creating data directory at ", napops_dir$data()))
    dir.create(napops_dir$data(), recursive = TRUE)
  }else
  {
    message(paste0("Using data directory at ", napops_dir$data()))
  }

  ####### Download the data from Github (Task 1/2) #####

  message("Downloading results from Github server (Task 1/2)")
  pb <- progress::progress_bar$new(
    format = "\r[:bar] :percent eta: :eta",
    clear = FALSE,
    total = 15,
    width = 100)
  pb$tick(0)

  temp_dir <- tempdir()

  # Download the date file and save to napops appdir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/date.txt",
                destfile = paste0(napops_dir$data(), "/date.txt"),
                method = "curl",
                quiet = quiet)

  # Download rem aic and save to temp dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/aic/rem_aic.rda",
                destfile = paste0(temp_dir, "/rem_aic.rda"),
                method = "curl",
                quiet = quiet)

  # Download dis aic and save to temp dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/aic/dis_aic.rda",
                destfile = paste0(temp_dir, "/dis_aic.rda"),
                method = "curl",
                quiet = quiet)

  # Download distance coefficients and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/coefficients/distance.csv",
                destfile = paste0(temp_dir, "/distance.csv"),
                method = "curl",
                quiet = quiet)

  # Download removal coefficients and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/coefficients/removal.csv",
                destfile = paste0(temp_dir, "/removal.csv"),
                method = "curl",
                quiet = quiet)

  # Download dis_covars.rda and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/quant-summary/dis_covars.rda",
                destfile = paste0(temp_dir, "/dis_covars.rda"),
                method = "curl",
                quiet = quiet)

  # Download dis_species_summary.rda and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/quant-summary/dis_species_summary.rda",
                destfile = paste0(temp_dir, "/dis_species_summary.rda"),
                method = "curl",
                quiet = quiet)

  # Download rem_covars and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/quant-summary/rem_covars.rda",
                destfile = paste0(temp_dir, "/rem_covars.rda"),
                method = "curl",
                quiet = quiet)

  # Download rem_species_summary.rda and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/quant-summary/rem_species_summary.rda",
                destfile = paste0(temp_dir, "/rem_species_summary.rda"),
                method = "curl",
                quiet = quiet)

  # Download species_table.csv and save to temp file
  download.file("https://raw.githubusercontent.com/na-pops/results/master/quant-summary/species_table.csv",
                destfile = paste0(temp_dir, "/species_table.csv"),
                method = "curl",
                quiet = quiet)

  # Download dis_coverage_bcr.rda and save to temp dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/spatial-summary/dis_coverage_bcr.rda",
                destfile = paste0(temp_dir, "/dis_coverage_bcr.rda"),
                method = "curl",
                quiet = quiet)

  # Download rem_coverage_bcr.rda and save to temp dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/spatial-summary/rem_coverage_bcr.rda",
                destfile = paste0(temp_dir, "/rem_coverage_bcr.rda"),
                method = "curl",
                quiet = quiet)

  # Download project_coverage_bcr.rda and save to temp dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/spatial-summary/project_coverage_bcr.rda",
                destfile = paste0(temp_dir, "/project_coverage_bcr.rda"),
                method = "curl",
                quiet = quiet)

  # Download distance covariance matrices and save to napops dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/var-covar/dis_vcv_list.rda",
                destfile = paste0(napops_dir$data(), "/dis_vcv.rda"),
                method = "curl",
                quiet = quiet)

  # Download removal covariance matrices and save to napops dir
  download.file("https://raw.githubusercontent.com/na-pops/results/master/var-covar/rem_vcv_list.rda",
                destfile = paste0(napops_dir$data(), "/rem_vcv.rda"),
                method = "curl",
                quiet = quiet)

  ##### Assemble the database (Task 2/2) ##############

  message("Assembling database (Task 2/2)")

  # Add rem AIC tables to db
  load(paste0(temp_dir, "/rem_aic.rda"))
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "rem_aic",
                    value = data.frame(dplyr::bind_rows(rem_aic,
                                             .id = "Species")),
                    overwrite = TRUE)

  # Add dis AIC table to db
  load(paste0(temp_dir, "/dis_aic.rda"))
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "dis_aic",
                    value = data.frame(dplyr::bind_rows(dis_aic,
                                                        .id = "Species")),
                    overwrite = TRUE)

  # Add rem coef table to db
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "rem_coef",
                    value = read.csv(paste0(temp_dir, "/removal.csv")),
                    overwrite = TRUE)

  # Add dis coef table to db
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "dis_coef",
                    value = read.csv(paste0(temp_dir, "/distance.csv")),
                    overwrite = TRUE)

  # Add dis_covars table to db
  load(paste0(temp_dir, "/dis_covars.rda"))
  names(dis_covars) <- c("Forest", "Road")
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "dis_covars",
                    value = dis_covars,
                    overwrite = TRUE)

  # Add dis_species_summary to db
  load(paste0(temp_dir, "/dis_species_summary.rda"))
  dis_species_summary <- dplyr::bind_rows(dis_species_summary,
                                          .id = "Species")
  names(dis_species_summary) <- c("Species", "Forest", "Road")
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "dis_species_summary",
                    value = dis_species_summary,
                    overwrite = TRUE)

  # Add dis_covars table to db
  load(paste0(temp_dir, "/rem_covars.rda"))
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "rem_covars",
                    value = rem_covars,
                    overwrite = TRUE)

  # Add rem_species_summary to db
  load(paste0(temp_dir, "/rem_species_summary.rda"))
  rem_species_summary <- dplyr::bind_rows(rem_species_summary,
                                          .id = "Species")
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "rem_species_summary",
                    value = rem_species_summary,
                    overwrite = TRUE)

  # Add species_table table to db
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "species",
                    value = read.csv(paste0(temp_dir, "/species_table.csv")),
                    overwrite = TRUE)

  # Add dis_coverage_bcr to db
  load(paste0(temp_dir, "/dis_coverage_bcr.rda"))
  bcr_dis_coverage <- dplyr::bind_rows(bcr_dis_coverage,
                                          .id = "Species")
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "dis_bcr",
                    value = bcr_dis_coverage,
                    overwrite = TRUE)

  # Add rem_coverage_bcr to db
  load(paste0(temp_dir, "/rem_coverage_bcr.rda"))
  bcr_rem_coverage <- dplyr::bind_rows(bcr_rem_coverage,
                                       .id = "Species")
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "rem_bcr",
                    value = bcr_rem_coverage,
                    overwrite = TRUE)

  # Add rem_coverage_bcr to db
  load(paste0(temp_dir, "/project_coverage_bcr.rda"))
  DBI::dbWriteTable(conn = napops:::napops_db,
                    name = "project_coverage",
                    value = bcr_coverage,
                    overwrite = TRUE)

  message("Connected to NA-POPS Database.")

}

