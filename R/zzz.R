.onLoad <- function (libname, pkgname)
{
  napops_dir <- rappdirs::app_dir(appname = "napops")

  # Create napops directory on disk if it doesn't exist
  if (isFALSE(file.exists(napops_dir$data())))
  {
    packageStartupMessage(paste0("Creating data directory at ", napops_dir$data()))
    dir.create(napops_dir$data(), recursive = TRUE)
  }

  # Create database connection and assign to package environment
  conn <- DBI::dbConnect(RSQLite::SQLite(),
                         paste0(napops_dir$data(), "/napops.db"))
  assign("napops_db", conn, envir = parent.env(environment()))

  # Check if any data has been downloaded before
  if (isFALSE(file.exists(paste0(napops_dir$data(), "/date.txt"))))
  {
    packageStartupMessage("NA-POPS database not yet loaded.\nUse fetch_data() to download NA-POPS data.")
  }else
  {
    packageStartupMessage("Connected to NA-POPS Database.")
  }
}
