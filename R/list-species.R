#' View a list of species modelled in NA-POPS
#'
#' \code{list_species} returns a data frame of species modelled in NA-POPS,
#'   along with which modelling component they are covered by (removal, distance, or both),
#'   and the sample size for each.
#'
#' @param model Return species for both models (\code{model = "both"}, default), removal
#'   models only (\code{model = "rem"}), or distance models only (\code{model = "dis"}).
#'
#' @importFrom rappdirs app_dir
#' @importFrom utils download.file
#'
#' @return Data frame with the following columns
#'   \item{Species}{Species 4-letter code of species}
#'   \item{n}{Sample size of species. This column appears only if "rem" or "dis" are selected for model}
#'   \item{Removal}{Boolean TRUE/FALSE for species modelled by removal models. Appears only if "both" are selected for model.}
#'   \item{n_removal}{Sample size of species for removal models. Appears only if "both" are selected for model.}
#'   \item{Distance}{Boolean TRUE/FALSE for species modelled by distance models. Appears only if "both" are selected for model.}
#'   \item{n_distance}{Sample size of species for distance models. Appears only if "both" are selected for model.}
#'
#' @examples
#'
#' # Get a list of species for both models
#' sp <- list_species()
#'
#' # Get a list of only species modelled using removal modelling
#' sp_rem <- list_species(model = "rem")
#'
#' # Get a list of only species modelled using distance modelling
#' sp_dis <- list_species(model = "dis")
#' @export
#'

list_species <- function(model = "both")
{
  napops_dir <- rappdirs::app_dir(appname = "napops")
  # Create napops directory on disk if it doesn't exist
  if (isFALSE(file.exists(napops_dir$data())))
  {
    stop("No NA-POPS data exists locally. Please use fetch_data() to download most recently NA-POPS results.\n")
  }

  removal <- read.csv(file = paste0(napops_dir$data(), "/removal.csv"))
  removal <- removal[!duplicated(removal$Species), c("Species", "n")]
  distance <- read.csv(file = paste0(napops_dir$data(), "/distance.csv"))
  distance <- distance[!duplicated(distance$Species), c("Species", "n")]

  if (model == "dis")
  {
    return(distance)
  }else if (model == "rem")
  {
    return(removal)
  }else if (model == "both")
  {
    combined_sp <- union(removal$Species, distance$Species)
    combined_df <- data.frame(Species = combined_sp,
                              Removal = combined_sp %in% removal$Species,
                              Distance = combined_sp %in% distance$Species)
    combined_df <- merge(combined_df, removal, by = "Species", all = TRUE)
    names(combined_df)[ncol(combined_df)] <- "n_removal"

    combined_df <- merge(combined_df, distance, by = "Species", all = TRUE)
    names(combined_df)[ncol(combined_df)] <- "n_distance"

    to_return <- combined_df[, c("Species",
                                 "Removal",
                                 "n_removal",
                                 "Distance",
                                 "n_distance")]
    return(to_return)
  }else
  {
    stop("Invalid argument for model.")
  }
}
