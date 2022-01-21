#' View a list of species modelled in NA-POPS
#'
#' \code{list_species} returns a data frame of species modelled in NA-POPS,
#'   along with which modelling component they are covered by (removal, distance, or both),
#'   and the sample size for each.
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
#' # Get a list of species modelled in NA-POPS
#' sp <- list_species()
#'
#' @export
#'

list_species <- function()
{
  return(DBI::dbGetQuery(conn = napops:::napops_db,
                         statement = "SELECT * from species"))
}
