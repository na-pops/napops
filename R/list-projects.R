#' View a list of projects in NA-POPS
#'
#' \code{list_projects} returns a data frame of all projects used for NA-POPS analysis.
#'
#' @importFrom DBI dbGetQuery
#'
#' @return Data frame with the following columns
#'   \item{Metaproject}{Name of the umbrella organization that hosts several projects}
#'   \item{Project}{Name of the project}
#' @examples
#'
#' # Get a list of projects in NA-POPS
#' sp <- list_projects()
#'
#' @export
#'

list_projects <- function()
{
  return(DBI::dbGetQuery(conn = napops:::napops_db,
                         statement = "SELECT * from projects"))
}
