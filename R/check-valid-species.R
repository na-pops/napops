#' Check if the species exists in the NA-POPS database
#'
#' \code{check_valid_species} is an internal function that checks for the existence of
#'   the species in the NA-POPS database, given the removal or distance modelling
#'
#' @param species Species 4 letter code
#' @param mod "dis" or "rem"
#'
#' @importFrom DBI dbGetQuery
#'
#' @return None
#'

check_valid_species <- function(species = NULL,
                                mod = NULL)
{
  sql_string <- "SELECT Species, Removal, Distance FROM species"
  sql_string <- napops:::build_sql_query(base = sql_string,
                                         species = species)

  in_db <- ifelse(mod == "rem",
                  DBI::dbGetQuery(conn = napops:::napops_db,
                                  statement = paste0(sql_string, " AND Removal == 1")),
                  DBI::dbGetQuery(conn = napops:::napops_db,
                                  statement = paste0(sql_string, " AND Distance == 1")))[[1]]

  not_in <- setdiff(species, in_db)

  if (length(not_in) > 0)
  {
    warning(paste0("The following species do not exist for the selected model type in NA-POPS:\n", not_in),
            call. = FALSE)
  }
}
