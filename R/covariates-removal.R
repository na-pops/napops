#' Get covariate space for removal modelling
#'
#' \code{covariates_removal} returns a dataframe of covariates that were associated
#'   with each sampling event in NA-POPS.
#'
#' @param project If TRUE, will return the set of covariates that were associated
#'   with each sampling event in the project; otherwise, will return the covariate matrix
#'   used for each selected species
#' @param species 4-letter banding code (or vector of) for the desired species.
#'   Not needed/ignored if project = TRUE
#'
#' @return Dataframe of covariates with the following entries:
#'   \item{OD}{Ordinal Day}
#'   \item{TSSR}{Time Since Sunrise}
#'
#' @examples
#'
#' rem_covars <- covariates_removal()
#'
#' @export
#'

covariates_removal <- function(project = TRUE,
                                   species = NULL)
{
  # Do initial data checking
  check_data_exists()

  sql_string <- "SELECT "

  if (isTRUE(project))
  {
    sql_string <- paste0(sql_string,
                         "* FROM rem_covars")
  }else
  {
    sql_string <- paste0(sql_string,
                         "* FROM rem_species_summary")
    sql_string <- build_sql_query(base = sql_string,
                                  species = species)
  }

  df <- DBI::dbGetQuery(conn = napops:::napops_db,
                        statement = sql_string)

  return(df)
}
