#' Get covariate space for removal modelling
#'
#' \code{covariates_removal} returns a dataframe of covariates that were associated
#'   with each sampling event in NA-POPS.
#'
#' @param species 4-letter banding code (or vector of) for the desired species.
#'   Not needed/ignored if all = TRUE
#' @param all If TRUE, will return the set of covariates that were associated
#'   with each sampling event in the entire NA-POPS database; otherwise, will return the covariate matrix
#'   used for each selected species. Defaults to FALSE
#'
#' @importFrom DBI dbGetQuery
#'
#' @return Dataframe of covariates with the following entries:
#'   \item{OD}{Ordinal Day}
#'   \item{TSSR}{Time Since Sunrise}
#'   \item{Method}{Removal sampling method}
#'
#' @examples
#'
#' # Get covariates used for Wood Thrush
#' rem_covars <- covariates_removal(species = "WOTH")
#'
#' # Get covariates for entire NA-POPS project
#' rem_covars_all <- covariates_removal(all = TRUE)
#'
#' @export
#'

covariates_removal <- function(species = NULL,
                               all = FALSE)
{
  # Do initial data checking
  check_data_exists()

  sql_string <- "SELECT "

  if (isTRUE(all))
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
