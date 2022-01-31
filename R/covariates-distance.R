#' Get covariate space for distance modelling
#'
#' \code{covariates_distance} returns a dataframe of covariates that were associated
#'   with each sampling event in NA-POPS.
#'
#' @param species 4-letter banding code (or vector of) for the desired species.
#'   Not needed/ignored if all = TRUE
#' @param all If TRUE, will return the set of covariates that were associated
#'   with each sampling event in the entire NA-POPS database; otherwise, will return the covariate matrix
#'   used for each selected species. Default FALSE
#'
#' @importFrom DBI dbGetQuery
#'
#' @return Dataframe of covariates with the following entries:
#'   \item{Species}{Species associated with covariate, only appears if project = FALSE}
#'   \item{Forest}{Forest Coverage}
#'   \item{Road}{Roadside status (1 for on-road, 0 for off-road)}
#'   \item{Method}{Method of distance sampling used}
#'
#' @examples
#'
#' # Get covariates used for Wood Thrush
#' dis_covars <- covariates_distance(species = "WOTH")
#'
#' # Get covariates for entire NA-POPS project
#' dis_covars_all <- covariates_distance(all = TRUE)
#'
#' @export
#'

covariates_distance <- function(species = NULL,
                                all = FALSE)
{
  # Do initial data checking
  check_data_exists()

  sql_string <- "SELECT "

  if (isTRUE(all))
  {
    sql_string <- paste0(sql_string,
                         "* FROM dis_covars")
  }else
  {
    sql_string <- paste0(sql_string,
                         "* FROM dis_species_summary")
    sql_string <- build_sql_query(base = sql_string,
                                  species = species)
  }

  df <- DBI::dbGetQuery(conn = napops:::napops_db,
                        statement = sql_string)

  return(df)
}
