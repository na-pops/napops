#' Get covariate space for distance modelling
#'
#' \code{get_distance_covariates} returns a dataframe of covariates that were associated
#'   with each sampling event in NA-POPS.
#'
#' @param project If TRUE, will return the set of covariates that were associated
#'   with each sampling event in the project; otherwise, will return the covariate matrix
#'   used for each selected species
#' @param species 4-letter banding code (or vector of) for the desired species.
#'   Not needed/ignored if project = TRUE
#'
#' @return Dataframe of covariates with the following entries:
#'   \item{Species}{Species associated with covariate, only appears if project = FALSE}
#'   \item{Forest}{Forest Coverage}
#'   \item{Road}{Roadside status (1 for on-road, 0 for off-road)}
#'
#' @examples
#'
#' rem_covars <- get_distance_covariates()
#'
#' @export
#'

get_distance_covariates <- function(project = TRUE,
                                   species = NULL)
{
  # Do initial data checking
  check_data_exists()

  sql_string <- "SELECT "

  if (isTRUE(project))
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
