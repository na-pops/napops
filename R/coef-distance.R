#' Get coefficients of a species for a distance model
#'
#' \code{coef_distance} returns a dataframe of coefficients for the selected species/
#'   distance model combinations.
#'
#' @param species 4-letter banding code (or vector of) for the desired species
#' @param model Numeric or vector of model numbers
#'
#' @importFrom DBI dbGetQuery
#'
#' @return Dataframe of coefficients for all species and distance models selected
#'
#' @examples
#' \dontrun{
#' # Get coefficients for distance model 1 for American Robin (AMRO)
#' coefs <- coef_distance(species = "AMRO", model = 1)
#'
#' # Get coefficients for all distance models for American Robin (AMRO)
#' # Option 1
#' coefs <- coef_distance(species = "AMRO", model = seq(1,5))
#' # Option 2
#' coefs <- coef_distance(species = "AMRO")
#'
#' # Get coefficients for American Robin, Black-throated Blue Warbler, and
#' #  Yellow-throated Warbler for distance models 1, 4, and 5
#' coefs <- coef_distance(species = c("AMRO", "BTBW", "YTWA"),
#'                   model = c(1,4,5))
#'
#' # Get coefficients for all species for distance model 5
#' coefs <- coef_distance(model = 5)
#'
#' # Get coefficients for all species, for all distance models
#' coefs <- coef_distance()
#'}
#'
#' @export
#'

coef_distance <- function(species = NULL,
                          model = NULL)
{
  # Do initial data checking
  check_data_exists()
  if (!is.null(species))
  {
    check_valid_species(species = species, mod = "dis")
  }

  if (!is.null(model))
  {
    check_valid_model(model = model, mod = "dis")
  }

  sql_string <- ""

  if (length(model) > 0)
  {
    if (model == "best")
    {
      # Build initial search string
      sql_string <- paste0(sql_string,
                           "SELECT Species, N, Model, MIN(AIC), Intercept, Road, Forest, RoadForest FROM dis_coef")
    }else
    {
      sql_string <- paste0(sql_string,
                           "SELECT Species, N, Model, AIC, Intercept, Road, Forest, RoadForest FROM dis_coef")
    }
  }else
  {
    sql_string <- paste0(sql_string,
                         "SELECT Species, N, Model, AIC, Intercept, Road, Forest, RoadForest FROM dis_coef")
  }

  sql_string <- build_sql_query(base = sql_string,
                                species = species,
                                model = model)

  df <- DBI::dbGetQuery(conn = napops_db,
                        statement = sql_string)

  names(df) <- c("Species", "N", "Model", "AIC", "Intercept", "Road", "Forest", "RoadForest")

  return(df)
}
