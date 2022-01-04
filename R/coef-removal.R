#' Get coefficients of a species for a removal model
#'
#' \code{coef_removal} returns a dataframe of coefficients for the selected species/
#'   removal model combinations.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 9.
#'
#' @return Dataframe of coefficients for all species and removal models selected
#'
#' @examples
#'
#' # Get coefficients for removal model 1 for American Robin (AMRO)
#' coefs <- coef_removal(species = "AMRO", model = 1)
#'
#' # Get coefficients for all removal models for American Robin (AMRO)
#' # Option 1
#' coefs <- coef_removal(species = "AMRO", model = seq(1,9))
#' # Option 2
#' coefs <- coef_removal(species = "AMRO")
#'
#' # Get coefficients for American Robin, Black-throated Blue Warbler, and
#' #  Yellow-throated Warbler for removal models 1, 4, and 7
#' coefs <- coef_removal(species = c("AMRO", "BTBW", "YTWA"),
#'                   model = c(1,4,7))
#'
#' # Get coefficients for all species for removal model 9
#' coefs <- coef_removal(model = 9)
#'
#' # Get coefficients for all species, for all removal models
#' coefs <- coef_removal()
#'
#' @export
#'

coef_removal <- function(species = NULL,
                         model = NULL)
{
  # Do initial data checking
  check_data_exists()

  if (!is.null(species))
  {
    check_valid_species(species = species, mod = "rem")
  }

  if (!is.null(model))
  {
    check_valid_model(model = model, mod = "rem")
  }


  sql_string <- ""

  if (length(model) > 0)
  {
    if (model[1] == "best")
    {
      # Build initial search string
      sql_string <- paste0(sql_string,
                           "SELECT Species, N, Model, MIN(AIC), Intercept, TSSR, TSSR2, OD, OD2 FROM rem_coef")
    }else
    {
      sql_string <- paste0(sql_string,
                           "SELECT Species, N, Model, AIC, Intercept, TSSR, TSSR2, OD, OD2 FROM rem_coef")
    }
  }else
  {
    sql_string <- paste0(sql_string,
                         "SELECT Species, N, Model, AIC, Intercept, TSSR, TSSR2, OD, OD2 FROM rem_coef")
  }

  sql_string <- build_sql_query(base = sql_string,
                                species = species,
                                model = model)

  df <- DBI::dbGetQuery(conn = napops:::napops_db,
                        statement = sql_string)

  names(df) <- c("Species", "N", "Model", "AIC", "Intercept", "TSSR", "TSSR2", "OD", "OD2")

  return(df)
}
