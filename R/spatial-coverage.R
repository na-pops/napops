#' Get spatial coverage of NA-POPS data
#'
#' \code{get_spatial_covereage} returns a Simple Features(sf) object for the selected
#'   model x species combination. This function can also return the overall project
#'   coverage for all species
#'
#' @param model "rem" for removal, "dis" for distance, "all" for overall coverage (default);
#'   cannot be a vector of models
#' @param species 4-letter banding code (or vector of) for the desired species.
#'   Not needed/ignored if model = "all"
#'
#' @return Simple Features (sf) object for select model x species combination
#'
#' @examples
#'
#' @export
#'

spatial_coverage <- function(model = "all",
                                 species = NULL)
{
  sf::sf_use_s2(FALSE)

  # Do initial data checking
  napops:::check_data_exists()
  if (isFALSE(model %in% c("rem", "dis", "all")))
  {
    stop("Invalid model.")
  }
  if (length(model) > 1)
  {
    stop("spatial_coverage() cannot handle multiple models.")
  }
  if (!is.null(species))
  {
    if (model != "all")
    {
      check_valid_species(species = species, mod = model)
    }
  }

  sql_string <- ""

  if (model == "all")
  {
    sql_string <- paste0(sql_string,
                         "SELECT BCR, ncounts, nc_cat FROM project_coverage")
  }else if (model == "dis")
  {
    sql_string <- paste0(sql_string,
                         "SELECT Species, BCR, ncounts, nc_cat FROM dis_bcr")
    sql_string <- napops:::build_sql_query(base = sql_string, species = species)
  }else if (model == "rem")
  {
    sql_string <- paste0(sql_string,
                         "SELECT Species, BCR, ncounts, nc_cat FROM rem_bcr")
    sql_string <- napops:::build_sql_query(base = sql_string, species = species)
  }

  df <- DBI::dbGetQuery(conn = napops:::napops_db,
                        statement = sql_string)

  map <- sf::read_sf(dsn = system.file("maps",
                                       package = "napops"),
                     layer = "BBS_BCR_strata",
                     quiet = TRUE)

  df <- merge(df, map, by.x = "BCR", by.y = "ST_12")

  return(sf::st_as_sf(df))
}
