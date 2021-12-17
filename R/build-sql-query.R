#' Build the SQL search query
#'
#' \code{build_sql_query} is an internal function that builds the SQL query to search
#'   the database for most tables
#'
#' @return String of SQL query
#'

build_sql_query <- function(base = NULL,
                            species = NULL,
                            model = NULL)
{
  sql_string <- base

  # Build search string of selected species
  if (!is.null(species))
  {
    sql_string <- paste0(sql_string, " WHERE Species IN (")
  }
  n_sp <- length(species)
  i <- 1
  for (s in species)
  {
    sql_string <- paste0(sql_string,
                         "\"",
                         s,
                         "\"")

    if (i < n_sp)
    {
      sql_string <- paste0(sql_string,
                           ", ")
    }else
    {
      sql_string <- paste0(sql_string,
                           ")")
    }
    i <- i + 1
  }

  # Build search string of selected models
  if (!is.null(model))
  {
    if (model == "best")
    {
      sql_string <- paste0(sql_string, " GROUP BY Species")
    }else
    {
      if (is.null(species))
      {
        sql_string <- paste0(sql_string, " WHERE Model IN (")
      }else
      {
        sql_string <- paste0(sql_string, " AND Model IN (")
      }

      n_mod <- length(model)
      i <- 1
      for (m in model)
      {
        sql_string <- paste0(sql_string,
                             m)

        if (i < n_mod)
        {
          sql_string <- paste0(sql_string,
                               ", ")
        }else
        {
          sql_string <- paste0(sql_string,
                               ")")
        }
        i <- i + 1
      }
    }

  }

  return(sql_string)
}
