#' Get effective detection radius for a species
#'
#' \code{edr} calculates the effective detection radius (EDR) for the supplied species, given the desired model,
#' roadside status, and forest coverage.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Which model to use? Defaults to the "best" model,
#'   but will accept the string "best" (for the best model as chosen by AIC),
#'   "full" (for the full model with all covariates), or any numeric digit
#'   in the range (1,5) corresponding to models 1 - 5.
#' @param roadside Survey roadside status, boolean TRUE or FALSE
#' @param forest Forest coverage, proportion between 0 and 1
#'
#' @importFrom rappdirs app_dir
#' @importFrom utils read.csv
#'
#' @return Numeric effective detection radius for species
#'
#' @examples
#'
#' # Get the effective detection radius for American Robin ("AMRO"), using the best model
#' #   for a roadside survey under 50% forest coverage.
#' edr(species = "AMRO", model = "best", roadside = TRUE, forest = 0.5)
#'
#' # Same as previous example, but this time with the full model
#' edr(species = "AMRO", model = "full", roadside = TRUE, forest = 0.5)
#'
#' # Use only roadside model, and get EDR for offroad surveys
#' edr(species = "AMRO", model = 2, roadside = FALSE)
#'
#' @export
#'

edr <- function(species = NULL,
                model = "best",
                roadside = NULL,
                forest = NULL)
{
  if (is.null(species))
  {
    stop("No argument passed for species\n")
  }
  napops_dir <- rappdirs::app_dir(appname = "napops")
  # Create napops directory on disk if it doesn't exist
  if (isFALSE(file.exists(napops_dir$data())))
  {
    stop("No NA-POPS data exists locally. Please use fetch_data() to download most recently NA-POPS results.\n")
  }

  distance <- read.csv(file = paste0(napops_dir$data(), "/distance.csv"))
  species <- toupper(species)

  if (isFALSE(species %in% distance$Species))
  {
    stop(paste0("Species ", species, " does not exist in NA-POPS database.\n"))
  }

  dis_sp <- distance[which(distance$Species == species), ]

  model_number <- NULL
  if (model == "best")
  {
    model_number <- which(dis_sp$aic == min(dis_sp$aic))
    dis_sp <- dis_sp[which(dis_sp$model == model_number), ]
  }else if (model == "full")
  {
    model_number <- 5
    dis_sp <- dis_sp[which(dis_sp$model == 5), ]
  }else if (is.numeric(model))
  {
    if (model < 1 || model > 5)
    {
      stop(paste0(model, " is an invalid model."))
    }else
    {
      model_number <- model
      dis_sp <- dis_sp[which(dis_sp$model == model), ]
    }
  }else
  {
    stop(paste0(model, " is an invalid model."))
  }

  # Return EDR
  if (model_number == 1)
  {
    return(exp(dis_sp$intercept))
  }else if (model_number == 2)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }
    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road))
  }else if (model_number == 3)
  {
    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 forest * dis_sp$forest))
  }else if (model_number == 4)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }

    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road +
                 forest * dis_sp$forest))
  }else if (model_number == 5)
  {
    if (is.null(roadside))
    {
      stop("No argument supplied for roadside status.")
    }else if (isFALSE(is.logical(roadside)))
    {
      stop("Invalid argument for roadside status. Must be TRUE or FALSE.")
    }

    if (is.null(forest))
    {
      stop("No argument supplied for forest coverage.")
    }else if (forest < 0 || forest > 1)
    {
      stop("Invalid argument for forest. Must be a proportion between 0 and 1.")
    }

    return(exp(dis_sp$intercept +
                 as.numeric(roadside) * dis_sp$road +
                 forest * dis_sp$forest +
                 (as.numeric(roadside) * forest * dis_sp$roadforest)))
  }
}
