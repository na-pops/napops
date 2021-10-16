#' Get cue rate for a species
#'
#' \code{cue_rate} calculates the cue rate for the supplied species, given the desired model,
#' ordinal day, and time since sunrise.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Which model to use? Defaults to the "best" model,
#'   but will accept the string "best" (for the best model as chosen by AIC),
#'   "full" (for the full model with all covariates), or any numeric digit
#'   in the range (1,9) corresponding to models 1 - 9.
#' @param od Ordinal day, numeric digit in the range (1,365)
#' @param tssr Time since sunrise, numeric digit in the range (-10,10)
#'
#' @importFrom rappdirs app_dir
#' @importFrom utils read.csv
#'
#' @return Numeric cue rate for species
#'
#' @export
#'

cue_rate <- function(species = NULL,
                     model = "best",
                     od = NULL,
                     tssr = NULL)
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

  removal <- read.csv(file = paste0(napops_dir$data(), "/removal.csv"))
  species <- toupper(species)

  if (isFALSE(species %in% removal$Species))
  {
    stop(paste0("Species ", species, " does not exist in NA-POPS database.\n"))
  }

  rem_sp <- removal[which(removal$Species == species), ]

  model_number <- NULL
  if (model == "best")
  {
    rem_sp <- rem_sp[1:9, ]
    model_number <- which(rem_sp$aic == min(rem_sp$aic))
    rem_sp <- rem_sp[which(rem_sp$model == model_number), ]
  }else if (model == "full")
  {
    model_number <- 9
    rem_sp <- rem_sp[which(rem_sp$model == 9), ]
  }else if (is.numeric(model))
  {
    if (model < 1 || model > 9)
    {
      stop(paste0(model, " is an invalid model."))
    }else
    {
      model_number <- model
      rem_sp <- rem_sp[which(rem_sp$model == model), ]
    }
  }else
  {
    stop(paste0(model, " is an invalid model."))
  }

  if (model_number == 1)
  {
    return(exp(rem_sp$intercept))
  }else if (model_number == 2)
  {
    if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }
    return(exp(rem_sp$intercept +
                 (tssr/24) * rem_sp$tssr))
  }else if (model_number == 3)
  {
    if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }
    return(exp(rem_sp$intercept +
                 (od/365) * rem_sp$jd))
  }else if (model_number == 4)
  {
    if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }
    return(exp(rem_sp$intercept +
                 (tssr/24) * rem_sp$tssr +
                 ((tssr/24)^2) * rem_sp$tssr2))
  }else if (model_number == 5)
  {
    if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }
    return(exp(rem_sp$intercept +
                 (od/365) * rem_sp$jd +
                 ((od/365)^2) * rem_sp$jd2))
  }else if (model_number == 6)
  {
    if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }else if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }
    return(exp(rem_sp$intercept +
                 (od/365) * rem_sp$jd +
                 (tssr/24) * rem_sp$tssr))
  }else if (model_number == 7)
  {
    if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }else if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }
    return(exp(rem_sp$intercept +
                 (tssr/24) * rem_sp$tssr +
                 ((tssr/24)^2) * rem_sp$tssr2 +
                 (od/365) * rem_sp$jd))
  }else if (model_number == 8)
  {
    if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }else if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }
    return(exp(rem_sp$intercept +
                 (od/365) * rem_sp$jd +
                 ((od/365)^2) * rem_sp$jd2 +
                 (tssr/24) * rem_sp$tssr))
  }else if (model_number == 9)
  {
    if (is.null(od))
    {
      stop("No argument supplied for OD.")
    }else if (is.null(tssr))
    {
      stop("No argument supplied for TSSR.")
    }
    return(exp(rem_sp$intercept +
                 (od/365) * rem_sp$jd +
                 ((od/365)^2) * rem_sp$jd2 +
                 (tssr/24) * rem_sp$tssr +
                 ((tssr/24)^2) * rem_sp$tssr2))
  }
}
