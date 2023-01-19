#' Get variance-covariance matrix for a species
#'
#' \code{get_vcv} calculates the cue rate for the supplied species, given the desired model,
#' ordinal day, and time since sunrise.
#'
#' @param species 4-letter banding code for the desired species
#' @param model_type "rem" for removal, "dis" for distance
#' @param model_num Numeric or vector of model numbers ranging from 1 - 9
#'   for model_type = "rem", or ranging from 1-5 for model_type = "dis".
#'
#' @importFrom rappdirs app_dir
#'
#' @return Variance-covariance matrix
#'
#' @export
#'

get_vcv <- function(species = NULL,
                    model_type = NULL,
                    model_num = NULL)
{
  rem_vcv_list <- NULL
  rm(rem_vcv_list)

  dis_vcv_list <- NULL
  rm(dis_vcv_list)

  # Do initial data checking
  check_data_exists()

  if (!is.null(species))
  {
    check_valid_species(species = species, mod = model_type)
  }

  if (!is.null(model_type) & !is.null(model_num))
  {
    check_valid_model(model = model_num, mod = model_type)
  }else{
    stop("Invalid Model number or model type.")
  }

  if (model_type == "dis")
  {
    load(paste0(rappdirs::app_dir(appname = "napops")$data(),
                "/dis_vcv.rda"))
    return(dis_vcv_list[[model_num]][[species]])
  }else if (model_type == "rem"){
    load(paste0(rappdirs::app_dir(appname = "napops")$data(),
                "/rem_vcv.rda"))
    return(rem_vcv_list[[model_num]][[species]])
  }

}
