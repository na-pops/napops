#' Check if the models entered are valid in the NA-POPS database
#'
#' \code{check_valid_model} is an internal function that checks for validity in the
#'   model arguments
#'
#' @param model Model number
#' @param mod "dis" or "rem"
#'
#' @return None
#'

check_valid_model <- function(model = NULL,
                              mod = NULL)
{
  if (mod == "dis")
  {
    if (length(model) > 0)
    {
      if (model[1] != "best")
      {
        if (any(is.character(model)))
        {
          stop("Invalid model(s)")
        }else if (any(model < 1) || any(model > 5))
        {
          warning("Invalid models sent to model argument. Ignoring these.",
                  call. = FALSE)
        }
      }else
      {
        if (length(model) > 1)
        {
          stop("Cannot mix string and numbers in models.")
        }
      }
    }
  }else if (mod == "rem")
  {
    if (length(model) > 0)
    {
      if (model[1] != "best")
      {
        if (any(is.character(model)))
        {
          stop("Invalid model(s)")
        }else if (any(model < 1) || any(model > 9))
        {
          warning("Invalid models sent to model argument. Ignoring these.",
                  call. = FALSE)
        }
      }else
      {
        if (length(model) > 1)
        {
          stop("Cannot mix string and numbers in models.")
        }
      }
    }
  }

}
