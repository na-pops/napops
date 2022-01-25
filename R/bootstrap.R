#' Get cue rate for a species
#'
#' \code{bootstrap} calculates the cue rate for the supplied species, given the desired model,
#' ordinal day, and time since sunrise.
#'
#' @param species 4-letter banding code for the desired species
#' @param model Numeric or vector of model numbers ranging from 1 - 9.
#' @param od Ordinal day, numeric digit in the range (1,365)
#' @param tssr Time since sunrise, numeric digit in the range (-10,10)
#' @param pairwise If FALSE (default), returns a cue rate for every combination of OD and TSSR supplied;
#'   if TRUE, returns cue rate for each OD/TSSR pair (and so length(od) must equal length(tssr))
#' @param quantiles Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL
#' @param samples Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000
#'
#' @return Numeric cue rate for species
#'
#' @examples
#'

bootstrap <- function(vcv = NULL,
                      coefficients = NULL,
                      design = NULL,
                      quantiles = NULL,
                      samples = 1000,
                      model = NULL)
{
  zeros_indices <- which(is.na(coefficients)) - 1
  if (length(zeros_indices) > 0)
  {
    coefficients <- coefficients[-which(is.na(coefficients))]
  }

  sim_coef <-tryCatch(
    {
      rbind(coefficients, MASS::mvrnorm(samples, coefficients, vcv))
      #sim_coef <- rbind(coefficients, MASS::mvrnorm(10^4, coefficients, vcv))
    },
    error = function(e)
    {
      return(NA)
    }
  )

  if (is.na(sim_coef[1]))
  {
    # In the case of an error, just output the calculated phi and NA
    # for the upper and lower
    if (length(zeros_indices) > 0)
    {
      coef_zeros <- c(coefficients, rep(0, length(zeros_indices)))
      id <- c(seq_along(coefficients), zeros_indices + 0.5)
      coef_zeros <- coef_zeros[order(id)]
    }else
    {
      coef_zeros <- coefficients
    }

    phi_pred <- exp(as.matrix(design) %*% (coef_zeros))

    warning("Could not calculate bootstrapped uncertainty for this species. Covariance matrix is not positive semi-definite. Returning only estimate.")

    if (model == "rem")
    {
      return(data.frame(CR_est = phi_pred))
    }else if (model == "dis")
    {
      return(data.frame(EDR_est = phi_pred))
    }

  }else
  {
    # Add columns of zeros back in to where NA coefficients were previously
    # See https://stackoverflow.com/a/1495204/5665609 for explanation
    if (length(zeros_indices) > 0)
    {
      coef_zeros <- cbind(sim_coef, matrix(0,
                                           ncol = length(zeros_indices),
                                           nrow = nrow(sim_coef)))
      id <- c(seq_along(sim_coef[1,]), zeros_indices + 0.5)
      coef_zeros <- coef_zeros[,order(id)]
    }else
    {
      coef_zeros <- sim_coef
    }

    phi_pred <- exp(as.matrix(design) %*% t(coef_zeros))
    phi <- as.numeric(phi_pred[,1])

    # Calculate quantiles
    phi_pred <- phi_pred[,-1]
    phi_quantiles <- matrix(0, nrow = length(phi), ncol = length(quantiles))

    for (q in 1:length(quantiles))
    {
      phi_quantiles[,q] <- as.numeric(apply(phi_pred,
                                       1,
                                       quantile,
                                       probs = c(quantiles[q]),
                                       na.rm = TRUE))
    }
    phi_quantiles <- as.data.frame(phi_quantiles)

    if (model == "rem")
    {
      names(phi_quantiles) <- paste0("CR_", quantiles * 100)
      return(cbind(data.frame(CR_est = phi), phi_quantiles))
    }else if (model == "dis")
    {
      names(phi_quantiles) <- paste0("EDR_", quantiles * 100)
      return(cbind(data.frame(EDR_est = phi), phi_quantiles))
    }
  }
}


