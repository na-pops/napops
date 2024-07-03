#' Bootstrap estimates, internal function
#'
#' \code{bootstrap} calculates a bootstrapped distribution for either cue rate or effective
#'   detection radius. Internal function.
#'
#' @param vcv Variance-covariance matrix
#' @param coefficients Point estimates of coefficients to bootstrap around
#' @param design Design matrix to produce values
#' @param quantiles Vector of quantiles to return bootstrapped point estimates of
#' @param samples Number of bootstrap samples. Defaults to 1000
#' @param model Which modelling technique, "rem" or "dis"
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats quantile
#'
#' @return Numeric cue rate for species
#'
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
      phi_quantiles[,q] <- quantile(phi_pred, probs = quantiles[q], na.rm = TRUE)
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


