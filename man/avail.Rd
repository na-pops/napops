% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avail.R
\name{avail}
\alias{avail}
\title{Get probability of availability for species}
\usage{
avail(
  species = NULL,
  model = NULL,
  od = NULL,
  tssr = NULL,
  time = NULL,
  pairwise = FALSE,
  quantiles = NULL,
  samples = 1000
)
}
\arguments{
\item{species}{4-letter banding code for the desired species}

\item{model}{Numeric or vector of model numbers ranging from 1 - 9. Can also use string "best" for best model chosen by AIC.}

\item{od}{Ordinal day, numeric digit or vector}

\item{tssr}{Time since sunrise, numeric digit or vector}

\item{time}{Maximum survey time in minutes, numeric digit or vector}

\item{pairwise}{If FALSE (default), returns a cue rate for every combination of OD and TSSR supplied;
if TRUE, returns probability of availability for each OD/TSSR pair (and so length(od) must equal length(tssr))}

\item{quantiles}{Optional range of quantiles to calculate bootstrapped uncertainty about the estimate. Defaults to NULL}

\item{samples}{Number of bootstrap samples if bootstrapped uncertainty is to be calculated. Defaults to 1000}
}
\value{
Probability of availability
}
\description{
\code{avail} calculates the probability that the specified species will give
  a cue within a maximum time range, given the day of the year and time of day.
}
\examples{
\dontrun{
# Get the probability of availability for American Robin ("AMRO"), using the best model
#   on June 1 (OD = 153), 1 hour after sunrise, for a survey of 5 minutes.
avail(species = "AMRO",
      model = "best",
      od = 153,
      tssr = 1,
      time = 5)

# Same as previous example, but this time with uncertainty, for model 7
avail(species = "AMRO",
      model = 7,
      od = 153,
      tssr = 1,
      time = 5,
      quantiles = c(0.025, 0.975))

# Availability for multiple species, multiple days, multiple models
avail(species = c("AMRO", "AMGO", "BCCH", "SCTA"),
      model = c(1,4,6,7,8),
      od = seq(90, 180, by = 2),
      tssr = 1,
      time = 5,
      quantiles = c(0.025, 0.975))

 }

}
