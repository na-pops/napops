% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates-distance.R
\name{covariates_distance}
\alias{covariates_distance}
\title{Get covariate space for distance modelling}
\usage{
covariates_distance(project = TRUE, species = NULL)
}
\arguments{
\item{project}{If TRUE, will return the set of covariates that were associated
with each sampling event in the project; otherwise, will return the covariate matrix
used for each selected species}

\item{species}{4-letter banding code (or vector of) for the desired species.
Not needed/ignored if project = TRUE}
}
\value{
Dataframe of covariates with the following entries:
  \item{Species}{Species associated with covariate, only appears if project = FALSE}
  \item{Forest}{Forest Coverage}
  \item{Road}{Roadside status (1 for on-road, 0 for off-road)}
}
\description{
\code{covariates_distance} returns a dataframe of covariates that were associated
  with each sampling event in NA-POPS.
}
\examples{

rem_covars <- covariates_distance()

}