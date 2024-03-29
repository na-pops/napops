% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariates-distance.R
\name{covariates_distance}
\alias{covariates_distance}
\title{Get covariate space for distance modelling}
\usage{
covariates_distance(species = NULL, all = FALSE)
}
\arguments{
\item{species}{4-letter banding code (or vector of) for the desired species.
Not needed/ignored if all = TRUE}

\item{all}{If TRUE, will return the set of covariates that were associated
with each sampling event in the entire NA-POPS database; otherwise, will return the covariate matrix
used for each selected species. Default FALSE}
}
\value{
Dataframe of covariates with the following entries:
  \item{Species}{Species associated with covariate, only appears if project = FALSE}
  \item{Forest}{Forest Coverage}
  \item{Road}{Roadside status (1 for on-road, 0 for off-road)}
  \item{Method}{Method of distance sampling used}
}
\description{
\code{covariates_distance} returns a dataframe of covariates that were associated
  with each sampling event in NA-POPS.
}
\examples{

\dontrun{
# Get covariates used for Wood Thrush
dis_covars <- covariates_distance(species = "WOTH")

# Get covariates for entire NA-POPS project
dis_covars_all <- covariates_distance(all = TRUE)
}
}
