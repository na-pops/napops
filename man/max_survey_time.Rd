% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max-survey-time.R
\name{max_survey_time}
\alias{max_survey_time}
\title{Get maximum time for removal survey method}
\usage{
max_survey_time(code = NULL)
}
\arguments{
\item{code}{Alphabetical code for survey method.}
}
\value{
Numerical value of maximum survey time, in minutes
}
\description{
\code{max_survey_time} returns the maximum survey time, in minutes, for the specified survey code
}
\examples{
\dontrun{
# Return maximum survey time for code "A"
max_survey_time(code = "A")

# If you provide a survey code that doesn't exist, you'll get an error
max_survey_time(code = "QH")
}
}
