% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snapflex.R
\name{flex_params}
\alias{flex_params}
\title{Template parameters}
\usage{
flex_params(template)
}
\arguments{
\item{template}{character, the flexdashboard template. See \code{\link{flex_templates}}.}
}
\value{
a list or \code{NULL}.
}
\description{
List the required and optional parameters for a flexdashboard template passed to \code{flex}.
}
\details{
This function returns a data frame with three columns. The first contains parameter names pertaining to a specific template.
The second is a column of default values. If \code{NA}, the parameter is required to be passed by the user any time the template is rendered using \code{flex}.
Otherwise, the parameter is optional, given that a default value is set by the template if the user ignores the parameter.
The third is a column of hints, containing information regarding what constitutes valid values for the parameters in case you are less familiar with the SNAPverse and SNAP climate data sets.
}
\examples{
flex_params("psc1")
}
\seealso{
flex_templates flex
}
