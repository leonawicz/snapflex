% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snapflex.R
\name{flex_templates}
\alias{flex_templates}
\title{Basic metadata for all flexdashboard templates in snapflex}
\usage{
flex_templates()
}
\value{
a data frame.
}
\description{
This function returns a data frame with basic meta data for all flexdashboard templates in \code{snapflex}.
This includes the following information:
}
\details{
\itemize{
  \item the template ID used for generating a local flexdashboard.
  \item a brief description of the template.
  \item the variables included in the flexdashboard.
  \item whether any variables are displayed as deltas in comparison to a baseline rather than as raw values.
  \item intra- and/or inter-annual time period information if applicable.
  \item whether Shiny is used at runtime to launch a local instance of an interactive flexdashboard rather than a standalone html document.
  \item whether the template requires any additional parameters specified by the user.
}

Regarding required template-specific parameters, note that all templates in \code{snapflex} accept some parameters that are optional
rather than required because the templates have set defaults for these if ignored. For example, all templates accept \code{gfont} , \code{regular}, \code{bold} and \code{snaptheme}.
Since optional parameters are always available and are safely ignored, entries in the final column of the data frame are \code{TRUE} only
if a template has required parameters where the user must always specify the value when rendering a template.
Use \code{\link{flex_params}} to view a table of information about all parameters available for a given template.
}
\examples{
flex_templates()
}
\seealso{
flex_templates flex_params
}
