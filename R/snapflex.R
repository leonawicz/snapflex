#' snapflex: flexdashboards for SNAP data sets.
#'
#' Provides functions and templates for generating local flexdashboards showcasing and summarizing SNAP data sets as well as functions for accessing existing popular SNAP flexdashboards hosted online.
#' \code{snapflex} is a member package in the SNAPverse collection of R packages.
#'
#' @docType package
#' @name snapflex
NULL

#' @importFrom magrittr %>%
NULL

#' Generate flexdashboards for SNAP data sets
#'
#' Generate local flexdashboards for SNAP data sets from package templates.
#'
#' This function creates a flexdashboard from an existing template provided by the package.
#' If a given template requires custom arguments to be provided, for example a location name,
#' these arguments can be passed as a named list to \code{params}.
#'
#' Some templates produce static documents. Others use Shiny at runtime.
#' By default, templates of the former type launch automatically in the default browser when generated.
#' Similarly, the latter type automatically launches a local instance of a flexdashboard with Shiny backend support for additional interactivity.
#' The static document templates also save the resulting standalone html document to \code{out_dir}, defaulting to the working directory.
#' In this case, \code{load_static = FALSE} will toggle off the automatic launch for static flexdashboards.
#'
#' Note that static does not mean the flexdashboard html document offers no interactivity.
#' A template may still offer the ability to switch between different graphs and tables and explore different content.
#' However, all data is embedded in the document. This makes it useful as a standalone file requiring no Shiny server backend,
#' but it also means that more complex static templates result in larger output file sizes and they cannot achieve anywhere near the
#' complexity of a flexdashboard that uses Shiny.
#'
#' @param template character, the ID of the flexdashboard template. See \code{\link{flex_templates}} for available IDs and descriptions.
#' @param out_dir character, output directory for standalone html document when \code{template} refers to a static (non-Shiny) flexdashboard.
#' @param params named list, additional parameters passed to the template if required. See \code{\link{flex_params}} for more information.
#'
#' @export
#'
#' @seealso flex_templates flex_params
#' @examples
#' \dontrun{flex("psc1", params = list(location = "Fairbanks"))}
flex <- function(template, out_dir = ".", params = NULL){
  path <- system.file("flex", package = "snapflex")
  path <- switch(template, "psc1" = file.path(path, "flex-clim-1.Rmd"))
  if(template %in% c("psc1")){
    no_loc <- "This template requires a point `location`, e.g., 'Anchorage'. See snaplocs::locs for options."
    if(!is.list(params) || !location %in% names(params)) stop(no_loc)
    rmarkdown::render(path, output_file = paste0(template, ".html"),
                      output_dir = out_dir, params = params, quiet = TRUE)
  } else {
    rmarkdown::render(path, quiet = TRUE)
  }
}

#' Basic metadata for all flexdashboard templates in snapflex
#'
#' This function returns a data frame with basic meta data for all flexdashboard templates in \code{snapflex}.
#' This includes the following information:
#'
#' \itemize{
#'   \item the template ID used for generating a local flexdashboard.
#'   \item a brief description of the template.
#'   \item the variables included in the flexdashboard.
#'   \item whether any variables are displayed as deltas in comparison to a baseline rather than as raw values.
#'   \item intra- and/or inter-annual time period information if applicable.
#'   \item whether Shiny is used at runtime to launch a local instance of an interactive flexdashboard rather than a standalone html document.
#' }
#'
#' @return a data frame.
#' @export
#'
#' @seealso flex_templates flex_params
#' @examples
#' flex_templates()
flex_templates <- function(){
  tibble::data_frame(id = "psc1", description = "projected seasonal climate: single point location",
                     variable = "temperature and precipitation", deltas = TRUE, period = "seasonal, projected",
                     shiny = FALSE)
}

#' List the required parameters for a flexdashboard template
#'
#' List the required parameters for a flexdashboard template that must be passed to \code{flex}.
#'
#' If this function returns \code{NULL} for a given template, then no parameters are required to be passed in a named list when calling \code{flex}.
#'
#' @param template
#'
#' @return
#' @export
#'
#' @seealso flex_templates flex
#' @examples
#' flex_params("psc1")
flex_params <- function(template){
  switch(template, "psc1" = "location")
}
