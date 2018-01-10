globalVariables(".data")

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
#' these arguments can be passed as a named list to \code{template_params}.
#'
#' Some templates produce static documents. Others use Shiny at runtime.
#' Flexdashboards with Shiny backend support automatically launch a local instance in the default web browser.
#' By contrast, the static document templates save the resulting standalone html document to \code{out_dir}, defaulting to the working directory.
#' In this case, \code{load_static = TRUE} will toggle on the Shiny-like automatic launch for static flexdashboards. This is off by default.
#'
#' Note that static does not mean the flexdashboard html document offers no interactivity.
#' A template may still offer the ability to switch between different graphs and tables and explore different content.
#' However, all data is embedded in the document. This makes it useful as a standalone file requiring no Shiny server backend,
#' but it also means that more complex static templates result in larger output file sizes and they cannot achieve anywhere near the
#' complexity of a flexdashboard that uses Shiny.
#'
#' @param template character, the ID of the flexdashboard template. See \code{\link{flex_templates}} for available IDs and descriptions.
#' @param out_dir character, output directory for standalone html document when \code{template} refers to a static (non-Shiny) flexdashboard.
#' @param template_params named list, additional parameters passed to the template if required. See \code{\link{flex_params}} for more information.
#' @param load_static logical, load static files automatically. See details.
#'
#' @export
#'
#' @seealso flex_templates flex_params
#' @examples
#' \dontrun{flex("psc1", template_params = list(location = "Fairbanks"))}
flex <- function(template, out_dir = getwd(), template_params = NULL, load_static = FALSE){
  path <- system.file("flex", package = "snapflex")
  path <- switch(template, "psc1" = file.path(path, "flex-clim-1.Rmd"))
  use_shiny <- dplyr::filter(flex_templates(), .data[["id"]] == template)$shiny
  if(!use_shiny) file <- paste0(template, ".html")
  params_required <- dplyr::filter(flex_templates(), .data[["id"]] == template)$params
  if(params_required) required_params <- strsplit(flex_params(template)$parameters, ",")[[1]]
  cat("Genrating flexdashboard...\n")
  if(params_required){
    missing_params <- "Additional parameters required. See `flex_params`."
    if(!is.list(template_params) || any(!required_params %in% names(template_params)))
      stop(missing_params)
    suppressWarnings(suppressMessages(
      if(use_shiny){
        rmarkdown::run(path, render_args = template_params)
      } else {
        rmarkdown::render(path, output_file = file, output_dir = out_dir,
                          params = template_params, quiet = TRUE)
      }
    ))
  } else {
    suppressWarnings(suppressMessages(
      if(use_shiny){
        rmarkdown::run(path)
      } else {
        rmarkdown::render(path, output_file = file, output_dir = out_dir, quiet = TRUE)
      }
    ))
  }
  cat("Dashboard complete.\n")
  if(!use_shiny && load_static) utils::browseURL(paste0("file://", file.path(out_dir, file)))
  invisible()
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
#'   \item whether the template requires any additional parameters specified by the user.
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
                     shiny = FALSE, params = TRUE)
}

#' List the required parameters for a flexdashboard template
#'
#' List the required parameters for a flexdashboard template that must be passed to \code{flex}.
#'
#' If this function returns \code{NULL} for a given template, then no parameters are required to be passed in a named list when calling \code{flex}.
#' Otherwise a length-2 list is returned. The first list element contains the vector of required parameters.
#' The second element contains information regarding what constitutes valid values for the parameters if you are less familiar with the SNAPverse and SNAP climate data sets.
#'
#' @param template character, the flexdashboard template. See \code{\link{flex_templates}}.
#'
#' @return a list or \code{NULL}.
#' @export
#'
#' @seealso flex_templates flex
#' @examples
#' flex_params("psc1")
flex_params <- function(template){
  switch(template, "psc1" = list(parameters = "location", hint = "See snaplocs::locs for valid point location names."))
}
