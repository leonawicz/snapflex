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
#' By contrast, the static document templates save the resulting standalone html document to \code{out_dir/<file>}, defaulting to the working directory.
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
#' @param file character, output file name.
#' @param template_params named list, additional parameters passed to the template if required. See \code{\link{flex_params}} for more information.
#' @param load_static logical, load static files automatically. See details.
#'
#' @export
#'
#' @seealso flex_templates flex_params
#' @examples
#' \dontrun{flex("psc1", template_params = list(location = "Fairbanks"))}
flex <- function(template, out_dir = getwd(), file = paste0(template, ".html"),
                 template_params = NULL, load_static = FALSE){
  path <- .flex_path(template)
  use_shiny <- dplyr::filter(flex_templates(), .data[["id"]] == template)$shiny
  params_required <- dplyr::filter(flex_templates(), .data[["id"]] == template)$params
  if(params_required) required_params <- strsplit(flex_params(template)$parameters, ", ")[[1]]
  cat("Genrating flexdashboard...\n")
  if(params_required){
    missing_params <- "Additional parameters required. See `flex_params`."
    if(!is.list(template_params) || any(!required_params %in% names(template_params)))
      stop(missing_params)
    storyboard <- "storyboard" %in% required_params && template_params$storyboard
    if(storyboard){
      tmp <- file.path(tempdir(), basename(path))
      rfile <- gsub(".Rmd", ".R", basename(path))
      file.copy(path, tmp, overwrite = TRUE)
      x <- readLines(path)
      idx <- which(substr(x, 5, 17) == "orientation: ")
      idx2 <- which(substr(x, 1, 18) == "knitr::read_chunk(")
      idx3 <- which(substr(x, 1, 3) == "Row" | substr(x, 1, 6) == "Column")
      x[idx] <- "    storyboard: true"
      if(length(idx2))
        x[idx2] <- paste0("knitr::read_chunk(\"", file.path(system.file("flex", package = "snapflex"), rfile), "\")")
      if(length(idx3))
        x[c(idx3, idx3 + 1)] <- ""
      writeLines(x, tmp)
      path <- tmp
    }
    suppressWarnings(suppressMessages(
      if(use_shiny){
        rmarkdown::run(path, render_args = template_params)
      } else {
        rmarkdown::render(path, output_file = file, output_dir = out_dir,
                          params = template_params, quiet = TRUE)
      }
    ))
    if(storyboard) unlink(tmp)
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

.flex_path <- function(template){
  file <- switch(template,
                 "psc1" = "flex-clim-1.Rmd",
                 "rsds1" = "flex-rsds-1.Rmd"
                 )
  file.path(system.file("flex", package = "snapflex"), file)
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
  tibble::data_frame(id = .flex_id, description = .flex_desc, variable = .flex_vars, deltas = .flex_deltas,
                     period = .flex_period, shiny = .flex_shiny, params = .flex_params)
}

.flex_id <- c("psc1", "rsds1")
.flex_desc <- c("projected seasonal climate: single point location", "projected RSDS examples: single point location")
.flex_vars <- c("temperature and precipitation", "rsds")
.flex_deltas <- c(TRUE, FALSE)
.flex_period <- c("seasonal, projected", "annual, projected")
.flex_shiny <- c(FALSE, FALSE)
.flex_params <- c(TRUE, TRUE)

#' Required template parameters
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
  switch(template,
         "psc1" = list(parameters = "location", hint = "See snaplocs::locs for valid point location names."),
         "rsds1" = list(parameters = "location, storyboard", hint = c(
           "Nine valid locations: 'Anaktuvuk Pass', 'Anchorage', 'Cantwell', 'Chicken', 'Churchill', 'Fairbanks', 'Juneau', 'Saskatoon', 'Vancouver'.", # nolint
           "'storyboard': logical."))
         )
}
