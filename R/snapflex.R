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
#' Additional arguments passed to \code{flex_dashboard} typically include \code{theme}, \code{css} and \code{storyboard}.
#' Unless a given template supports it specifically, using \code{storyboard = TRUE} will not add any meaningful content.
#' Choice of \code{theme} is limited to those available in the \code{flexdashboard} package.
#' Additional CSS can be attached with \code{css}. Styling contained in a CSS file may be attached remotely via CDN (Content Delivery Network).
#'
#' @param template character, the ID of the flexdashboard template. See \code{\link{flex_templates}} for available IDs and descriptions.
#' @param out_dir character, output directory for standalone html document when \code{template} refers to a static (non-Shiny) flexdashboard.
#' @param file character, output file name.
#' @param template_params named list, additional parameters passed to a specific template if required. See \code{\link{flex_params}} for more information.
#' @param load_static logical, load static files automatically. See details.
#' @param ... additional arguments passed to \code{flex_dashboard}. See details.
#'
#' @export
#'
#' @seealso flex_templates flex_params
#' @examples
#' \dontrun{
#' flex_params("rsds1")
#' pars <- list(location = "Vancouver")
#' flex("rsds1", template_params = pars)
#' flex("rsds1", template_params = pars, theme = "sandstone", storyboard = TRUE)
#' pars$gfont <- "Source Sans Pro"
#' pars$snaptheme <- "theme_snap"
#' flex("rsds1", template_params = pars)
#' }
flex <- function(template, out_dir = getwd(), file = paste0(template, ".html"),
                 template_params = NULL, load_static = FALSE, ...){
  .check_template_id(template)
  path <- .flex_path(template)
  use_shiny <- dplyr::filter(flex_templates(), .data[["id"]] == template)$shiny
  params_required <- dplyr::filter(flex_templates(), .data[["id"]] == template)$params
  if(params_required){
    pars <- flex_params(template)
    required_params <- pars$parameter[is.na(pars$value)]
  }
  dots <- list(...)

  file_lines <- readLines(path) # used for orientation, storyboard, css
  idx <- which(substr(file_lines, 1, 8) == "    css:")
  template_css <- gsub("\"", "", substring(file_lines[idx], 10))
  if(is.null(dots$orientation)){
    idx <- which(substr(file_lines, 1, 16) == "    orientation:")
    dots$orientation <- utils::tail(strsplit(file_lines[idx], " ")[[1]], 1)
  }
  storyboard <- ifelse(is.null(dots$storyboard), FALSE, dots$storyboard)
  if(is.null(template_params)){
    template_params <- list(storyboard = storyboard)
  } else {
    template_params$storyboard <- storyboard
  }
  if(!is.null(dots$css) && any(substr(dots$css, 1, 4) == "http")){
    idx <- which(substr(dots$css, 1, 4) == "http")
    for(i in idx){
      tmp <- file.path(tempdir(), paste0("remote_styles", i, ".css"))
      utils::download.file(dots$css[i], tmp, quiet = TRUE)
      dots$css[idx] <- tmp
    }
  }
  if(storyboard){
    if(template_css != "null") template_css <- file.path(dirname(path), template_css)
    tmp <- file.path(tempdir(), basename(path))
    rfile <- gsub(".Rmd", ".R", basename(path))
    file.copy(path, tmp, overwrite = TRUE)
    idx1 <- which(substr(file_lines, 1, 18) == "knitr::read_chunk(")
    idx2 <- which(substr(file_lines, 1, 3) == "Row" | substr(file_lines, 1, 6) == "Column")
    if(length(idx1))
      file_lines[idx1] <- paste0("knitr::read_chunk(\"",
                                 file.path(system.file("flex", package = "snapflex"), rfile), "\")")
    if(length(idx2)) file_lines[c(idx2, idx2 + 1)] <- ""
    writeLines(file_lines, tmp)
    path <- tmp
  }
  if(template_css != "null") dots$css <- c(template_css, dots$css)
  default <- .template_defaults(template)
  for(i in names(default)) if(is.null(template_params[[i]])) template_params[[i]] <- default[[i]]

  cat("Genrating flexdashboard...\n")
  if(params_required){
    missing_params <- "Additional parameters required. See `flex_params`."
    if(!is.list(template_params) || any(!required_params %in% names(template_params)))
      stop(missing_params)
    suppressWarnings(suppressMessages(
      if(use_shiny){
        rmarkdown::run(path, render_args = template_params)
      } else {
        rmarkdown::render(path, do.call(flexdashboard::flex_dashboard, dots),
                          output_file = file, output_dir = out_dir,
                          params = template_params, quiet = TRUE)
      }
    ))
  } else {
    suppressWarnings(suppressMessages(
      if(use_shiny){
        rmarkdown::run(path)
      } else {
        rmarkdown::render(path, do.call(flexdashboard::flex_dashboard, dots),
                          output_file = file, output_dir = out_dir, quiet = TRUE)
      }
    ))
  }
  if(storyboard) unlink(tmp)
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
#' Regarding required template-specific parameters, note that all templates in \code{snapflex} accept some parameters that are optional
#' rather than required because the templates have set defaults for these if ignored. For example, all templates accept \code{gfont} , \code{regular}, \code{bold} and \code{snaptheme}.
#' Since optional parameters are always available and are safely ignored, entries in the final column of the data frame are \code{TRUE} only
#' if a template has required parameters where the user must always specify the value when rendering a template.
#' Use \code{\link{flex_params}} to view a table of information about all parameters available for a given template.
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

#' Template parameters
#'
#' List the required and optional parameters for a flexdashboard template passed to \code{flex}.
#'
#' This function returns a data frame with three columns. The first contains parameter names pertaining to a specific template.
#' The second is a column of default values. If \code{NA}, the parameter is required to be passed by the user any time the template is rendered using \code{flex}.
#' Otherwise, the parameter is optional, given that a default value is set by the template if the user ignores the parameter.
#' The third is a column of hints, containing information regarding what constitutes valid values for the parameters in case you are less familiar with the SNAPverse and SNAP climate data sets.
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
  .check_template_id(template)
  default <- .template_defaults(template)
  pars <- names(default)
  hint <- c(
    gfont = "A valid Google Fonts name. See https://fonts.google.com/ and `sysfonts::font_families_google`.",
    regular = "Regular Google font weight.",
    bold = "Bold Google font weight.",
    snaptheme = "A ggplot theme from the snapplot package, e.g., 'theme_snap' or 'theme_snapdark'.",
    simplify = "Logical: Simplify select dashboard content."
  )[pars]

  x <- switch(template,
         "psc1" = list(pars = "location", hint = "See snaplocs::locs for valid point location names."),
         "rsds1" = list(pars = "location",
                        hint = "Nine valid locations: 'Anaktuvuk Pass', 'Anchorage', 'Cantwell', 'Chicken', 'Churchill', 'Fairbanks', 'Juneau', 'Saskatoon', 'Vancouver'.") # nolint
  )
  tibble::data_frame(parameter = c(x$pars, pars),
                     value = c(as.character(rep(NA, length(x[[1]]))), default),
                     hint = c(x$hint, hint))
}

.template_defaults <- function(id){
  fixed <- c("gfont", "regular", "bold", "snaptheme")
  x <- switch(id,
              "psc1" = c("Play", 400, 400, "theme_snap"),
              "rsds1" = c("Jura", 400, 400, "theme_snapdark", FALSE)
  )
  names(x) <- switch(id,
                     "psc1" = fixed,
                     "rsds1" = c(fixed, "simplify")
  )
  x
}

.check_template_id <- function(x){
  if(!x %in% flex_templates()$id) stop("Invalid `template` ID. See `flex_templates`.")
}
