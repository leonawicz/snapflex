# snapflex 0.2.0 (Release date: 2018-01-16)

* Added support to `flex` for passing along `flex_dashboard` arguments.
* Made passing `css` to `...` work with remote CSS files from a CDN. If a template has its own CSS, this will be loaded prior to any user-specified CSS.
* Improved handling of `flex_dashboard` arguments and `template_params` arguments together, specifically `orientation`, `theme`, `storyboard` and `css`.
* Added `gfont` optional template argument for choice of font name from Google Fonts. Used in `template_params` list.
* Added choice of primary SNAP plot theme via `snaptheme` argument. Used in `template_params` list.
* Allowed for template-specific parameters that have default settings so that these do not have to be supplied by the user when rendering a template.
* Ensured that templates use color palettes that switch during rendering conditional on choice of light or dark SNAP plot theme.
* Refactored `flex_params`, now returns a data frame with required and optional parameters.
* Updated existing templates.
* Updated and added new sections to vignette.

# snapflex 0.1.2

* Added template for example projected RSDS data at several point locations.
* Refactored `flex` to handle logical storyboard input parameter.
* Added `file` argument to `flex` to override default output filename.

# snapflex 0.1.1

* Added vignette content.
* Updated other documentation.
* Added unit tests.
* Minor tweaks to template.
* Changed `flex_params` to return a list containing required paramters plus information regarding valid paramter values for those unfamiliar with SNAP data sets.
* Minor function updates.

# snapflex 0.1.0

* Added wrapper functions for looking up and using flexdashboard templates from the `snapflex` package.
* Added initial template.
* Build out code for handling static vs. Shiny runtime flexdashboards and automatic launch for the former type.

# snapflex 0.0.0.9000

* Set up packaged scaffolding.
