context("snapflex")

test_that("templates are as expected", {
  expect_is(flex_templates(), "tbl_df")
  expect_equal(dim(flex_templates()), c(2, 7))
})

test_that("params are as expected", {
  expect_error(flex_params("x"), "Invalid `template` ID. See `flex_templates`.")
  expect_error(flex("x"), "Invalid `template` ID. See `flex_templates`.")

  x <- flex_params("psc1")
  expect_is(x, "tbl_df")
  expect_equal(x$parameter, c("location", "gfont", "regular", "bold", "snaptheme"))
  expect_equal(x$value[1], as.character(NA))
  expect_equal(x$hint[1], "See snaplocs::locs for valid point location names.")

  x <- flex_params("rsds1")
  expect_is(x, "tbl_df")
  expect_equal(x$parameter, c("location", "gfont", "regular", "bold", "snaptheme", "simplify"))
  expect_equal(x$value, c(NA, "Jura", 400, 400, "theme_snapdark", FALSE))
  h <- c("Nine valid locations: 'Anaktuvuk Pass', 'Anchorage', 'Cantwell', 'Chicken', 'Churchill', 'Fairbanks', 'Juneau', 'Saskatoon', 'Vancouver'.", # nolint
         "A valid Google Fonts name. See https://fonts.google.com/ and `sysfonts::font_families_google`.",
         "Regular Google font weight.",
         "Bold Google font weight.",
         "A ggplot theme from the snapplot package, e.g., 'theme_snap' or 'theme_snapdark'.",
         "Logical: Simplify select dashboard content.")
  expect_equal(x$hint, h)
})

test_that("flex returns as expected", {
  expect_error(flex("psc1"), "Additional parameters required. See `flex_params`.")

  testthat::skip_on_appveyor()
  expect_is(flex("psc1", template_params = list(location = "Fairbanks")), "NULL")
  pars <- list(location = "Fairbanks", gfont = "Orbitron", regular = 400, bold = 400, snaptheme = "theme_snapdark")
  expect_is(flex("psc1", template_params = pars), "NULL")
  css <- "https://bootswatch.com/4/slate/bootstrap.css"
  expect_is(flex("psc1", template_params = pars, theme = "sandstone", css = css, storyboard = TRUE), "NULL")
})

unlink("psc1.html")
