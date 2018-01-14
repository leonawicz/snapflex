context("snapflex")

test_that("templates are as expected", {
  expect_is(flex_templates(), "tbl_df")
  expect_equal(dim(flex_templates()), c(2, 7))
})

test_that("params are as expected", {
  expect_is(flex_params("x"), "NULL")
  expect_is(flex_params("psc1"), "list")
  expect_equal(flex_params("psc1")[[1]], "location")
  expect_is(flex_params("psc1")[[2]], "character")
})

test_that("flex returns as expected", {
  expect_error(flex("psc1"), "Additional parameters required. See `flex_params`.")

  testthat::skip_on_appveyor()
  expect_is(flex("psc1", template_params = list(location = "Fairbanks")), "NULL")
})

unlink("psc1.html")
