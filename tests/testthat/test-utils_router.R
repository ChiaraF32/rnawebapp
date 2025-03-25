# tests/testthat/test-router.R

test_that("render_router_ui returns UI for valid routes", {
  ui_index <- render_router_ui("index")
  ui_upload <- render_router_ui("upload")

  expect_s3_class(ui_index, "shiny.tag.list")
  expect_s3_class(ui_upload, "shiny.tag.list")
})

test_that("render_router_ui returns fallback for unknown route", {
  ui_404 <- render_router_ui("not_a_real_page")

  expect_s3_class(ui_404, "shiny.tag")
  expect_match(as.character(ui_404), "404", ignore.case = TRUE)
})

test_that("ROUTES contains expected keys and values", {
  expect_true("UPLOAD" %in% names(ROUTES))
  expect_equal(ROUTES$UPLOAD, "upload")
})
