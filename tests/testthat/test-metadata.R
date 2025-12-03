test_that("dose-response metadata loads and matches qraLm definitions", {
  expect_true(exists("load_dose_response_metadata"))
  metadata <- load_dose_response_metadata()

  expect_setequal(
    names(metadata),
    c("dr_param", "models", "populations_by_model", "margin_labels")
  )
  expect_s3_class(metadata$dr_param, "data.frame")
  expect_true(length(metadata$models) > 0)
  expect_setequal(sort(unique(metadata$dr_param$Model)), metadata$models)
  expect_true(all(metadata$models %in% names(metadata$populations_by_model)))
})
