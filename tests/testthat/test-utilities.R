library(shiny)

test_that("dose model helpers reflect loaded metadata", {
  metadata <- load_dose_response_metadata()

  old_opts <- options(qraLm.dose_response_metadata = metadata)
  on.exit(options(old_opts), add = TRUE)

  expect_setequal(get_dose_models(), metadata$models)

  model <- metadata$models[[1]]
  choices <- get_population_choices(model)

  expect_true(length(choices) > 0)
  populations <- metadata$populations_by_model[[model]]$Population
  core_choices <- choices

  margin_label <- metadata$margin_labels[[model]]
  if (!is.null(margin_label) && length(core_choices) > 0 && names(core_choices)[1] == margin_label) {
    core_choices <- core_choices[-1]
  }

  expect_true(all(core_choices %in% populations))

  if (!is.null(margin_label)) {
    expect_identical(unname(choices[[1]]), 0)
    expect_identical(names(choices)[1], margin_label)
  }
})

test_that("input helper falls back to defaults and help link is built", {
  input <- list("demo_value" = 12)
  expect_equal(get_input_value(input, "demo_", "value", default = 99), 12)
  expect_equal(get_input_value(input, "missing_", "value", default = 5), 5)

  old_opts <- options(myRepos = "https://example.org/repo")
  on.exit(options(old_opts), add = TRUE)

  help_tag <- makeHelp("Example", "Lot2LotGen")
  help_html <- as.character(help_tag)

  expect_true(grepl("Example", help_html, fixed = TRUE))
  expect_true(
    grepl("https://example.org/repo/qraLm/reference/Lot2LotGen.html", help_html, fixed = TRUE)
  )
})
