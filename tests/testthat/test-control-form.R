test_that("lookup help button specs only include enabled lookups", {
  config <- modifyList(
    default_app_config(tempdir()),
    list(
      lookup1Label = "Lookup One",
      lookup2Label = "Lookup Two",
      lookup3Label = "Lookup Three",
      lookup4Label = "Lookup Four",
      lookup2Enabled = TRUE,
      lookup3Enabled = FALSE,
      lookup4Enabled = FALSE
    )
  )

  specs <- lookup_help_button_specs(config = config)

  expect_equal(vapply(specs, `[[`, integer(1), "index"), c(1L, 2L))
  expect_equal(vapply(specs, `[[`, character(1), "input_id"), c("lookup1_help", "lookup2_help"))
})


test_that("lookup help buttons ui only renders enabled lookup buttons", {
  config <- modifyList(
    default_app_config(tempdir()),
    list(
      lookup1Label = "Lookup One",
      lookup2Label = "Lookup Two",
      lookup3Label = "Lookup Three",
      lookup4Label = "Lookup Four",
      lookup2Enabled = TRUE,
      lookup3Enabled = FALSE,
      lookup4Enabled = FALSE
    )
  )

  html <- paste(
    htmltools::renderTags(
      build_lookup_help_buttons_ui(
        ns = function(id) paste0("control_form-", id),
        config = config
      )
    )$html,
    collapse = ""
  )

  expect_match(html, "Help Files:")
  expect_match(html, "control_form-lookup1_help")
  expect_match(html, "control_form-lookup2_help")
  expect_match(html, "./app_data/help1.pdf", fixed = TRUE)
  expect_match(html, "./app_data/help2.pdf", fixed = TRUE)
  expect_false(grepl("lookup3_help", html, fixed = TRUE))
  expect_false(grepl("lookup4_help", html, fixed = TRUE))
})
