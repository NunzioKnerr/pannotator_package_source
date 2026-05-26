test_that("leaflet map module can initialize with session app state", {
  app_state <- create_app_state(build_runtime_context(initialize = TRUE))

  testServer(mod_leaflet_map_server, args = list(r = app_state), {
    expect_true(TRUE)
  })
})
