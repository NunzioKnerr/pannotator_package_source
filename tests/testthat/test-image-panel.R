test_that("image ready notice clears once an image is selected", {
  ready_notice <- list(
    title = "Images Ready",
    message = "Choose an image here or click one on the map, then switch modes as needed to start annotating.",
    type = "info"
  )

  expect_null(clear_image_ready_notice(ready_notice, "IMG_001.JPG"))
  expect_equal(clear_image_ready_notice(ready_notice, ""), ready_notice)
  expect_equal(
    clear_image_ready_notice(
      list(title = "Export Successful", message = "done", type = "success"),
      "IMG_001.JPG"
    )$title,
    "Export Successful"
  )
})
