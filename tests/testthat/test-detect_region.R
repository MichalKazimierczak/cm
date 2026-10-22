test_that("detect_region gives the 0.1.0 output", {
  x <- fixture("detect_region")
  for (cc in names(x)) expect_identical(detect_region(x[[cc]]$input, cc), x[[cc]]$output)
})

test_that("detect_region stops for a country without postal codes", {
  expect_error(detect_region(c("8001", "8002"), "CH"))
})
