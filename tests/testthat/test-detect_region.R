test_that("detect_region gives the saved reference output", {
  x <- fixture("detect_region")
  for (cc in names(x)) expect_identical(detect_region(x[[cc]]$input, cc), x[[cc]]$output)
})

test_that("detect_region gives no region for a country without postal codes", {
  expect_identical(detect_region(c("8001", NA, "8002"), "CH"), c("", NA, ""))
})
