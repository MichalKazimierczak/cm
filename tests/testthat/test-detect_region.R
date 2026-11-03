test_that("detect_region gives the saved reference output", {
  x <- fixture("detect_region")
  for (cc in names(x)) expect_identical(detect_region(x[[cc]]$input, cc), x[[cc]]$output)
})

test_that("detect_region gives no region for a country without postal codes", {
  expect_identical(detect_region(c("8001", NA, "8002"), "CH"), c("", NA, ""))
})

test_that("detect_region gives each region once per value", {
  expect_identical(detect_region(c("80331 80333", "80331 80331"), "DE"), c("DE212", "DE212"))
})

test_that("detect_region finds Dutch codes written without the space or in lower case", {
  expect_identical(detect_region(c("1012 AB", "1012AB", "1012ab", "NL-1012AB", "1012ABC"), "NL"),
                   c("NL32B", "NL32B", "NL32B", "NL32B", ""))
})
