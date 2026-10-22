test_that("detect_address gives the 0.1.0 output", {
  x <- fixture("detect_address")
  expect_equal(detect_address(x$input, "adr", "x", "PL"), x$output)
})

test_that("detect_address removes a historical name matched without its note", {
  r <- data.frame(id = 1, adr = "Domstrasse 5, 97070 Wirziburg", stringsAsFactors = FALSE)
  x <- detect_address(r, "adr", "x", "DE")
  expect_equal(x$x_city, "WUERZBURG")
  expect_false(grepl("WIRZIBURG", x$x_street))
})
