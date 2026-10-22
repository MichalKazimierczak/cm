test_that("detect_address gives the 0.1.0 output", {
  x <- fixture("detect_address")
  expect_equal(detect_address(x$input, "adr", "x", "PL"), x$output)
})
