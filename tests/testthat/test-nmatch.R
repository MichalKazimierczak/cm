test_that("nmatch gives the 0.1.0 output", {
  x <- fixture("nmatch")
  expect_equal(nmatch(x$a, x$b, "n", "n"), x$sorted)
  expect_equal(nmatch(x$a, x$b, "n", "n", sort = FALSE, unique = FALSE), x$unsorted)
})
