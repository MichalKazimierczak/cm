test_that("norma gives the 0.1.0 output", {
  x <- fixture("norma")
  expect_equal(norma(x$input, "name", "country", "key"), x$output)
})

test_that("norma_short gives the 0.1.0 output", {
  x <- fixture("norma_short")
  expect_equal(norma_short(x$input, "name"), x$output)
})

test_that("norma_legal gives the 0.1.0 output", {
  x <- fixture("norma_legal")
  expect_equal(norma_legal(x$input, "name", "country"), x$output)
})
