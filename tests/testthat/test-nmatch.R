test_that("nmatch gives the saved reference output", {
  x <- fixture("nmatch")
  expect_equal(nmatch(x$a, x$b, "n", "n"), x$sorted)
  expect_equal(nmatch(x$a, x$b, "n", "n", sort = FALSE, unique = FALSE), x$unsorted)
})

test_that("nmatch leaves out missing and empty names", {
  a <- data.frame(id_a = 1:3, n = c(NA, "", "NOWAK"), stringsAsFactors = FALSE)
  b <- data.frame(id_b = 11:13, n = c("", NA, "NOWAK"), stringsAsFactors = FALSE)
  expect_equal(nmatch(a, b, "n", "n")$id_a, 3)
  expect_equal(nmatch(a, b, "n", "n", sort = FALSE, unique = FALSE)$id_a, 3)
})
