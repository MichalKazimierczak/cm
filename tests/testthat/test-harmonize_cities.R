test_that("harmonize_cities gives the 0.1.0 output", {
  x <- fixture("harmonize_cities")
  for (cc in names(x)) {
    expect_equal(harmonize_cities(x[[cc]]$input, "city_a", "city_b", cc), x[[cc]]$new_col)
    expect_equal(harmonize_cities(x[[cc]]$input, "city_a", "city_b", cc, new_col = FALSE), x[[cc]]$replace)
  }
})

test_that("harmonize_cities prefers official names, then longer names", {
  r <- data.frame(a = c("Muenchen", "rue de Paris, 75001 paris", "xyz"), b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r[1, ], "a", "b", "DE")$a_harmonized, "MUNICH")
  expect_equal(harmonize_cities(r[2:3, ], "a", "b", "FR")$a_harmonized, c("PARIS", "XYZ"))
})
