test_that("tibbles and data.tables give the same results as data frames", {
  skip_if_not_installed("tibble")
  as_dt <- function(x) data.table::as.data.table(x)
  as_tb <- function(x) tibble::as_tibble(x)

  h <- fixture("harmonize_cities")$LU
  ref <- harmonize_cities(h$input, "city_a", "city_b", "LU")
  expect_equal(harmonize_cities(as_tb(h$input), "city_a", "city_b", "LU"), ref)
  expect_equal(harmonize_cities(as_dt(h$input), "city_a", "city_b", "LU"), ref)

  n <- fixture("norma")
  ref <- norma(n$input, "name", "country", "key")
  expect_equal(norma(as_tb(n$input), "name", "country", "key"), ref)
  expect_equal(norma(as_dt(n$input), "name", "country", "key"), ref)

  m <- fixture("nmatch")
  ref <- nmatch(m$a, m$b, "n", "n")
  expect_equal(nmatch(as_tb(m$a), as_dt(m$b), "n", "n"), ref)

  cl <- fixture("clean")
  args <- list("id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b", "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  ref <- do.call(clean, c(list(cl$input), args))
  expect_equal(do.call(clean, c(list(as_dt(cl$input)), args)), ref)

  a <- fixture("detect_address")
  ref <- detect_address(a$input, "adr", "x", "PL")
  expect_equal(detect_address(as_tb(a$input), "adr", "x", "PL"), ref)
})
