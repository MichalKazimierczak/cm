test_that("clean gives the 0.1.0 output", {
  x <- fixture("clean")
  expect_equal(clean(x$input, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
                     "city_a", "city_b", "street_a", "street_b", "nuts", "cc"), x$output)
})

test_that("clean uses orb_reg when the postal code of A is missing", {
  x <- fixture("clean")
  out <- clean(x$input, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
               "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  row <- out[out$id_a == 3, ]
  expect_equal(row$reg_a, "DE300")
  expect_true(row$reg_sim)
})
