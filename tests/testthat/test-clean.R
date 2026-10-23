test_that("clean gives the saved reference output", {
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

test_that("clean falls back to orb_reg for a country without postal codes", {
  m <- data.frame(id_a = 1, id_b = 2, name_a = "ALPHA AG", name_b = "ALPHA AG", lf_a = "AG", lf_b = "AG",
                  zip_a = "8001", zip_b = "8001", city_a = "ZURICH", city_b = "ZURICH", street_a = "X", street_b = "X",
                  nuts = "CH040", cc = "CH", stringsAsFactors = FALSE)
  out <- clean(m, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
               "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  expect_equal(out$reg_a, "CH040")
  expect_true(out$reg_sim)
})
