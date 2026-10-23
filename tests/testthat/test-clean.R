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

test_that("clean looks postal codes up in each row's own country", {
  m <- data.frame(id_a = 1:2, id_b = 3:4, name_a = c("ALPHA GMBH", "BETA SP Z O O"), name_b = c("ALPHA GMBH", "BETA SP Z O O"),
                  lf_a = c("GMBH", "SPZOO"), lf_b = c("GMBH", "SPZOO"), zip_a = c("80331", "00-002"), zip_b = c("80331", "00-002"),
                  city_a = c("MUNICH", "WARSAW"), city_b = c("MUNICH", "WARSAW"), street_a = "X", street_b = "X",
                  nuts = NA, cc = c("DE", "PL"), stringsAsFactors = FALSE)
  out <- clean(m, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
               "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  expect_equal(out$reg_a[order(out$id_a)], c("DE212", "PL911"))
  expect_true(all(out$reg_sim))
})

test_that("clean works when the key columns are named key_a and key_b", {
  x <- fixture("clean")
  m <- x$input
  names(m)[names(m) == "id_a"] <- "key_a"
  names(m)[names(m) == "id_b"] <- "key_b"
  out <- clean(m, "key_a", "key_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
               "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  expect_equal(out$one_to_one, x$output$one_to_one)
  expect_equal(out$key_b, x$output$id_b)
})
