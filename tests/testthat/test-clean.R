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

test_that("clean uses Jaro by default and Jaro-Winkler with p", {
  x <- fixture("clean")
  args <- list(x$input, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b",
               "city_a", "city_b", "street_a", "street_b", "nuts", "cc")
  jaro <- do.call(clean, args)
  jw <- do.call(clean, c(args, p = 0.1))
  jaro_ref <- stringdist::stringsim(toupper(jaro$city_a), toupper(jaro$city_b), method = "jw")
  expect_equal(jaro$city_sim, ifelse(is.na(jaro_ref), 0, jaro_ref))
  expect_true(all(jw$name_sim >= jaro$name_sim))
  expect_gt(jw$city_sim[1], jaro$city_sim[1])
})
