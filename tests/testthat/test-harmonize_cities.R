test_that("harmonize_cities gives the saved reference output", {
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

test_that("historical names with a note such as [A ] match without the note", {
  r <- data.frame(a = c("Wirziburg", "Gersbeke", "Bardorpe"), b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r, "a", "b", "DE")$a_harmonized, c("WUERZBURG", "GORSBACH", "BAHRDORF"))
})

test_that("city_lookup drops IPA transcriptions and keeps no bracket notes", {
  lk <- city_lookup(as.data.frame(cities[cities$Country.Code == "DE", ]))
  expect_false(any(grepl("[", lk$key, fixed = TRUE)))
  expect_false(any(grepl("LEIV", lk$key)))
})

test_that("harmonize_cities keeps all Latin, Greek and Cyrillic letters and drops other characters", {
  r <- data.frame(a = c("пфаффенгофен ан дер ільм", "München"), b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r, "a", "b", "DE")$a_harmonized, c("PFAFFENHOFEN AN DER ILM", "MUNICH"))
  r <- data.frame(a = "Esch°sur°Alzette", b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r, "a", "b", "LU")$a_harmonized, "ESCH SUR ALZETTE")
})

test_that("harmonize_cities names the new columns after city_a and city_b", {
  r <- data.frame(id = 1, city = "Esch-sur-Alzette", city_orbis = "Luxemburg", a = 1, stringsAsFactors = FALSE)
  x <- harmonize_cities(r, "city", "city_orbis", "LU")
  expect_equal(names(x), c("id", "city", "city_orbis", "a", "city_harmonized", "city_orbis_harmonized"))
  expect_equal(x$city_orbis_harmonized, "LUXEMBOURG")
})
