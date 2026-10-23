test_that("Eurostat codes EL and UK and lower case work like the ISO codes", {
  r <- data.frame(a = c("Athina", "Thessaloniki"), b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r, "a", "b", "EL"), harmonize_cities(r, "a", "b", "GR"))
  r <- data.frame(a = "Manchester", b = "", stringsAsFactors = FALSE)
  expect_equal(harmonize_cities(r, "a", "b", "UK")$a_harmonized, "MANCHESTER")
  expect_equal(harmonize_cities(data.frame(a = "Muenchen", b = ""), "a", "b", "de")$a_harmonized, "MUNICH")
  expect_equal(detect_region("105 63", "EL"), detect_region("105 63", "GR"))
  a <- data.frame(adr = "10 Downing Street, London SW1A 2AA")
  expect_equal(detect_address(a, "adr", "x", "UK")$x_city, "LONDON")
})

test_that("norma_legal uses the GR and GB lists for EL and UK and keeps the country column", {
  d <- data.frame(key = 1:2, name = c("ALPHA AE", "BETA LIMITED"), country = c("EL", "UK"), stringsAsFactors = FALSE)
  x <- norma_legal(d, "name", "country")
  e <- norma_legal(transform(d, country = c("GR", "GB")), "name", "country")
  x <- x[order(x$key), ]
  e <- e[order(e$key), ]
  expect_equal(x$name, e$name)
  expect_equal(x$lf, e$lf)
  expect_equal(x$country, c("EL", "UK"))
})
