test_that("detect_address gives the saved reference output", {
  x <- fixture("detect_address")
  expect_equal(detect_address(x$input, "adr", "x", "PL"), x$output)
})

test_that("detect_address removes a historical name matched without its note", {
  r <- data.frame(id = 1, adr = "Domstrasse 5, 97070 Wirziburg", stringsAsFactors = FALSE)
  x <- detect_address(r, "adr", "x", "DE")
  expect_equal(x$x_city, "WUERZBURG")
  expect_false(grepl("WIRZIBURG", x$x_street))
})

test_that("detect_address keeps the words around the city apart", {
  r <- data.frame(adr = "Marienplatz 1 Muenchen Hinterhaus", stringsAsFactors = FALSE)
  x <- detect_address(r, "adr", "x", "DE")
  expect_equal(x$x_city, "MUNICH")
  expect_equal(x$x_street, "MARIENPLATZ 1 HINTERHAUS")
})

test_that("detect_address keeps your columns named city or miasto", {
  r <- data.frame(adr = "Marienplatz 1 Muenchen", city = "mine", miasto = "mine too", stringsAsFactors = FALSE)
  x <- detect_address(r, "adr", "x", "DE")
  expect_equal(x$city, "mine")
  expect_equal(x$miasto, "mine too")
})
