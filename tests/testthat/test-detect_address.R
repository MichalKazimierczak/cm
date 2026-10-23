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

test_that("detect_address chooses the place after the postal code, or not after a street word", {
  pl <- detect_address(data.frame(adr = c("ul. Lesna 50, 31-123 Ploty", "ul. Lesna 50 Ploty")), "adr", "x", "PL")
  expect_equal(pl$x_city, c("PLOTY", "PLOTY"))
  expect_equal(pl$x_street, c("UL LESNA 50", "UL LESNA 50"))
  de <- detect_address(data.frame(adr = "Marienplatz 1, 80331 Muenchen Altstadt"), "adr", "x", "DE")
  expect_equal(de$x_city, "MUNICH")
})

test_that("detect_address keeps the word before the postal code and finds CEDEX in any case", {
  gb <- detect_address(data.frame(adr = "10 Downing Street, London SW1A 2AA"), "adr", "x", "GB")
  expect_equal(c(gb$x_zip, gb$x_city, gb$x_street), c("SW1A 2AA", "LONDON", "10 DOWNING STREET"))
  de <- detect_address(data.frame(adr = c("Unter den Linden 5, Berlin 10117", "Unter den Linden 5, D-10117 Berlin")), "adr", "x", "DE")
  expect_equal(de$x_city, c("BERLIN", "BERLIN"))
  expect_equal(de$x_zip, c("10117", "10117"))
  fr <- detect_address(data.frame(adr = "2 rue de Rivoli 75001 Paris Cedex 01"), "adr", "x", "FR")
  expect_equal(c(fr$x_city, fr$x_street), c("PARIS", "2 RUE DE RIVOLI"))
})
