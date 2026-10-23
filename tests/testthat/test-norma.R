test_that("norma gives the saved reference output", {
  x <- fixture("norma")
  expect_equal(norma(x$input, "name", "country", "key"), x$output)
})

test_that("norma_short gives the saved reference output", {
  x <- fixture("norma_short")
  expect_equal(norma_short(x$input, "name"), x$output)
})

test_that("norma_legal gives the saved reference output", {
  x <- fixture("norma_legal")
  expect_equal(norma_legal(x$input, "name", "country"), x$output)
})

test_that("norma_legal keeps rows of countries without a list of legal forms", {
  d <- data.frame(key = 1:4, name = c("ABC GMBH", "XYZ SARL", "ACME INC", "FOO LTD"),
                  country = c("DE", "FR", "US", NA), stringsAsFactors = FALSE)
  x <- norma_legal(d, "name", "country")
  expect_equal(sort(x$key), 1:4)
  expect_equal(x$lf[x$key == 3], "")
  expect_equal(x$name[x$key == 3], "ACME INC")
  expect_equal(x$name[x$key == 4], "FOO LTD")
})

test_that("norma_legal removes every weak word of a rule, not only the first", {
  d <- data.frame(key = 1:3, name = c("DIE DELTA HOLDING GMBH", "DELTA GROUP HOLDING GMBH", "MUELLER BAECKEREI GMBH"),
                  country = "DE", stringsAsFactors = FALSE)
  expect_equal(norma_legal(d, "name", "country")$name, c("DELTA", "DELTA", "MULLER BACKEREI"))
})

test_that("norma transliterates only names written in Greek or Cyrillic", {
  d <- data.frame(key = 1:2, name = c("Maya Khristov Trading OOD", "Мая Христова ООД"),
                  country = "BG", stringsAsFactors = FALSE)
  x <- norma(d, "name", "country", "key")
  expect_equal(x$norm_name[x$key == 1], "MAYA KHRISTOV TRADING")
  expect_equal(x$norm_name[x$key == 2], "MAA HRISTOVA")
  expect_equal(x$lf, c("OOD", "OOD"))
})

test_that("Greek weak words are removed only as whole words", {
  d <- data.frame(key = 1:4, name = c("ATHINAIKI ZYTHOPOIIA AE", "ANELLADA AE", "GREEKFOOD AE", "ATHINA TRADE AE"),
                  country = "GR", stringsAsFactors = FALSE)
  expect_equal(norma_legal(d, "name", "country")$name, c("ZYTHOPOIIA", "ANELLADA", "GREEKFOOD", "TRADE"))
})

test_that("the umlaut rule applies in DE, AT, CH and LI but not in BG", {
  d <- data.frame(key = 1:5, name = c("MUELLER BAECKEREI GMBH", "MUELLER BAECKEREI GMBH", "MUELLER BAECKEREI AG",
                                      "MUELLER BAECKEREI GMBH", "BOEV EOOD"),
                  country = c("DE", "AT", "CH", "LI", "BG"), stringsAsFactors = FALSE)
  x <- norma_legal(d, "name", "country")
  expect_equal(x$name[order(x$key)], c(rep("MULLER BACKEREI", 4), "BOEV"))
})
