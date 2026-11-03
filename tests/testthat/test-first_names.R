toy_first_names <- function() {
  fnames <- c(rep("Jan", 40), rep("Jan Pieter", 10), rep("Maria", 50), rep("Mark", 30), rep("Rose", 20), "Gmbh",
              rep("Andrea", 20), rep("Andrea", 6))
  fcc <- c(rep("NL", 50), rep("IT", 50), rep("DE", 30), rep("GB", 20), "DE", rep("IT", 20), rep("DE", 6))
  orgs <- c(rep("Mark Media GmbH", 30), rep("Rose Garden Ltd", 25), rep("Alpha Beta Srl", 50))
  occ <- c(rep("DE", 30), rep("GB", 25), rep("IT", 50))
  build_first_names(fnames, fcc, orgs, occ, min_count = 5)
}

test_that("build_first_names counts first names per country and over all countries", {
  fn <- toy_first_names()
  all <- fn[fn$country == "*", ]
  expect_equal(all$n_person[all$word == "JAN"], 50)
  expect_equal(all$n_person[all$word == "PIETER"], 10)
  expect_equal(fn$n_person[fn$word == "ANDREA" & fn$country == "IT"], 20)
  expect_equal(fn$n_person[fn$word == "ANDREA" & fn$country == "DE"], 6)
  expect_false("GMBH" %in% fn$word)
  expect_equal(all$person_share[all$word == "JAN"], 1)
  expect_lt(all$person_share[all$word == "MARK"], 0.5)
  expect_lt(all$person_share[all$word == "ROSE"], 0.5)
  no_org <- build_first_names(c("Jan", "Jan", "Mark"), min_count = 1)
  expect_true(all(no_org$person_share == 1))
  expect_true(all(no_org$country == "*"))
})

test_that("guess_person reads 'SURNAME, FIRSTNAME', first and last words, and company words", {
  fn <- toy_first_names()
  g <- guess_person(c("DE VRIES, JAN", "DE VRIES, JAN PIETER", "Jan de Vries", "Rossi Maria", "Mark Media",
                      "Alpha Beta", "Rossi Maria Srl", "Rose Smith", NA),
                    country = c("NL", "NL", "NL", "IT", "DE", "IT", "IT", "GB", NA), first_names = fn)
  expect_equal(g$type, c("person", "person", "person", "person", "organisation", NA, "organisation", NA, NA))
  expect_equal(g$first_name[1:4], c("JAN", "JAN", "JAN", "MARIA"))
})

test_that("guess_person uses the name's country when it has enough data, otherwise all countries", {
  fn <- build_first_names(c(rep("Andrea", 60), rep("Andrea", 6), rep("Hans", 30)), country = c(rep("IT", 60), rep("DE", 6), rep("DE", 30)),
                          org_names = c(rep("Andrea Bau GmbH", 3), rep("Alpha Srl", 50), rep("Mueller Bau GmbH", 30)),
                          org_country = c(rep("DE", 3), rep("IT", 50), rep("DE", 30)), min_count = 5)
  expect_lt(fn$person_share[fn$word == "ANDREA" & fn$country == "DE"], 0.9)
  expect_gt(fn$person_share[fn$word == "ANDREA" & fn$country == "*"], 0.9)
  g <- guess_person(c("ROSSI, ANDREA", "MUELLER, ANDREA", "DUPONT, ANDREA"), country = c("IT", "DE", "FR"), first_names = fn)
  expect_equal(g$type, c("person", NA, "person"))
})

test_that("match_quality takes the type from one side, with first names where the field is empty", {
  x <- quality_example()
  fn <- build_first_names(c(rep("Jan", 10), rep("Maria", 10)), country = c(rep("NL", 10), rep("IT", 10)), min_count = 5)
  q <- match_quality(x$rk, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b",
                     city_a = "city_a", city_b = "city_b", norm_a = x$na, norm_b = x$nb,
                     norm_key_a = "id_a", norm_key_b = "id_b", type_side = "b", first_names = fn)
  p <- pick(q, 2, 12)
  expect_equal(p$q_type_b, "person")
  expect_equal(p$q_type_source_b, "first name")
  expect_equal(p$q_type_pair, "person")
  expect_match(p$q_reasons, "^person \\(type from first name\\)")
  expect_true(all(q$q_type_pair %in% c("person", "organisation")))
  s <- quality_summary(q)
  expect_false(any(grepl("^person( |$)|^organisation( |$)", s$reasons$reason)))
  expect_true("person-like name" %in% s$reasons$reason)
})

test_that("one country code applies to all names", {
  fn <- build_first_names(c("Jan", "Jan", "Piet"), country = "NL", org_names = c("Piet Bouw BV", "Alpha BV"), org_country = "NL", min_count = 1)
  expect_equal(fn$n_person[fn$word == "JAN" & fn$country == "NL"], 2)
  expect_equal(guess_person(c("DE VRIES, JAN", "SMIT, JAN"), country = "NL", first_names = fn, min_count = 1)$type, c("person", "person"))
})
