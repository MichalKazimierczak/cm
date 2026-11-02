## a small matching run: a sure match, an ambiguous one, a legal-form conflict, a person with the surname first,
## and a sole trader ('h.o.d.n.') matched as person and as firm
quality_example <- function() {
  a <- data.frame(id_a = 1:5, name_a = c("Alpha GmbH", "Jan de Vries", "Beta AG", "Nord KG", "Jan de Vries h.o.d.n. Bakkerij de Molen"),
                  country = c("DE", "DE", "DE", "DE", "NL"), zip_a = c("80331", NA, "10115", "20095", NA),
                  city_a = c("Muenchen", NA, "Berlin", "Hamburg", NA), street_a = c("Marienplatz 1", NA, "Unter den Linden 5", NA, NA),
                  type = c("Legal entity", "Natural person", "Legal entity", "Legal entity", "Natural person"), stringsAsFactors = FALSE)
  b <- data.frame(id_b = 11:16, name_b = c("ALPHA GMBH", "DE VRIES JAN", "BETA AG", "BETA AKTIENGESELLSCHAFT", "NORD GMBH", "BAKKERIJ DE MOLEN"),
                  country = c(rep("DE", 5), "NL"), zip_b = c("80331", NA, "10115", "50667", NA, NA),
                  city_b = c("MUENCHEN", NA, "BERLIN", "KOELN", "HAMBURG", NA), street_b = NA, nuts = NA,
                  psn_sector = c("COMPANY", "INDIVIDUAL", "COMPANY", NA, "COMPANY", "COMPANY"), stringsAsFactors = FALSE)
  na <- norma(a, "name_a", "country", "id_a")
  nb <- norma(b, "name_b", "country", "id_b")
  m <- nmatch(na, nb, "norm_name", "norm_name")
  names(m) <- sub("\\.x$", "_a", sub("\\.y$", "_b", names(m)))
  cl <- clean(m, "id_a", "id_b", "name_a", "name_b", "lf_a", "lf_b", "zip_a", "zip_b", "city_a", "city_b",
              "street_a", "street_b", "nuts", "country_a")
  list(na = na, nb = nb, cl = cl, rk = suppressMessages(rankmatch(cl)))
}
quality_full <- function(x, pairs = x$rk) {
  match_quality(pairs, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b", zip_a = "zip_a", zip_b = "zip_b",
                city_a = "city_a", city_b = "city_b", street_a = "street_a", street_b = "street_b",
                norm_a = x$na, norm_b = x$nb, norm_key_a = "id_a", norm_key_b = "id_b", type_a = "type", type_b = "psn_sector")
}
pick <- function(q, a, b) q[q$id_a == a & q$id_b == b, ]

test_that("match_quality gives the expected classes and keeps the pairs unchanged", {
  x <- quality_example()
  q <- quality_full(x)
  expect_equal(nrow(q), nrow(x$rk))
  expect_equal(q[, names(x$rk)], as.data.frame(x$rk))
  expect_equal(pick(q, 1, 11)$q_class, "A")
  expect_equal(pick(q, 3, 13)$q_class, "A")
  expect_equal(pick(q, 3, 14)$q_class, "D")
  expect_equal(pick(q, 4, 15)$q_class, "D")
  expect_true(pick(q, 4, 15)$q_conflict_lf)
  expect_equal(pick(q, 2, 12)$q_class, "C")
})

test_that("match_quality treats persons, surname order and sole traders sensibly", {
  q <- quality_full(quality_example())
  p <- pick(q, 2, 12)
  expect_false(p$q_names_differ)
  expect_equal(p$q_type_pair, "person")
  expect_match(p$q_reasons, "name only")
  s <- pick(q, 5, 16)
  expect_true(s$q_alias)
  expect_true(is.na(s$q_names_differ))
  expect_equal(s$q_type_pair, "mixed")
  expect_equal(s$q_class, "C")
  expect_match(s$q_reasons, "matched through part of a name")
})

test_that("match_quality reads the type columns and guesses where they are empty", {
  q <- quality_full(quality_example())
  expect_equal(pick(q, 1, 11)$q_type_source_b, "field")
  expect_equal(pick(q, 3, 14)$q_type_source_b, "guessed")
  expect_equal(pick(q, 3, 14)$q_type_b, "organisation")
})

test_that("match_quality works without the optional inputs", {
  x <- quality_example()
  q <- match_quality(x$rk, "id_a", "id_b", "name_a", "name_b")
  expect_true(all(is.na(q$q_key_freq_a)))
  expect_true(all(is.na(q$q_alias)))
  expect_true(all(q$q_class %in% c("A", "B", "C", "D")))
  expect_match(pick(q, 2, 12)$q_reasons, "name frequency not available")
})

test_that("match_quality works on the output of clean() without a score, and on no rows", {
  x <- quality_example()
  q <- quality_full(x, x$cl)
  expect_true(all(is.na(q$q_margin)))
  expect_match(pick(q, 3, 13)$q_reasons, "several candidates, no score")
  expect_equal(nrow(quality_full(x, x$rk[0, ])), 0)
})

test_that("match_quality says which columns are missing", {
  x <- quality_example()
  expect_error(match_quality(x$rk[, setdiff(names(x$rk), "reg_sim")], "id_a", "id_b", "name_a", "name_b"), "reg_sim")
})
