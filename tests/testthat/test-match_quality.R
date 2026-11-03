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
  expect_match(pick(q, 2, 12)$q_reasons, "person-like name")
  expect_true(all(is.na(q$q_namesakes)))
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

test_that("quality_summary counts classes and reasons", {
  q <- quality_full(quality_example())
  s <- quality_summary(q)
  expect_equal(s$classes$class, c("A", "B", "C", "D"))
  expect_equal(sum(s$classes$total), nrow(q))
  expect_equal(s$classes$total, as.vector(table(factor(q$q_class, levels = c("A", "B", "C", "D")))))
  expect_true("name only" %in% s$reasons$reason)
  expect_equal(s$reasons$total[s$reasons$reason == "name only"], sum(grepl("name only", q$q_reasons)))
  expect_false(any(grepl("^both |^agreeing:", s$reasons$reason)))
  e <- quality_summary(quality_full(quality_example(), quality_example()$rk[0, ]))
  expect_equal(sum(e$classes$total), 0)
  expect_equal(nrow(e$reasons), 0)
})

test_that("match_quality flags names that are not distinctive on the reference side", {
  x <- quality_example()
  ## a reference side where JAN, DE and VRIES are common words, and BAKKERIJ and MOLEN rare
  common <- data.frame(id_b = 100 + 1:300,
                       norm_name = c(paste("JAN", sample(c("BAKKER", "JANSEN", "SMIT"), 100, TRUE)),
                                     paste("DE", sample(c("BOER", "GROOT", "JONG"), 100, TRUE)),
                                     paste("PIET", sample(c("VRIES", "MEER", "WIT"), 100, TRUE))), stringsAsFactors = FALSE)
  nb <- rbind(x$nb[, c("id_b", "norm_name")], common)
  q <- match_quality(x$rk, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b",
                     norm_a = x$na, norm_b = nb, norm_key_a = "id_a", norm_key_b = "id_b")
  jan <- pick(q, 2, 12)
  molen <- pick(q, 5, 16)
  expect_gt(jan$q_namesakes, molen$q_namesakes)
  expect_gt(jan$q_rarest_word, molen$q_rarest_word)
  q2 <- match_quality(x$rk, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b",
                      norm_a = x$na, norm_b = nb, norm_key_a = "id_a", norm_key_b = "id_b", namesakes_max = jan$q_namesakes)
  expect_match(pick(q2, 2, 12)$q_reasons, "name not distinctive")
  expect_false(grepl("name not distinctive", pick(q2, 5, 16)$q_reasons))
})

test_that("person-like names are flagged also when the name has company words", {
  x <- quality_example()
  fn <- build_first_names(c(rep("Jan", 10), rep("Mark", 10)), country = c(rep("NL", 10), rep("DE", 10)),
                          org_names = c(rep("Mark Media GmbH", 4), rep("Other GmbH", 6)), org_country = "DE", min_count = 5)
  q <- match_quality(x$rk, "id_a", "id_b", "name_a", "name_b", lf_a = "lf_a", lf_b = "lf_b",
                     type_side = "a", first_names = fn, country_a = "country_a")
  expect_true(pick(q, 5, 16)$q_person_like)       # Jan de Vries h.o.d.n. Bakkerij de Molen: JAN
  expect_false(pick(q, 1, 11)$q_person_like)      # Alpha GmbH
})
