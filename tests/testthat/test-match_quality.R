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
