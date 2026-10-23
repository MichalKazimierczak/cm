test_that("rankmatch works without a bvd_is_root column", {
  r <- data.frame(id = 1:2, name_sim = c(0.5, 1), lf_sim = 1, reg_sim = c(TRUE, FALSE), city_sim = 1, street_sim = 0)
  expect_message(x <- rankmatch(r), "bvd_is_root")
  expect_equal(x$final_score, c(0.7, 0.6))
  expect_false("bvd_is_root" %in% names(x))
})

test_that("rankmatch prefers root entities when bvd_is_root is given", {
  r <- data.frame(id = 1:2, bvd_is_root = c(TRUE, FALSE), name_sim = c(0, 1), lf_sim = c(0, 1), reg_sim = 1, city_sim = 1, street_sim = 1)
  expect_equal(rankmatch(r)$id, c(1, 2))
  expect_equal(rankmatch(r, prefer_root = FALSE)$id, c(2, 1))
})
