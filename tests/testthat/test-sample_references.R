# Tests for sample_references() (R/sample_references.R)

dat <- data.frame(
  record_id       = 1:20,
  decision_binary = rep(c(1, 0), each = 10),
  human_code      = c(rep(1, 8), rep(0, 12))
)

test_that("simple random sampling returns n rows and is reproducible", {
  s1 <- sample_references(dat, n = 5)
  s2 <- sample_references(dat, n = 5)

  expect_equal(nrow(s1), 5)
  expect_equal(s1, s2) # same default seed
  expect_false(anyDuplicated(s1$record_id) > 0) # without replacement
})

test_that("simple random sampling respects prob_vec", {
  # Only the first record has a non-zero chance of being drawn
  s <- sample_references(dat, n = 1, prob_vec = c(1, rep(0, 19)))
  expect_equal(s$record_id, 1)
})

test_that("sample_references() warns when no seed is given", {
  expect_warning(sample_references(dat, n = 3, seed = NULL), "No seed provided")
})

test_that("sample_references() needs n, or relevant_col with c_target and R_c", {
  expect_error(sample_references(dat), "Provide `n` for simple random sampling")

  expect_error(
    sample_references(dat, relevant_col = "decision_binary"),
    "Provide `n`, or both `c_target` and `R_c`"
  )
})

test_that("sample_references() checks that id_col and relevant_col exist", {
  expect_error(
    sample_references(
      dat[, c("decision_binary", "human_code")],
      relevant_col = "decision_binary", c_target = 0.9, R_c = 0.5
    ),
    "`id_col` = \"record_id\" was not found"
  )

  expect_error(
    sample_references(dat, relevant_col = "not_a_column", c_target = 0.9, R_c = 0.5),
    "not_a_column.*was not found"
  )
})

test_that("target set is sampled from relevant records with the right size", {
  # k = ceiling(log(1 - R_c) / log(c_target)) = ceiling(log(0.5) / log(0.9)) = 7
  res <- sample_references(
    dat, relevant_col = "decision_binary", c_target = 0.9, R_c = 0.5
  )

  expect_equal(res$k, 7)
  expect_length(res$target_ids, 7)
  expect_false(anyDuplicated(res$target_ids) > 0)
  expect_true(all(res$target_set$decision_binary == 1))
  expect_equal(res$reliability_guarantee, 1 - 0.9^7)
})

test_that("several relevant_col columns must all be 1", {
  res <- sample_references(
    dat, relevant_col = c("decision_binary", "human_code"),
    c_target = 0.9, R_c = 0.5
  )

  expect_true(all(res$target_set$decision_binary == 1))
  expect_true(all(res$target_set$human_code == 1))
})

test_that("sample_references() stops when there are too few relevant records", {
  # k = 45 but only 10 relevant records
  expect_error(
    sample_references(
      dat, relevant_col = "decision_binary", c_target = 0.95, R_c = 0.9
    ),
    "Only 10 relevant records available"
  )
})

test_that("sample_references() prints a message when message = TRUE", {
  expect_message(
    sample_references(
      dat, relevant_col = "decision_binary", c_target = 0.9, R_c = 0.5,
      message = TRUE
    ),
    "sampled with replacement"
  )
})
