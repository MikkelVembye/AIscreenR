ref_dat <- function() {
  tibble::tibble(
    record_id = 1:40,
    human_code = rep(c(1, 0), 20),
    decision_binary = rep(c(1, 1, 0, 0), 10)
  )
}

test_that("sample_references() draws a simple random sample", {
  res <- sample_references(ref_dat(), n = 5, seed = 4021)
  expect_equal(nrow(res), 5)
  expect_equal(nrow(sample_references(ref_dat(), n = 10, with_replacement = TRUE, seed = 4021)), 10)
  expect_equal(nrow(sample_references(ref_dat(), n = 3, prob_vec = rep(1 / 40, 40), seed = 4021)), 3)
  expect_equal(
    sample_references(ref_dat(), n = 5, seed = 4021),
    sample_references(ref_dat(), n = 5, seed = 4021)
  )
})

test_that("sample_references() warns without a seed", {
  expect_snapshot(res <- sample_references(ref_dat(), n = 2, seed = NULL))
})

test_that("sample_references() samples a target set", {
  res <- sample_references(
    ref_dat(), relevant_col = "human_code", c_target = 0.9, R_c = 0.8, seed = 4021
  )
  expect_equal(res$k, ceiling(log(0.2) / log(0.9)))
  expect_equal(nrow(res$target_set), res$k)
  expect_equal(res$target_ids, res$target_set$record_id)
  expect_equal(res$reliability_guarantee, 1 - 0.9^res$k)
  expect_true(all(res$target_set$human_code == 1))

  both <- sample_references(
    ref_dat(), relevant_col = c("human_code", "decision_binary"), c_target = 0.8, R_c = 0.7, seed = 4021
  )
  expect_true(all(both$target_set$human_code == 1 & both$target_set$decision_binary == 1))

  expect_message(
    sample_references(
      ref_dat(), relevant_col = "human_code", c_target = 0.8, R_c = 0.7, message = TRUE, seed = 4021
    ),
    "Reliability guarantee"
  )
})

test_that("sample_references() validates its input", {
  expect_error(sample_references(ref_dat()), "Provide `n`")
  expect_error(
    sample_references(ref_dat(), relevant_col = "human_code", id_col = "nope", c_target = 0.9, R_c = 0.9),
    "id_col"
  )
  expect_error(sample_references(ref_dat(), relevant_col = "human_code"), "c_target")
  expect_error(
    sample_references(ref_dat(), relevant_col = "missing", c_target = 0.9, R_c = 0.9),
    "was not found"
  )
  expect_error(
    sample_references(ref_dat(), relevant_col = "human_code", c_target = 0.99, R_c = 0.99999),
    "relevant records available"
  )
})
