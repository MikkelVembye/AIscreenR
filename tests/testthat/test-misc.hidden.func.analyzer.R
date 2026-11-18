# Tests for .calc_perform() (R/misc.hidden.func.analyzer.R)

# Counts worked out by hand:
#   human in,  gpt in  = 3   (rows 1-3)
#   human in,  gpt ex  = 1   (row 4)
#   human ex,  gpt in  = 2   (rows 5-6)
#   human ex,  gpt ex  = 4   (rows 7-10)
perf_dat <- data.frame(
  studyid               = 1:10,
  human_code            = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
  final_decision_gpt_num = c(1, 1, 1, 0, 1, 1, 0, 0, 0, 0)
)

test_that(".calc_perform() counts the four decision combinations", {
  res <- .calc_perform(perf_dat, human_code)

  expect_equal(nrow(res), 1)
  expect_equal(res$n_refs, 10)
  expect_equal(res$n_screened, 10)
  expect_equal(res$n_missing, 0)
  expect_equal(res$human_in_gpt_in, 3)
  expect_equal(res$human_in_gpt_ex, 1)
  expect_equal(res$human_ex_gpt_in, 2)
  expect_equal(res$human_ex_gpt_ex, 4)
})

test_that(".calc_perform() computes the performance measures", {
  res <- .calc_perform(perf_dat, human_code)

  expect_equal(res$precision, 3 / (3 + 2))
  expect_equal(res$recall, 3 / (3 + 1))
  expect_equal(res$npv, 4 / (4 + 1))
  expect_equal(res$specificity, 4 / (4 + 2))
  expect_equal(res$bacc, (3 / 4 + 4 / 6) / 2)
  expect_equal(res$accuracy, 3 / 10)
  expect_equal(res$p_agreement, 7 / 10)

  # chance agreement is 0.5, so irr = (0.7 - 0.5) / (1 - 0.5) = 0.4
  expect_equal(res$irr, 0.4)
  expect_equal(res$level_of_agreement, "Weak")
})

test_that(".calc_perform() reports references without a gpt decision as missing", {
  dat_na <- perf_dat
  dat_na$final_decision_gpt_num[10] <- NA

  res <- .calc_perform(dat_na, human_code)

  expect_equal(res$n_refs, 10)
  expect_equal(res$n_screened, 9)
  expect_equal(res$n_missing, 1)
  expect_equal(res$human_ex_gpt_ex, 3) # row 10 no longer counts
})
