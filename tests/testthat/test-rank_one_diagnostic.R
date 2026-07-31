# Tests for rank_one_diagnostic() (R/rank_one_diagnostic.R)

test_that("rank_one_diagnostic() validates its input", {
  expect_error(
    rank_one_diagnostic(list(a = 1), B = 5),
    "must be an object of class 'sog'"
  )
})

test_that("rank_one_diagnostic() returns correctly structured output", {
  evaluations <- data.frame(
    item_id = paste0("s", 1:10),
    gpt     = c("Include", "Include", "Exclude", "Exclude", "Include", "Exclude", "Include", "Exclude", "Include", "Exclude"),
    human_1 = c("Include", "Include", "Exclude", "Include", "Include", "Exclude", "Include", "Exclude", "Exclude", "Exclude"),
    human_2 = c("Include", "Exclude", "Exclude", "Exclude", "Include", "Include", "Include", "Exclude", "Include", "Exclude"),
    stringsAsFactors = FALSE
  )

  fit <- solve_or_guess(
    evaluations,
    system_rater_id  = "gpt",
    reference_raters = c("human_1", "human_2"),
    B = 5,
    seed = 1,
    verbose = FALSE,
    progress = FALSE
  )

  diag <- rank_one_diagnostic(fit, B = 10, seed = 1, verbose = FALSE)

  expect_s3_class(diag, "sog_rank_one")
  expect_named(
    diag,
    c("T_obs", "T_boot", "B", "n_valid_pairs", "n_params", "df", "p_value", "raters"),
    ignore.order = TRUE
  )
  expect_length(diag$T_boot, 10)
  expect_true(diag$p_value >= 0 && diag$p_value <= 1)
  expect_equal(diag$raters[1], "gpt") # system rater listed first
  expect_equal(diag$n_valid_pairs, choose(length(diag$raters), 2)) # every pair overlaps fully here
})
