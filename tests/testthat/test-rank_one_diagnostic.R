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
    human_3 = c("Include", "Include", "Exclude", "Exclude", "Include", "Exclude", "Exclude", "Exclude", "Include", "Include"),
    stringsAsFactors = FALSE
  )

  fit <- solve_or_guess(
    evaluations,
    system_rater_id  = "gpt",
    reference_raters = c("human_1", "human_2", "human_3"),
    B = 5,
    seed = 1,
    progress = FALSE
  )

  diag <- rank_one_diagnostic(fit, B = 10, seed = 1, verbose = FALSE)

  expect_s3_class(diag, "sog_rank_one")
  expect_named(
    diag,
    c("T_obs", "T_boot", "B", "n_valid_pairs", "n_params", "df", "p_value", "raters", "Pi", "Pe"),
    ignore.order = TRUE
  )
  expect_length(diag$T_boot, 10)
  expect_true(diag$df > 0)
  # (1 + count) / (B + 1) is never 0
  expect_true(diag$p_value >= 1 / 11 && diag$p_value <= 1)
  expect_equal(diag$raters[1], "gpt") # system rater listed first
  expect_equal(diag$n_valid_pairs, choose(length(diag$raters), 2)) # every pair overlaps fully here
})

test_that("rank_one_diagnostic() warns and returns NA p-value when df <= 0", {
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
    progress = FALSE
  )

  # 3 raters -> 3 pairs, 3 parameters -> df = 0
  expect_warning(
    diag <- rank_one_diagnostic(fit, B = 10, seed = 1, verbose = FALSE),
    "Degrees of freedom"
  )
  expect_equal(diag$df, 0)
  expect_true(is.na(diag$p_value))
})

test_that("rank_one_diagnostic() reads numeric 0/1 labels by name, not position", {
  evaluations <- data.frame(
    item_id = paste0("s", 1:10),
    gpt     = c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0),
    human_1 = c(1, 1, 0, 1, 1, 0, 1, 0, 0, 0),
    human_2 = c(1, 0, 0, 0, 1, 1, 1, 0, 1, 0),
    human_3 = c(1, 1, 0, 0, 1, 0, 0, 0, 1, 1)
  )

  fit <- solve_or_guess(
    evaluations,
    system_rater_id  = "gpt",
    reference_raters = c("human_1", "human_2", "human_3"),
    B = 5,
    seed = 1,
    progress = FALSE
  )

  diag <- rank_one_diagnostic(fit, B = 5, seed = 1, verbose = FALSE)
  Pi <- diag$Pi
  gd <- fit$fit$guessing_distribution

  expect_equal(colnames(Pi), c("0", "1"))
  expect_equal(unname(rowSums(Pi)), rep(1, 4))
  # The old bug made every row (1, 0)
  expect_false(any(Pi[, "0"] == 1))
  # Pi must match the fitted guess probabilities for each label
  for (r in diag$raters) {
    p0 <- gd$probability[gd$rater_id == r & gd$evaluation == 0]
    p1 <- gd$probability[gd$rater_id == r & gd$evaluation == 1]
    expect_equal(unname(Pi[r, "1"]), p1 / (p0 + p1))
  }
})

test_that("rank_one_diagnostic() prints progress messages when verbose = TRUE", {
  evaluations <- data.frame(
    item_id = paste0("s", 1:10),
    gpt     = c("Include", "Include", "Exclude", "Exclude", "Include", "Exclude", "Include", "Exclude", "Include", "Exclude"),
    human_1 = c("Include", "Include", "Exclude", "Include", "Include", "Exclude", "Include", "Exclude", "Exclude", "Exclude"),
    human_2 = c("Include", "Exclude", "Exclude", "Exclude", "Include", "Include", "Include", "Exclude", "Include", "Exclude"),
    human_3 = c("Include", "Include", "Exclude", "Exclude", "Include", "Exclude", "Exclude", "Exclude", "Include", "Include"),
    stringsAsFactors = FALSE
  )

  fit <- solve_or_guess(
    evaluations,
    system_rater_id  = "gpt",
    reference_raters = c("human_1", "human_2", "human_3"),
    B = 5,
    seed = 1,
    progress = FALSE
  )

  msgs <- testthat::capture_messages(
    rank_one_diagnostic(fit, B = 5, seed = 1, verbose = TRUE)
  )

  # 4 raters give 6 pairs and 4 parameters, so df = 2
  expect_match(msgs, "Valid rater pairs (with overlap): 6 / 6 possible. Parameters: 4. Degrees of freedom: 2.", fixed = TRUE, all = FALSE)
  expect_match(msgs, "Parametric bootstrap (B = 5)", fixed = TRUE, all = FALSE)
})
