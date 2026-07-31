# Tests for solve_or_guess() (R/solve_or_guess.R)

evaluations <- data.frame(
  item_id = paste0("s", 1:10),
  gpt     = c("Include", "Include", "Exclude", "Exclude", "Include", "Exclude", "Include", "Exclude", "Include", "Exclude"),
  human   = c("Include", "Include", "Exclude", "Include", "Include", "Exclude", "Include", "Exclude", "Exclude", "Exclude"),
  stringsAsFactors = FALSE
)

test_that("solve_or_guess() fits a plain wide data.frame and returns expected structure", {
  fit <- solve_or_guess(
    evaluations,
    system_rater_id  = "gpt",
    reference_raters = "human",
    B = 5,
    seed = 1,
    verbose = FALSE,
    progress = FALSE
  )

  expect_s3_class(fit, "sog")
  expect_setequal(fit$solving_probabilities$rater_id, c("gpt", "human"))
  expect_true(all(fit$solving_probabilities$p_hat >= 0 & fit$solving_probabilities$p_hat <= 1))
  expect_equal(fit$pairwise_kappa$n_overlap, 10)
  expect_equal(fit$kappa_ratios$comparison, "gpt / human")
})

test_that("solve_or_guess() validates its arguments", {
  expect_error(
    solve_or_guess(evaluations, system_rater_id = c("gpt", "human"), reference_raters = "human"),
    "single column name"
  )
  expect_error(
    solve_or_guess(evaluations, system_rater_id = "gpt", reference_raters = "gpt"),
    "must not also appear"
  )
  expect_error(
    solve_or_guess(evaluations, system_rater_id = "gpt", reference_raters = "nonexistent"),
    "missing column"
  )
})

test_that("solve_or_guess() supports multiple reference raters and drops missing decisions", {
  evaluations2 <- data.frame(
    item_id = paste0("s", 1:8),
    gpt     = c("Include", "Exclude", "Include", "Exclude", "Include", "Exclude", "Include", "Exclude"),
    human_1 = c("Include", "Exclude", "Exclude", "Exclude", "Include", "Include", "Include", "Exclude"),
    human_2 = c("Include", "Exclude", "Include", "Include", NA, NA, "Include", "Exclude"),
    stringsAsFactors = FALSE
  )

  fit <- suppressWarnings(solve_or_guess(
    evaluations2,
    system_rater_id  = "gpt",
    reference_raters = c("human_1", "human_2"),
    B = 5,
    seed = 2,
    verbose = FALSE,
    progress = FALSE
  ))

  expect_setequal(fit$solving_probabilities$rater_id, c("gpt", "human_1", "human_2"))
  expect_equal(sum(fit$evaluations$rater_id == "human_2"), 6) # the 2 NA rows are dropped, not kept
})

test_that("solve_or_guess() accepts a tabscreen result object directly", {
  gpt_tbl <- data.frame(
    studyid = 1:6, promptid = 1L, topp = 1, model = "gpt-4o-mini",
    decision_binary = c(1, 0, 1, 0, 1, 0),
    human_code      = c(1, 0, 0, 0, 1, 1),
    stringsAsFactors = FALSE
  )
  class(gpt_tbl) <- c("gpt_tbl", class(gpt_tbl))

  fit <- solve_or_guess(
    gpt_tbl,
    system_rater_id  = "gpt-4o-mini",
    reference_raters = "human_code",
    B = 5,
    seed = 3,
    verbose = FALSE,
    progress = FALSE
  )

  expect_s3_class(fit, "sog")
  expect_setequal(fit$solving_probabilities$rater_id, c("gpt-4o-mini", "human_code"))
})
