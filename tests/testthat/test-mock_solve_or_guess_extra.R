long_evals <- function(n = 14) {
  set.seed(5819)
  truth <- rbinom(n, 1, 0.5)
  noisy <- \(p) ifelse(runif(n) < p, truth, 1 - truth)
  tidyr::expand_grid(item_id = paste0("s", seq_len(n)), rater_id = c("sys", "h1", "h2")) |>
    dplyr::mutate(
      truth = rep(truth, each = 3),
      evaluation = dplyr::case_when(
        rater_id == "sys" ~ noisy(0.85)[rep(seq_len(n), each = 3)],
        rater_id == "h1" ~ noisy(0.75)[rep(seq_len(n), each = 3)],
        TRUE ~ noisy(0.8)[rep(seq_len(n), each = 3)]
      ),
      evaluation = ifelse(evaluation == 1, "Include", "Exclude")
    ) |>
    dplyr::select(item_id, rater_id, evaluation)
}

screened_with_humans <- function(reps = 1, models = "gpt-5-nano", env = parent.frame()) {
  dat <- mock_dat(8)
  dat$human_code <- rep(c(1, 0), 4)
  dat$human_code2 <- rep(c(1, 1, 0, 0), 2)
  local_mock_api(llm_handler("gpt"), .env = env)
  res <- suppressMessages(tabscreen_gpt(
    dat, prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = models, reps = reps, max_tries = 1
  ))
  res$answer_data$decision_binary <- rep(c(1, 0, 0, 1), length.out = nrow(res$answer_data))
  if (!is.null(res$answer_data_aggregated)) {
    res$answer_data_aggregated$final_decision_gpt <- rep(c("Include", "Exclude", "Exclude", "Include"), length.out = nrow(res$answer_data_aggregated))
  }
  res
}

test_that(".solve_or_guess_fast() fits with item difficulty features and verbose output", {
  evals <- long_evals()
  difficulty <- tidyr::expand_grid(item_id = unique(evals$item_id), feature_id = c("len", "tech")) |>
    dplyr::mutate(value = rep(c(0.2, -0.4, 1, 0.5, -1, 0.1, 0.3), length.out = dplyr::n()))

  fit <- suppressWarnings(.solve_or_guess_fast(evals, difficulty = difficulty, system_rater_id = "sys", verbose = FALSE))
  expect_true(all(c("intercept", "len", "tech") %in% fit$rater_ability$parameter_id))

  expect_output(.solve_or_guess_fast(evals, system_rater_id = "sys", verbose = TRUE), "Iteration|Convergence")
})

test_that(".solve_or_guess_fast() warns about unrated pairs and validates input", {
  evals <- long_evals()
  evals <- evals[!(evals$rater_id == "h2" & evals$item_id %in% c("s1", "s2")), ]
  expect_snapshot(fit <- .solve_or_guess_fast(evals, system_rater_id = "sys", verbose = FALSE))

  expect_error(.solve_or_guess_fast(data.frame(a = 1), system_rater_id = "sys"), "must contain columns")
  expect_error(.solve_or_guess_fast(long_evals(), system_rater_id = "nobody"), "not found in the data")
  expect_error(
    .solve_or_guess_fast(long_evals(), difficulty = data.frame(item_id = "s1"), system_rater_id = "sys"),
    "difficulty"
  )
})

test_that("the bootstrap resamples items together with difficulty features", {
  evals <- long_evals(60)
  difficulty <- tibble::tibble(
    item_id = unique(evals$item_id), feature_id = "len", value = sin(seq_len(60))
  )
  seen <- new.env()
  local_mocked_bindings(.solve_or_guess_fast = function(evaluations, difficulty, ...) {
    seen$difficulty <- difficulty
    list(rater_ability = tibble::tibble(rater_id = "sys", parameter_id = "intercept", estimate = 0))
  })
  boot <- suppressWarnings(.nonparametric_bootstrap_solve_or_guess(
    evals, difficulty = difficulty, system_rater_id = "sys", B = 2, progress = FALSE
  ))
  expect_equal(nrow(seen$difficulty), 60)
  expect_equal(boot$n_boot, 2)
  expect_equal(dplyr::n_distinct(boot$bootstrap_rater_ability$bootstrap_iter), 2)
})

test_that("solve_or_guess() accepts gpt result objects", {
  res <- screened_with_humans()
  fit <- suppressWarnings(solve_or_guess(
    res, system_rater_id = "gpt-5-nano", reference_raters = c("human_code", "human_code2"),
    B = 3, seed = 1, progress = FALSE
  ))
  expect_s3_class(fit, "sog")

  agg <- screened_with_humans(reps = 2)
  fit <- suppressWarnings(solve_or_guess(
    agg, system_rater_id = "gpt-5-nano", reference_raters = c("human_code", "human_code2"),
    B = 3, seed = 1, progress = FALSE
  ))
  expect_s3_class(fit, "sog")

  fit <- suppressWarnings(solve_or_guess(
    agg$answer_data_aggregated, system_rater_id = "gpt-5-nano", reference_raters = "human_code",
    B = 3, seed = 1, progress = FALSE
  ))
  expect_s3_class(fit, "sog")

  expect_snapshot(
    fit <- solve_or_guess(
      res, system_rater_id = "gpt-5-nano", reference_raters = "human_code",
      B = 2, seed = 1, progress = FALSE
    ) |> suppressMessages()
  )
})

test_that(".tabscreen_to_wide() converts and validates tabscreen objects", {
  res <- screened_with_humans()
  wide <- .tabscreen_to_wide(res, "human_code")
  expect_equal(names(wide), c("item_id", "human_code", "gpt-5-nano"))
  expect_setequal(wide[["gpt-5-nano"]], c("Include", "Exclude"))
  expect_setequal(wide$human_code, c("Include", "Exclude"))

  expect_error(.tabscreen_to_wide(res, 1), "character vector")
  expect_error(.tabscreen_to_wide(res, "nope"), "not found")
  expect_error(.tabscreen_to_wide(data.frame(a = 1), "a"), "must be a data.frame")

  two_prompts <- res$answer_data
  two_prompts$promptid[5:8] <- 2L
  class(two_prompts) <- c("gpt_tbl", class(two_prompts))
  expect_error(.tabscreen_to_wide(two_prompts, "human_code"), "more than one prompt")

  tbl <- res$answer_data
  class(tbl) <- c("gpt_tbl", class(tbl))
  expect_equal(nrow(.tabscreen_to_wide(tbl, "human_code")), 8)

  agg <- screened_with_humans(reps = 2)$answer_data_aggregated
  expect_equal(nrow(.tabscreen_to_wide(agg, "human_code")), 8)
})

test_that("calc of expected agreement helpers behave on trivial input", {
  expect_equal(.logit_inv(0), 0.5)
})
