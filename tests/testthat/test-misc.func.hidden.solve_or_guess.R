# Tests for internal solve-or-guess engine functions (R/misc.func.hidden.solve_or_guess.R)

evaluations <- data.frame(
  item_id  = rep(paste0("s", 1:10), 2),
  rater_id = rep(c("system", "human"), each = 10),
  evaluation = c(
    "Include", "Include", "Exclude", "Exclude", "Include",
    "Exclude", "Include", "Exclude", "Include", "Exclude",
    "Include", "Include", "Exclude", "Include", "Include",
    "Exclude", "Include", "Exclude", "Exclude", "Exclude"
  ),
  stringsAsFactors = FALSE
)

test_that(".logit_inv() computes the logistic sigmoid", {
  expect_equal(.logit_inv(0), 0.5)
  expect_true(.logit_inv(10) > 0.99)
  expect_true(.logit_inv(-10) < 0.01)
})

test_that(".cohens_kappa_numerator() matches a hand-computed example", {
  Y <- cbind(c(1, 1, 2, 2), c(1, 1, 2, 2))
  out <- .cohens_kappa_numerator(Y)
  expect_equal(out[1, 2], 0.5, tolerance = 1e-8)
})

test_that(".logistic_fit() recovers the intercept-only solution", {
  Z <- matrix(1, nrow = 10, ncol = 1)
  y <- rep(0.8, 10)
  beta <- .logistic_fit(Z, y)
  expect_equal(.logit_inv(beta), 0.8, tolerance = 1e-4)
})

test_that(".solve_or_guess_fast() returns valid, correctly structured output", {
  fit <- .solve_or_guess_fast(evaluations, system_rater_id = "system", verbose = FALSE)

  expect_named(
    fit,
    c("rater_ability", "item_estimates", "guessing_distribution", "class_distribution"),
    ignore.order = TRUE
  )
  expect_setequal(unique(fit$rater_ability$rater_id), c("system", "human"))
  expect_true(all(fit$item_estimates$success_prob >= 0 & fit$item_estimates$success_prob <= 1))

  guess_sums <- tapply(fit$guessing_distribution$probability, fit$guessing_distribution$rater_id, sum)
  expect_equal(as.vector(guess_sums), rep(1, length(guess_sums)), tolerance = 1e-6)
})

partial_evaluations <- data.frame(
  item_id  = c(paste0("s", 1:10), paste0("s", 1:5)),
  rater_id = rep(c("system", "human"), c(10, 5)),
  evaluation = c(
    "Include", "Include", "Exclude", "Exclude", "Include",
    "Exclude", "Include", "Exclude", "Include", "Exclude",
    "Include", "Include", "Exclude", "Exclude", "Include"
  ),
  stringsAsFactors = FALSE
)

test_that(".solve_or_guess_fast() excludes unrated items instead of treating them as a 'MISSING' answer", {
  fit <- .solve_or_guess_fast(partial_evaluations, system_rater_id = "system", verbose = FALSE)

  expect_setequal(unique(fit$guessing_distribution$evaluation), c("Include", "Exclude"))

  human_estimates <- fit$item_estimates[fit$item_estimates$rater_id == "human", ]
  rated <- human_estimates$item_id %in% paste0("s", 1:5)
  expect_true(all(!is.na(human_estimates$expected_success[rated])))
  expect_true(all(is.na(human_estimates$expected_success[!rated])))
})

test_that(".nonparametric_bootstrap_solve_or_guess() returns B replicates", {
  boot <- .nonparametric_bootstrap_solve_or_guess(
    evaluations, system_rater_id = "system", B = 3, progress = FALSE
  )

  expect_equal(boot$n_boot, 3)
  expect_setequal(unique(boot$bootstrap_rater_ability$bootstrap_iter), 1:3)
})

gpt_tbl <- data.frame(
  studyid = 1:5, promptid = 1L, topp = 1, model = "gpt-4o-mini",
  decision_binary = c(1, 0, 1, 0, 1),
  human_code      = c(1, 0, 0, 0, 1),
  stringsAsFactors = FALSE
)
class(gpt_tbl) <- c("gpt_tbl", class(gpt_tbl))

test_that(".tabscreen_to_wide() converts a gpt_tbl object into one row per item", {
  wide <- .tabscreen_to_wide(gpt_tbl, human_decision = "human_code")

  expect_setequal(names(wide), c("item_id", "human_code", "gpt-4o-mini"))
  expect_equal(nrow(wide), 5)
  expect_true(all(wide[["gpt-4o-mini"]] %in% c("Include", "Exclude")))
})

test_that(".tabscreen_to_wide() errors on more than one prompt/top_p configuration", {
  gpt_tbl$promptid <- c(1, 1, 2, 2, 2)

  expect_error(
    .tabscreen_to_wide(gpt_tbl, human_decision = "human_code"),
    "more than one prompt/top_p configuration"
  )
})
