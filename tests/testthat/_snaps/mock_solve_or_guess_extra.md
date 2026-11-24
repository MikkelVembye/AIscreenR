# .solve_or_guess_fast() warns about unrated pairs and validates input

    Code
      fit <- .solve_or_guess_fast(evals, system_rater_id = "sys", verbose = FALSE)
    Condition
      Warning in `.solve_or_guess_fast()`:
      There are 2 rater-item pairs with no evaluation; excluded from that rater's fit.

# solve_or_guess() accepts gpt result objects

    Code
      fit <- suppressMessages(solve_or_guess(res, system_rater_id = "gpt-5-nano",
        reference_raters = "human_code", B = 2, seed = 1, progress = FALSE))
    Condition
      Warning in `solve_or_guess()`:
      At least 2 reference raters (3 total) are recommended.

