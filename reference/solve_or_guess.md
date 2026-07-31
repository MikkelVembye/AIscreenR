# Fit the solve-or-guess model to compare an automated screener against human raters

Fits the solve-or-guess model (Rohe et al., 2026) to a set of raters.
One automated ("system") rater, such as an AI assistant, and one or more
human raters, who each labeled some or all of the same items as
Include/Exclude (or any other binary/categorical decision). The model
estimates each rater's solving probability i.e. how often the rater
actually recovers the true label rather than guessing. This is done
without requiring a gold-standard label set. The kappa-ratio of the
system rater against each reference human rater is the primary
comparison: a bootstrap interval that contains 1 means the two are
statistically indistinguishable in solving probability.

## Usage

``` r
solve_or_guess(
  evaluations,
  system_rater_id,
  reference_raters,
  item_id = "item_id",
  B = 1000,
  conf_level = 0.95,
  seed = NULL,
  verbose = TRUE,
  progress = TRUE
)
```

## Arguments

- evaluations:

  Either (a) a `data.frame`/`tibble` with one row per item (study),
  containing at least one column with the AI decision and at least one
  column with a human decision - see `system_rater_id` and
  `reference_raters` below for how those columns are identified - or (b)
  a tabscreen result object of class `'gpt'`, `'gpt_tbl'`, or
  `'gpt_agg_tbl'` (i.e. the object returned by
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools_responses.md)
  or one of the other tabscreen functions). When (b) is used, each model
  that was screened becomes its own AI rater (named after the model),
  and the result must reflect a single prompt/top_p configuration.

- system_rater_id:

  The name of the column in `evaluations` holding the automated/system
  rater's decisions (e.g. `"gpt-4o-mini"`), coded the same way as the
  human decision column(s).

- reference_raters:

  Character vector naming the column(s) in `evaluations` to compare the
  system rater against - typically one or more human raters, coded
  numerically as `1` (include) / `0` (exclude); other AI raters can also
  be included here. When `evaluations` is a tabscreen result object,
  these must be the human decision column(s) already present in that
  object.

- item_id:

  Name of the column in `evaluations` that identifies each item (study).
  Ignored when `evaluations` is a tabscreen result object (the object's
  study ID is used automatically). Default `"item_id"`.

- B:

  Number of nonparametric (item-level) bootstrap replicates used for
  confidence intervals. Default `1000`. The `B` model refits run in
  parallel whenever a non-sequential
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  is active (e.g. `future::plan(future::multisession)`).

- conf_level:

  Confidence level for the bootstrap intervals. Default `0.95`.

- seed:

  Optional integer seed, set before bootstrapping, for reproducibility.

- verbose:

  Logical; print progress messages while fitting/bootstrapping. Default
  `TRUE`.

- progress:

  Logical; show a progress bar for the `B` bootstrap refits. Default
  `TRUE`.

## Value

An object of class `"sog"`, a list with elements:

|  |  |  |
|----|----|----|
| **solving_probabilities** | `tibble` | `rater_id`, `p_hat`, and bootstrap `ci_lower`/`ci_upper` for each rater. |
| **pairwise_kappa** | `tibble` | observed Cohen's kappa for every rater pair, restricted to items both raters in the pair actually rated (`n_overlap`). |
| **kappa_ratios** | `tibble` | \\p_system/p_reference\\ for each reference rater, with a bootstrap CI. |
| **fit** | `list` | the raw solve-or-guess model fit (`rater_ability`, `item_estimates`, `guessing_distribution`, `class_distribution`). |
| **evaluations** | `tibble` | the long-format (`item_id`, `rater_id`, `evaluation`) data actually used to fit the model, after any tabscreen-object conversion and dropping of unrated pairs. |
| **system_rater_id** | `character` | the column name used as the system rater. |
| **reference_raters** | `character` | the column name(s) used as the reference raters. |
| **bootstrap_rater_ability** | `tibble` | every bootstrap replicate's rater ability estimates, tagged by `bootstrap_iter` - the raw draws behind `solving_probabilities`' and `kappa_ratios`' confidence intervals. |
| **B** | `integer` | the number of bootstrap replicates used. |
| **conf_level** | `numeric` | the confidence level used for the bootstrap intervals. |

## References

Rohe, K., Krauska, A. N., Collins, G., Higgins, J., & Pustejovsky, J.
(2026). The solve or guess model: Validating automated systems against
heterogeneous human raters. Working paper.

## Examples

``` r
if (FALSE) { # \dontrun{
evaluations <- data.frame(
  item_id  = paste0("study_", 1:50),
  human    = sample(c("Include", "Exclude"), 50, replace = TRUE),
  gpt      = sample(c("Include", "Exclude"), 50, replace = TRUE)
)

fit <- solve_or_guess(
  evaluations,
  system_rater_id = "gpt",
  reference_raters = "human",
  B = 200
)

# Using a tabscreen result object directly
res <- tabscreen_gpt(
  data = filges2015_dat,
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract
)

fit <- solve_or_guess(
  res,
  system_rater_id = "gpt-4o-mini",
  reference_raters = c("human_code_1", "human_code_2"),
  B = 200
)
} # }
```
