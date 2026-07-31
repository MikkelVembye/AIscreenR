# Rank-one diagnostic for a solve-or-guess fit

This function checks whether the solve-or-guess fit from
[`solve_or_guess`](https://mikkelvembye.github.io/AIscreenR/reference/solve_or_guess.md)
is consistent with its core assumption: a single solving probability per
rater explains how often any two raters agree. Under this assumption,
each pair of raters' agreement rate should be fully explained by a
shared chance baseline plus the product of their two individual solving
probabilities, with nothing pair-specific left over. Departure from that
structure is measured by a residual sum of squares (`T_obs`), computed
over rater pairs that share at least one rated item.

The significance of `T_obs` is assessed with a parametric bootstrap.
Here we simulate fresh datasets from the fitted model, refit the
rank-one form, and recompute the residual each time (`T_boot`). The
p-value is the fraction of bootstrap replicates whose residual is at
least as large as the observed one; a small p-value means the observed
agreement pattern is not well explained by a single solving probability
per rater.

## Usage

``` r
rank_one_diagnostic(x, B = 1000, seed = NULL, verbose = TRUE)
```

## Arguments

- x:

  An object of class `"sog"`, as returned by
  [`solve_or_guess`](https://mikkelvembye.github.io/AIscreenR/reference/solve_or_guess.md).

- B:

  Number of parametric bootstrap replicates. Default `1000`.

- seed:

  Optional integer seed, set before bootstrapping, for reproducibility.

- verbose:

  Logical; print progress messages, including the valid-pairs/
  degrees-of-freedom summary. Default `TRUE`.

## Value

An object of class `"sog_rank_one"`, a list with elements:

|  |  |  |
|----|----|----|
| **T_obs** | `numeric` | the observed rank-one residual sum of squares, computed from the real data. |
| **T_boot** | `numeric` | length-`B` vector of the residual recomputed from each parametric bootstrap replicate - the simulated null distribution that `T_obs` is compared against. |
| **B** | `integer` | the number of bootstrap replicates used. |
| **n_valid_pairs** | `integer` | number of rater pairs that share at least one rated item, and so contribute to `T_obs`/`T_boot`. |
| **n_params** | `integer` | number of free parameters in the rank-one form (one solving probability per rater, plus the shared chance constant). |
| **df** | `integer` | `n_valid_pairs - n_params`. A non-positive value means there are no more valid pairs than parameters, but the bootstrap p-value is still valid regardless, since the rank-one equations are nonlinear. |
| **p_value** | `numeric` | fraction of `T_boot` at least as large as `T_obs` - a small value means the observed agreement is not well explained by a single solving probability per rater. |
| **raters** | `character` | rater identifiers in the order used to build the underlying agreement matrix (system rater first, then the rest). |

## References

Rohe, K., Krauska, A. N., Collins, G., Higgins, J., & Pustejovsky, J.
(2026). The solve or guess model: Validating automated systems against
heterogeneous human raters. Working paper.

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- solve_or_guess(evaluations, system_rater_id = "gpt", B = 200)
rank_one_diagnostic(fit, B = 200)
} # }
```
