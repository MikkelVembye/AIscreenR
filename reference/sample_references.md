# Random sample references

`sample_references`samples n rows from the dataset with titles and
abstracts. Implements the target set sampling algorithm from Hou &
Tipton (2024) when `n` is NULL and `relevant_col` is provided, providing
a formal reliability guarantee. Defined as the probability of achieving
`c_target` recall is at least `1 - c_target^k`.

The algorithm uses with replacement. This means that relevant records
are returned to the pool after selection, making draws independent
without needing to know the total number of relevant records (L). See
[Hou & Tipton (2024)](https://doi.org/10.1002/jrsm.1690) for details.

## Usage

``` r
sample_references(
  data,
  n = NULL,
  relevant_col = NULL,
  c_target = NULL,
  R_c = NULL,
  with_replacement = FALSE,
  id_col = "record_id",
  prob_vec = NULL,
  seed = 123
)
```

## Arguments

- data:

  Dataset containing records with relevance labels.

- n:

  Integer. Number of rows to sample using simple random sampling. If
  provided, `relevant_col`, `c_target`, and `R_c` are ignored and simple
  random sampling is used directly. If NULL and `relevant_col` is
  provided, k (the target set size) is computed from `c_target` and
  `R_c`.

- relevant_col:

  Character string naming the binary relevance column (1 = relevant).
  Can also be a character vector of multiple column names, in which case
  a record is treated as relevant only if *all* of the named columns
  equal 1 (e.g. `human_code == 1` and `decision_binary == 1`). Only used
  when `n` is NULL; otherwise falls back to simple random sampling
  behaviour.

- c_target:

  Numeric in (0,1). Desired recall level (e.g. 0.95). Used to compute k.

- R_c:

  Numeric in (0,1). Desired reliability, i.e. probability of achieving
  `c_target` recall (e.g. 0.90). Used to compute k.

- with_replacement:

  Logical. Whether to sample with replacement. Default TRUE. Only used
  in simple random sampling.

- id_col:

  Character string naming the record ID column. Default `"record_id"`.

- prob_vec:

  Vector of probability weights. Only used in simple random sampling.
  Default is a uniform vector of 1/n.

- seed:

  Integer. Random seed for reproducibility. Default is 123.

## Value

When `n` is NULL and `relevant_col` is provided, a list with:

- target_set:

  Data frame of k target records

- target_ids:

  Vector of record IDs in the target set

- k:

  Target set size

- c_target:

  Desired recall level

- R_c:

  Desired reliability

- reliability_guarantee:

  Lower bound on probability of achieving c_target recall: 1 -
  c_target^k

Otherwise (i.e. whenever `n` is provided), returns a data frame of n
rows (original, simple random sampling behaviour).

## References

Hou, Z., & Tipton, E. (2024). Enhancing recall in automated record
screening: A resampling algorithm. *Research Synthesis Methods, 15*(3),
372-383. [doi:10.1002/jrsm.1690](https://doi.org/10.1002/jrsm.1690)

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2025). Generative Pretrained Transformer Models Can Function as Highly
Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
A Proof of Concept and Common Guidelines. *Psychological Methods*.
[doi:10.1037/met0000769](https://doi.org/10.1037/met0000769)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compute k from c_target and R_c:
target_studies <- sample_references(
  data = combined_data,
  relevant_col = "decision_binary",
  c_target = 0.95,
  R_c = 0.90
)

# Relevant only if both the human coder and the model flagged the record:
target_studies <- sample_references(
  data = combined_data,
  relevant_col = c("human_code", "decision_binary"),
  c_target = 0.95,
  R_c = 0.90
)

# Simple random sampling:
excl_test_dat <- filges2015_dat[1:200, ] |> sample_references(100)
} # }
```
