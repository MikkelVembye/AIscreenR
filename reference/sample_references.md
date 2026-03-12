# Random sample references

`sample_references`samples n rows from the dataset with titles and
abstracts either with or without replacement. This function is supposed
to support the construct of a test dataset, as suggested by [Vembye et
al. (2024)](https://osf.io/preprints/osf/yrhzm).

## Usage

``` r
sample_references(
  data,
  n,
  with_replacement = FALSE,
  prob_vec = rep(1/n, nrow(data))
)
```

## Arguments

- data:

  Dataset containing the titles and abstracts wanted to be screened.

- n:

  A non-negative integer giving the number of rows to choose.

- with_replacement:

  Logical indicating if sampling should be done with of without
  replacement. Default is `FALSE`.

- prob_vec:

  'A vector of probability weights for obtaining the elements of the
  vector being sampled.' Default is a vector of 1/n.

## Value

A dataset with n rows.

## References

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2024) *GPT API Models Can Function as Highly Reliable Second Screeners
of Titles and Abstracts in Systematic Reviews: A Proof of Concept and
Common Guidelines* <https://osf.io/preprints/osf/yrhzm>

## Examples

``` r
excl_test_dat <- filges2015_dat[1:200,] |> sample_references(100)
```
