# RIS file data from Functional Family Therapy (FFT) systematic review

Bibliometric toy data from a systematic review regarding Functional
Family Therapy (FFT) for Young People in Treatment for Non-opioid Drug
Use (Filges et al., 2015). The data includes all 90 included and 180
excluded randomly sampled references from the literature search of the
systematic review.

## Usage

``` r
filges2015_dat
```

## Format

A `tibble` with 270 rows/studies and 6 variables/columns

|                |             |                                                                      |
|----------------|-------------|----------------------------------------------------------------------|
| **author**     | `character` | indicating the authors of the reference                              |
| **eppi_id**    | `character` | indicating a unique eppi-ID for each study                           |
| **studyid**    | `numeric`   | indicating a unique study-ID for each study                          |
| **title**      | `character` | with the title of the study                                          |
| **abstract**   | `character` | with the study abstract                                              |
| **human_code** | `numeric`   | indicating the human screening decision. 1 = included, 0 = excluded. |

## References

Filges, T., Andersen, D, & Jørgensen, A-M. K (2015). Functional Family
Therapy (FFT) for Young People in Treatment for Non-opioid Drug Use: A
Systematic Review *Campbell Systematic Reviews*,
[doi:10.4073/csr.2015.14](https://doi.org/10.4073/csr.2015.14)
