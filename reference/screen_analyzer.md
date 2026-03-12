# Analyze performance between the human and AI screening.

**\[stable\]**  
  
When both the human and AI title and abstract screening has been done,
this function allows you to calculate performance measures of the
screening, including the overall accuracy, specificity/recall, and
sensitivity of the screening, as well as inter-rater reliability kappa
statistics (Gartlehner et al., 2019; McHugh, 2012; Syriani et al.,
2024).

## Usage

``` r
screen_analyzer(x, human_decision = human_code, key_result = TRUE)
```

## Arguments

- x:

  An object of either class`'gpt'` or `'chatgpt'` or a dataset of either
  class `'gpt_tbl'`, `'chatgpt_tbl'`, or `'gpt_agg_tbl'`

- human_decision:

  Indicate the variable in the data that contains the human_decision.
  This variable must be numeric, containing 1 (for included references)
  and 0 (for excluded references) only.

- key_result:

  Logical indicating if only the raw agreement, recall, and specificity
  measures should be returned. Default is `TRUE`.

## Value

A `tibble` with screening performance measures. The `tibble` includes
the following variables:

|                        |             |                                                                                                                                  |
|------------------------|-------------|----------------------------------------------------------------------------------------------------------------------------------|
| **promptid**           | `integer`   | indicating the prompt ID.                                                                                                        |
| **model**              | `character` | indicating the specific gpt-model used.                                                                                          |
| **reps**               | `integer`   | indicating the number of times the same question was sent to GPT server.                                                         |
| **top_p**              | `numeric`   | indicating the applied top_p.                                                                                                    |
| **n_screened**         | `integer`   | indicating the number of screened references.                                                                                    |
| **n_missing**          | `numeric`   | indicating the number of missing responses.                                                                                      |
| **n_refs**             | `integer`   | indicating the total number of references expected to be screened for the given condition.                                       |
| **human_in_gpt_ex**    | `numeric`   | indicating the number of references included by humans and excluded by gpt.                                                      |
| **human_ex_gpt_in**    | `numeric`   | indicating the number of references excluded by humans and included by gpt.                                                      |
| **human_in_gpt_in**    | `numeric`   | indicating the number of references included by humans and included by gpt.                                                      |
| **human_ex_gpt_ex**    | `numeric`   | indicating the number of references excluded by humans and excluded by gpt.                                                      |
| **accuracy**           | `numeric`   | indicating the overall percent disagreement between human and gpt (Gartlehner et al., 2019).                                     |
| **p_agreement**        | `numeric`   | indicating the overall percent agreement between human and gpt.                                                                  |
| **precision**          | `numeric`   | "measures the ability to include only articles that should be included" (Syriani et al., 2023).                                  |
| **recall**             | `numeric`   | "measures the ability to include all articles that should be included" (Syriani et al., 2023).                                   |
| **npv**                | `numeric`   | Negative predictive value (NPV) "measures the ability to exclude only articles that should be excluded" (Syriani et al., 2023).  |
| **specificity**        | `numeric`   | "measures the ability to exclude all articles that should be excluded" (Syriani et al., 2023).                                   |
| **bacc**               | `numeric`   | "capture the accuracy of deciding both inclusion and exclusion classes" (Syriani et al., 2023).                                  |
| **F2**                 | `numeric`   | F-measure that "consider the cost of getting false negatives twice as costly as getting false positives" (Syriani et al., 2023). |
| **mcc**                | `numeric`   | indicating percent agreement for excluded references (Gartlehner et al., 2019).                                                  |
| **irr**                | `numeric`   | indicating the inter-rater reliability as described in McHugh (2012).                                                            |
| **se_irr**             | `numeric`   | indicating standard error for the inter-rater reliability.                                                                       |
| **cl_irr**             | `numeric`   | indicating lower confidence interval for the inter-rater reliability.                                                            |
| **cu_irr**             | `numeric`   | indicating upper confidence interval for the inter-rater reliability.                                                            |
| **level_of_agreement** | `character` | interpretation of the inter-rater reliability as suggested by McHugh (2012).                                                     |

## References

Gartlehner, G., Wagner, G., Lux, L., Affengruber, L., Dobrescu, A.,
Kaminski-Hartenthaler, A., & Viswanathan, M. (2019). Assessing the
accuracy of machine-assisted abstract screening with DistillerAI: a user
study. *Systematic Reviews*, 8:277, 1-10.
[doi:10.1186/s13643-019-1221-3](https://doi.org/10.1186/s13643-019-1221-3)

McHugh, M. L. (2012). Interrater reliability: The kappa statistic.
*Biochemia Medica*, 22(3), 276-282.
<https://pubmed.ncbi.nlm.nih.gov/23092060/>

Syriani, E., David, I., & Kumar, G. (2023). Assessing the Ability of
ChatGPT to Screen Articles for Systematic Reviews. *ArXiv Preprint
ArXiv:2307.06464*.

## Examples

``` r
if (FALSE) { # \dontrun{

library(future)

set_api_key()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

plan(multisession)

res <- tabscreen_gpt(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract
  )

plan(sequential)

res |> screen_analyzer()

} # }
```
