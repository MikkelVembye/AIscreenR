# Groq model prices (last updated March 18, 2026)

Data set containing input and output prizes for all GROQ's API models.

## Usage

``` r
groq_model_prizes
```

## Format

A `data.frame` containing 4 rows/models and 3 variables/columns

|                         |             |                                       |
|-------------------------|-------------|---------------------------------------|
| **model**               | `character` | indicating the specific GPT model     |
| **price_in_per_token**  | `character` | indicating the input prize per token  |
| **price_out_per_token** | `character` | indicating the output prize per token |

## References

GROQ. *Pricing*. <https://groq.com/pricing>
