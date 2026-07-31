# Model prize data (last updated July 31, 2026)

Dataset mainly containing input and output prizes for all OpenAI's GPT
API models.

## Usage

``` r
model_prizes
```

## Format

A `data.frame` containing 42 rows/models and 3 variables/columns

|                         |             |                                       |
|-------------------------|-------------|---------------------------------------|
| **model**               | `character` | indicating the specific GPT model     |
| **price_in_per_token**  | `character` | indicating the input prize per token  |
| **price_out_per_token** | `character` | indicating the output prize per token |

## References

OpenAI. *Pricing*. <https://developers.openai.com/api/docs/pricing>
