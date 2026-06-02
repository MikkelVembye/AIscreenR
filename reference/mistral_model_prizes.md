# Mistral model prices (last updated May 7, 2026) Data set containing input and output prizes for all Mistral's API models.

Mistral model prices (last updated May 7, 2026) Data set containing
input and output prizes for all Mistral's API models.

## Usage

``` r
mistral_model_prizes
```

## Format

A `data.frame` containing 4 rows/models and 3 variables/columns

|                         |             |                                       |
|-------------------------|-------------|---------------------------------------|
| **model**               | `character` | indicating the specific GPT model     |
| **price_in_per_token**  | `character` | indicating the input prize per token  |
| **price_out_per_token** | `character` | indicating the output prize per token |

## References

Mistral. *Pricing*. <https://mistral.ai/pricing>
