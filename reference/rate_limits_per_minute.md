# Find updated rate limits for API models

**\[stable\]**  
  
`rate_limits_per_minute` reports the rate limits for a given API model.
The function returns the available requests per minute (RPM) as well as
tokens per minute (TPM). Find general information at
<https://platform.openai.com/docs/guides/rate-limits/overview>.

## Usage

``` r
rate_limits_per_minute(
  model = "gpt-4o-mini",
  AI_tool = "gpt",
  api_key = get_api_key()
)
```

## Arguments

- model:

  Character string with the name of the completion model. Default is
  `"gpt-4o-mini"`. Can take multiple values. Find available model at
  <https://platform.openai.com/docs/models/model-endpoint-compatibility>.

- AI_tool:

  Character string specifying the AI tool from which the API is issued.
  Default is `"gpt"`.

- api_key:

  Numerical value with your personal API key. Default setting draws on
  the
  [`get_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md)
  to retrieve the API key from the R environment, so that the key is not
  compromised. The API key can be added to the R environment via
  [`set_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md)
  or by using
  [`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).
  In the `.Renviron` file, write `CHATGPT_KEY=INSERT_YOUR_KEY_HERE`.
  After entering the API key, close and save the `.Renviron` file and
  restart `RStudio` (ctrl + shift + F10). Alternatively, one can use
  [`httr2::secret_make_key()`](https://httr2.r-lib.org/reference/secrets.html),
  [`httr2::secret_encrypt()`](https://httr2.r-lib.org/reference/secrets.html),
  and
  [`httr2::secret_decrypt()`](https://httr2.r-lib.org/reference/secrets.html)
  to scramble and decrypt the API key.

## Value

A `tibble` including variables with information about the model used,
the number of requests and tokens per minute.

## Examples

``` r
if (FALSE) { # \dontrun{
set_api_key()

rate_limits_per_minute()
} # }
```
