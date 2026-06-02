# Find updated rate limits for API models

**\[stable\]**  
  
`rate_limits_per_minute` reports the rate limits for a given API model.
The function returns the available requests per minute (RPM) as well as
tokens per minute (TPM). Find general information at
<https://developers.openai.com/api/docs/models/model-endpoint-compatibility>.

## Usage

``` r
rate_limits_per_minute(
  model = "gpt-4o-mini",
  AI_tool = "OpenAI",
  api_key = NULL
)
```

## Arguments

- model:

  Character string with the name of the completion model. Default is
  `"gpt-4o-mini"`. Can take multiple values. For OpenAI models, find
  available models at
  <https://developers.openai.com/api/docs/models/model-endpoint-compatibility>.
  For Groq models, find available models at
  <https://console.groq.com/docs/models>. For Mistral models, find
  available models at <https://docs.mistral.ai/models/overview>.

- AI_tool:

  Character string specifying the AI tool from which the API is issued.
  Currently supports `"OpenAI"` (default), `"Groq"`, and `"Mistral"`.

- api_key:

  Character string with the API key. For OpenAI, use
  [`get_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md).
  For Groq, use
  [`get_api_key_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_groq.md).
  For Mistral, use
  [`get_api_key_mistral()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_mistral.md).

## Value

A `tibble` including variables with information about the model used,
the number of requests and tokens per minute.

## Examples

``` r
if (FALSE) { # \dontrun{
set_api_key()

rate_limits_per_minute(
  model = "gpt-4o-mini",
  AI_tool = "OpenAI",
  api_key = get_api_key()
)

# Groq example
rate_limits_per_minute(
  model = "llama3-70b-8192",
  AI_tool = "Groq",
  api_key = get_api_key_groq()
)

# Mistral example
rate_limits_per_minute(
 model = "mistral-small-latest",
 AI_tool = "Mistral",
 api_key = get_api_key_mistral()
)
} # }
```
