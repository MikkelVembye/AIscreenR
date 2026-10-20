# Approximate price estimation for title and abstract screening using OpenAI's GPT API models

**\[experimental\]**  
  
This function supports the approximation of the price of title and
abstract screenings when using OpenAI's GPT API models. The function
only provide approximately accurate price estimates. When detailed
descriptions are used, this will increase the completion tokens with an
unknown amount.

## Usage

``` r
approximate_price_gpt(
  data,
  prompt,
  studyid,
  title,
  abstract,
  model = "gpt-6-luna",
  reps = 1,
  token_word_ratio = 1.6,
  reasoning_effort = "medium"
)
```

## Arguments

- data:

  Dataset containing the titles and abstracts.

- prompt:

  Prompt(s) to be added before the title and abstract.

- studyid:

  Unique Study ID. If missing, this is generated automatically.

- title:

  Name of the variable containing the title information.

- abstract:

  Name of variable containing the abstract information.

- model:

  Character string with the name of the completion model. Can take
  multiple models. Default = `"gpt-6-luna"`. Be aware that gpt-4 and
  gpt-3.5 models are deprecated and can no longer be used. Find
  available model at <https://developers.openai.com/api/docs/models>.

- reps:

  Numerical value indicating the number of times the same question
  should be sent to the GPT server. This can be useful to test
  consistency between answers. Default is `1` but when using cheaper
  models, we recommend setting this value to `10`.

- token_word_ratio:

  The multiplier used to approximate the number of tokens per word.
  Default is `1.6` which we empirically have found to be the average
  number of tokens per word.

- reasoning_effort:

  Character string indicating the level of reasoning effort required for
  the task. Default is `"medium"`. Can take the values `"low"`,
  `"medium"`, and `"high"`. Only applicable for gpt models. Used to
  approximate the reasoning tokens. The approximation is based on
  measured reasoning token use with the default verbosity (`"low"`).

## Value

An object of class `"gpt_price"`. The object is a list containing the
following components:

- price:

  numerical value indicating the total approximate price (in USD) of the
  screening across all gpt-models expected to be used for the screening.

- price_data:

  dataset with prices across all gpt models expected to be used for
  screening.

## Examples

``` r
prompt <- "This is a prompt"

app_price <- approximate_price_gpt(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-6-luna", "gpt-5-nano"),
  reps = c(10, 1)
)

app_price
#> The approximate price of the (simple) screening will be around $0.0024.
app_price$price_dollar
#> [1] 0.0024
app_price$price_data
#> # A tibble: 2 × 8
#>   promptid model      iterations prompt   completion_tokens input_price_dollar
#>   <chr>    <chr>           <dbl> <chr>                <dbl>              <dbl>
#> 1 Prompt 1 gpt-6-luna         10 Prompt 1              544.             0.0014
#> 2 Prompt 1 gpt-5-nano          1 Prompt 1             1806.             0     
#> # ℹ 2 more variables: output_price_dollar <dbl>, total_price_dollar <dbl>
```
