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
  model = "gpt-4o-mini",
  reps = 1,
  top_p = 1,
  token_word_ratio = 1.6
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
  multiple models, including gpt-4 models. Default = `"gpt-4o-mini"`.
  Find available model at
  <https://platform.openai.com/docs/models/model-endpoint-compatibility>.

- reps:

  Numerical value indicating the number of times the same question
  should be sent to the GPT server. This can be useful to test
  consistency between answers. Default is `1` but when using
  gpt-3.5-turbo or gpt-4o-mini models, we recommend setting this value
  to `10`.

- top_p:

  'An alternative to sampling with temperature, called nucleus sampling,
  where the model considers the results of the tokens with top_p
  probability mass. So 0.1 means only the tokens comprising the top 10%
  probability mass are considered. We generally recommend altering this
  or temperature but not both.' (OPEN-AI). Default is 1. Find
  documentation at
  <https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p>.

- token_word_ratio:

  The multiplier used to approximate the number of tokens per word.
  Default is `1.6` which we empirically have found to be the average
  number of tokens per word.

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
  model = c("gpt-4o-mini", "gpt-4"),
  reps = c(10, 1)
)

app_price
#> The approximate price of the (simple) screening will be around $0.0469.
app_price$price_dollar
#> [1] 0.0469
app_price$price_data
#> # A tibble: 2 × 6
#>   prompt   model       iterations input_price_dollar output_price_dollar
#>   <chr>    <chr>            <dbl>              <dbl>               <dbl>
#> 1 Prompt 1 gpt-4o-mini         10             0.0022           0.0000846
#> 2 Prompt 1 gpt-4                1             0.0438           0.000846 
#> # ℹ 1 more variable: total_price_dollar <dbl>
```
