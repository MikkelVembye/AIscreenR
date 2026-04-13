# Title and abstract screening with GPT API models using function calls via the original function call arguments

**\[deprecated\]**  
  

This function has been deprecated (but can still be used) because OpenAI
has deprecated the function_call and and functions argument which is
used in this function. Instead use the
[`tabscreen_gpt.tools()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
that handles the function calling via the tools and tool_choice
arguments.  

This function supports the conduct of title and abstract screening with
GPT API models in R. This function only works with GPT-4, more
specifically gpt-4-0613. To draw on other models, use
[`tabscreen_gpt.tools()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md).
The function allows to run title and abstract screening across multiple
prompts and with repeated questions to check for consistency across
answers. This function draws on the newly developed function calling to
better steer the output of the responses. This function was used in
[Vembye, Christensen, Mølgaard, and Schytt.
(2025)](https://osf.io/preprints/osf/yrhzm).

## Usage

``` r
tabscreen_gpt.original(
  data,
  prompt,
  studyid,
  title,
  abstract,
  ...,
  model = "gpt-4",
  role = "user",
  functions = incl_function_simple,
  function_call_name = list(name = "inclusion_decision_simple"),
  top_p = 1,
  time_info = TRUE,
  token_info = TRUE,
  api_key = get_api_key(),
  max_tries = 16,
  max_seconds = NULL,
  is_transient = gpt_is_transient,
  backoff = NULL,
  after = NULL,
  rpm = 10000,
  reps = 1,
  seed_par = NULL,
  progress = TRUE,
  messages = TRUE,
  incl_cutoff_upper = 0.5,
  incl_cutoff_lower = incl_cutoff_upper - 0.1,
  force = FALSE
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

- ...:

  Further argument to pass to the request body. See
  <https://developers.openai.com/api/reference/resources/chat>.

- model:

  Character string with the name of the completion model. Can take
  multiple models, including gpt-4 models. Default = `"gpt-4"` (i.e.,
  gpt-4-0613). This model has been shown to outperform the gpt-3.5-turbo
  models in terms of its ability to detect relevant studies (Vembye et
  al., Under preparation). Find available model at
  <https://developers.openai.com/api/docs/models/model-endpoint-compatibility>.

- role:

  Character string indicate the role of the user. Default is `"user"`.

- functions:

  Function to steer output. Default is `incl_function_simple`. To get
  detailed responses use the hidden function call `incl_function` from
  the package. Also see 'Examples below. Find further documentation for
  function calling at
  <https://openai.com/blog/function-calling-and-other-api-updates>.

- function_call_name:

  Functions to call. Default is
  `list(name = "inclusion_decision_simple")`. To get detailed responses
  use `list(name = "inclusion_decision")`. Also see 'Examples below.

- top_p:

  'An alternative to sampling with temperature, called nucleus sampling,
  where the model considers the results of the tokens with top_p
  probability mass. So 0.1 means only the tokens comprising the top 10%
  probability mass are considered. We generally recommend altering this
  or temperature but not both.' (OPEN-AI). Default is 1. Find
  documentation at
  <https://developers.openai.com/api/reference/resources/chat#chat/create-top_p>.

- time_info:

  Logical indicating whether the run time of each request/question
  should be included in the data. Default = `TRUE`.

- token_info:

  Logical indicating whether the number of prompt and completion tokens
  per request should be included in the output data. Default = `TRUE`.
  When `TRUE`, the output object will include price information of the
  conducted screening.

- api_key:

  Numerical value with your personal API key. Find at
  <https://platform.openai.com/account/api-keys>. Use
  [`httr2::secret_make_key()`](https://httr2.r-lib.org/reference/secrets.html),
  [`httr2::secret_encrypt()`](https://httr2.r-lib.org/reference/secrets.html),
  and
  [`httr2::secret_decrypt()`](https://httr2.r-lib.org/reference/secrets.html)
  to scramble and decrypt the api key and use
  [`set_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md)
  to securely automate the use of the api key by setting the api key as
  a locale environment variable.

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry' (Wickham, 2023).

- is_transient:

  'A predicate function that takes a single argument (the response) and
  returns `TRUE` or `FALSE` specifying whether or not the response
  represents a transient error' (Wickham, 2023).

- backoff:

  'A function that takes a single argument (the number of failed
  attempts so far) and returns the number of seconds to wait' (Wickham,
  2023).

- after:

  'A function that takes a single argument (the response) and returns
  either a number of seconds to wait or `NULL`, which indicates that a
  precise wait time is not available that the `backoff` strategy should
  be used instead' (Wickham, 2023).

- rpm:

  Numerical value indicating the number of requests per minute (rpm)
  available for the specified api key. Find more information at
  <https://developers.openai.com/api/docs/models/model-endpoint-compatibility>.
  Alternatively, use
  [`rate_limits_per_minute()`](https://mikkelvembye.github.io/AIscreenR/reference/rate_limits_per_minute.md).

- reps:

  Numerical value indicating the number of times the same question
  should be sent to OpenAI's GPT API models. This can be useful to test
  consistency between answers. Default is `1` but when using 3.5 models,
  we recommend setting this value to `10`.

- seed_par:

  Numerical value for a seed to ensure that proper, parallel-safe random
  numbers are produced.

- progress:

  Logical indicating whether a progress line should be shown when
  running the title and abstract screening in parallel. Default is
  `TRUE`.

- messages:

  Logical indicating whether to print messages embedded in the function.
  Default is `TRUE`.

- incl_cutoff_upper:

  Numerical value indicating the probability threshold for which a
  studies should be included. Default is 0.5, which indicates that
  titles and abstracts that OpenAI's GPT API model has included more
  than 50 percent of the times should be included.

- incl_cutoff_lower:

  Numerical value indicating the probability threshold above which
  studies should be check by a human. Default is 0.4, which means that
  if you ask OpenAI's GPT API model the same questions 10 times and it
  includes the title and abstract 4 times, we suggest that the study
  should be check by a human.

- force:

  Logical argument indicating whether to force the function to use more
  than 10 iterations for gpt-3.5 models and more than 1 iteration for
  gpt-4 models. This argument is developed to avoid the conduct of wrong
  and extreme sized screening. Default is `FALSE`.

## Value

An object of class `"chatgpt"`. The object is a list containing the
following components:

- answer_data_sum:

  dataset with the summarized, probabilistic inclusion decision for each
  title and abstract across multiple repeated questions.

- answer_data_all:

  dataset with all individual answers.

- price:

  numerical value indicating the total price (in USD) of the screening.

- price_data:

  dataset with prices across all gpt models used for screening.

## Note

The `answer_data_sum` data contains the following mandatory variables:

|                            |             |                                                                                                                                                                                                                                     |
|----------------------------|-------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **studyid**                | `integer`   | indicating the study ID of the reference.                                                                                                                                                                                           |
| **title**                  | `character` | indicating the title of the reference.                                                                                                                                                                                              |
| **abstract**               | `character` | indicating the abstract of the reference.                                                                                                                                                                                           |
| **promptid**               | `integer`   | indicating the prompt ID.                                                                                                                                                                                                           |
| **prompt**                 | `character` | indicating the prompt.                                                                                                                                                                                                              |
| **model**                  | `character` | indicating the specific gpt-model used.                                                                                                                                                                                             |
| **question**               | `character` | indicating the final question sent to OpenAI's GPT API models.                                                                                                                                                                      |
| **top_p**                  | `numeric`   | indicating the applied top_p.                                                                                                                                                                                                       |
| **incl_p**                 | `numeric`   | indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract.                                                                                                               |
| **final_decision_gpt**     | `character` | indicating the final decision reached by gpt - either 'Include', 'Exclude', or 'Check'.                                                                                                                                             |
| **final_decision_gpt_num** | `integer`   | indicating the final numeric decision reached by gpt - either 1 or 0.                                                                                                                                                               |
| **longest_answer**         | `character` | indicating the longest gpt response obtained across multiple repeated responses on the same title and abstract. Only included if the detailed function calling function is used. See 'Examples' below for how to use this function. |
| **reps**                   | `integer`   | indicating the number of times the same question has been sent to OpenAI's GPT API models.                                                                                                                                          |
| **n_mis_answers**          | `integer`   | indicating the number of missing responses.                                                                                                                                                                                         |

  
The `answer_data_all` data contains the following mandatory variables:

|                          |             |                                                                                                                                                                                                            |
|--------------------------|-------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **studyid**              | `integer`   | indicating the study ID of the reference.                                                                                                                                                                  |
| **title**                | `character` | indicating the title of the reference.                                                                                                                                                                     |
| **abstract**             | `character` | indicating the abstract of the reference.                                                                                                                                                                  |
| **promptid**             | `integer`   | indicating the prompt ID.                                                                                                                                                                                  |
| **prompt**               | `character` | indicating the prompt.                                                                                                                                                                                     |
| **model**                | `character` | indicating the specific gpt-model used.                                                                                                                                                                    |
| **iterations**           | `numeric`   | indicating the number of times the same question has been sent to OpenAI's GPT API models.                                                                                                                 |
| **question**             | `character` | indicating the final question sent to OpenAI's GPT API models.                                                                                                                                             |
| **top_p**                | `numeric`   | indicating the applied top_p.                                                                                                                                                                              |
| **decision_gpt**         | `character` | indicating the raw gpt decision - either `"1", "0", "1.1"` for inclusion, exclusion, or uncertainty, respectively.                                                                                         |
| **detailed_description** | `character` | indicating detailed description of the given decision made by OpenAI's GPT API models. Only included if the detailed function calling function is used. See 'Examples' below for how to use this function. |
| **decision_binary**      | `integer`   | indicating the binary gpt decision, that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case.                                                                           |
| **prompt_tokens**        | `integer`   | indicating the number of prompt tokens sent to the server for the given request.                                                                                                                           |
| **completion_tokens**    | `integer`   | indicating the number of completion tokens sent to the server for the given request.                                                                                                                       |
| **run_time**             | `numeric`   | indicating the time it took to obtain a response from the server for the given request.                                                                                                                    |
| **n**                    | `integer`   | indicating request ID.                                                                                                                                                                                     |

  
If any requests failed to reach the server, the `chatgpt` object
contains an error data set (`error_data`) having the same variables as
`answer_data_all` but with failed request references only.  

The `price_data` data contains the following variables:

|                         |             |                                                                         |
|-------------------------|-------------|-------------------------------------------------------------------------|
| **model**               | `character` | gpt model.                                                              |
| **input_price_dollar**  | `integer`   | price for all prompt/input tokens for the correspondent gpt-model.      |
| **output_price_dollar** | `integer`   | price for all completion/output tokens for the correspondent gpt-model. |
| **price_total_dollar**  | `integer`   | total price for all tokens for the correspondent gpt-model.             |

Find current token pricing at <https://openai.com/pricing>.

## References

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2025) *GPT API Models Can Function as Highly Reliable Second Screeners
of Titles and Abstracts in Systematic Reviews: A Proof of Concept and
Common Guidelines* <https://osf.io/preprints/osf/yrhzm>

Wickham H (2023). *httr2: Perform HTTP Requests and Process the
Responses*. https://httr2.r-lib.org, https://github.com/r-lib/httr2.

## Examples

``` r
if (FALSE) { # \dontrun{

set_api_key()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

tabscreen_gpt.original(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  max_tries = 2
  )

 # Get detailed descriptions of the gpt decisions by using the
 # embedded function calling functions from the package. See example below.
 tabscreen_gpt.original(
   data = filges2015_dat[1:2,],
   prompt = prompt,
   studyid = studyid,
   title = title,
   abstract = abstract,
   functions = incl_function,
   function_call_name = list(name = "inclusion_decision"),
   max_tries = 2
 )
} # }
```
