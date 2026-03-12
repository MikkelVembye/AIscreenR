# Title and abstract screening with GPT API models using function calls via the tools argument

**\[stable\]**  
  
This function supports the conduct of title and abstract screening with
GPT API models in R. Specifically, it allows the user to draw on
GPT-3.5, GPT-4, GPT-4o, GPT-4o-mini, and fine-tuned models. The function
allows to run title and abstract screening across multiple prompts and
with repeated questions to check for consistency across answers. All of
which can be done in parallel. The function draws on the newly developed
function calling which is called via the tools argument in the request
body. This is the main different between `tabscreen_gpt.tools()` and
[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md).
Function calls ensure more reliable and consistent responses to ones
requests. See [Vembye, Christensen, Mølgaard, and Schytt.
(2025)](https://osf.io/preprints/osf/yrhzm) for guidance on how
adequately to conduct title and abstract screening with GPT models.

## Usage

``` r
tabscreen_gpt.tools(data, prompt, studyid, title, abstract,
   model = "gpt-4o-mini", role = "user", tools = NULL, tool_choice = NULL, top_p = 1,
   time_info = TRUE, token_info = TRUE, api_key = get_api_key(), max_tries = 16,
   max_seconds = NULL, is_transient = gpt_is_transient, backoff = NULL,
   after = NULL, rpm = 10000, reps = 1, seed_par = NULL, progress = TRUE,
   decision_description = FALSE, messages = TRUE, incl_cutoff_upper = NULL,
   incl_cutoff_lower = NULL, force = FALSE, fine_tuned = FALSE, ...)

tabscreen_gpt(data, prompt, studyid, title, abstract,
   model = "gpt-4o-mini", role = "user", tools = NULL, tool_choice = NULL, top_p = 1,
   time_info = TRUE, token_info = TRUE, api_key = get_api_key(), max_tries = 16,
   max_seconds = NULL, is_transient = gpt_is_transient, backoff = NULL,
   after = NULL, rpm = 10000, reps = 1, seed_par = NULL, progress = TRUE,
   decision_description = FALSE, messages = TRUE, incl_cutoff_upper = NULL,
   incl_cutoff_lower = NULL, force = FALSE, fine_tuned = FALSE, ...)
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
  multiple models. Default is the latest `"gpt-4o-mini"`. Find available
  model at
  <https://platform.openai.com/docs/models/model-endpoint-compatibility>.

- role:

  Character string indicating the role of the user. Default is `"user"`.

- tools:

  This argument allows this user to apply customized functions. See
  <https://platform.openai.com/docs/api-reference/chat/create#chat-create-tools>.
  Default is `NULL`. If not specified the default function calls from
  `AIscreenR` are used.

- tool_choice:

  If a customized function is provided this argument 'controls which (if
  any) tool is called by the model' (OpenAI). Default is `NULL`. If set
  to `NULL` when using a customized function, the default is `"auto"`.
  See
  <https://platform.openai.com/docs/api-reference/chat/create#chat-create-tool_choice>.

- top_p:

  'An alternative to sampling with temperature, called nucleus sampling,
  where the model considers the results of the tokens with top_p
  probability mass. So 0.1 means only the tokens comprising the top 10%
  probability mass are considered. We generally recommend altering this
  or temperature but not both.' (OpenAI). Default is 1. Find
  documentation at
  <https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p>.

- time_info:

  Logical indicating whether the run time of each request/question
  should be included in the data. Default is `TRUE`.

- token_info:

  Logical indicating whether token information should be included in the
  output data. Default is `TRUE`. When `TRUE`, the output object will
  include price information of the conducted screening.

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

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry' (Wickham, 2023). The default of `max_tries` is 16.

- is_transient:

  'A predicate function that takes a single argument (the response) and
  returns `TRUE` or `FALSE` specifying whether or not the response
  represents a transient error' (Wickham, 2023). This function runs
  automatically in the AIscreenR but can be customized by the user if
  necessary.

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
  available for the specified model. Find more information at
  <https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api>.
  Alternatively, use
  [`rate_limits_per_minute()`](https://mikkelvembye.github.io/AIscreenR/reference/rate_limits_per_minute.md).

- reps:

  Numerical value indicating the number of times the same question
  should be send to the server. This can be useful to test consistency
  between answers, and/or can be used to make inclusion judgments based
  on how many times a study has been included across a the given number
  of screenings. Default is `1` but when using gpt-3.5-turbo models or
  gpt-4o-mini, we recommend setting this value to `10` to catch model
  uncertainty.

- seed_par:

  Numerical value for a seed to ensure that proper, parallel-safe random
  numbers are produced.

- progress:

  Logical indicating whether a progress line should be shown when
  running the title and abstract screening in parallel. Default is
  `TRUE`.

- decision_description:

  Logical indicating whether a detailed description should follow the
  decision made by GPT. Default is `FALSE`. When conducting large-scale
  screening, we generally recommend not using this feature as it will
  substantially increase the cost of the screening. We generally
  recommend using it when encountering disagreements between GPT and
  human decisions.

- messages:

  Logical indicating whether to print messages embedded in the function.
  Default is `TRUE`.

- incl_cutoff_upper:

  Numerical value indicating the probability threshold for which a
  studies should be included. ONLY relevant when the same questions is
  requested multiple times (i.e., when any reps \> 1). Default is 0.5,
  indicating that titles and abstracts should only be included if GPT
  has included the study more than 50 percent of the times.

- incl_cutoff_lower:

  Numerical value indicating the probability threshold above which
  studies should be check by a human. ONLY relevant when the same
  questions is requested multiple times (i.e., when any reps \> 1).
  Default is 0.4, meaning that if you ask GPT the same questions 10
  times and it includes the title and abstract 4 times, we suggest that
  the study should be check by a human.

- force:

  Logical argument indicating whether to force the function to use more
  than 10 iterations for gpt-3.5 models and more than 1 iteration for
  gpt-4 models other than gpt-4o-mini. This argument is developed to
  avoid the conduct of wrong and extreme sized screening. Default is
  `FALSE`.

- fine_tuned:

  Logical indicating whether a fine-tuned model is used. Default is
  `FALSE`.

- ...:

  Further argument to pass to the request body. See
  <https://platform.openai.com/docs/api-reference/chat/create>.

## Value

An object of class `'gpt'`. The object is a list containing the
following datasets and components:

- answer_data:

  dataset of class `'gpt_tbl'` with all individual answers.

- price_dollar:

  numerical value indicating the total price (in USD) of the screening.

- price_data:

  dataset with prices across all gpt models used for screening.

- run_date:

  string indicating the date when the screening was ran. In some
  frameworks, time details are considered important to report (see e.g.,
  Thomas et al., 2024).

- ...:

  some additional attributed values/components, including an attributed
  list with the arguments used in the function. These are used in
  [`screen_errors()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.md)
  to re-screen transient errors.

If the same question is requested multiple times, the object will also
contain the following dataset with results aggregated across the
iterated requests/questions.

- answer_data_aggregated:

  dataset of class `'gpt_agg_tbl'` with the summarized, probabilistic
  inclusion decision for each title and abstract across multiple
  repeated questions.

## Note

The `answer_data` data contains the following *mandatory* variables:

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
| **detailed_description** | `character` | indicating detailed description of the given decision made by OpenAI's GPT API models. ONLY included if the detailed function calling function is used. See 'Examples' below for how to use this function. |
| **decision_binary**      | `integer`   | indicating the binary gpt decision, that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case.                                                                           |
| **prompt_tokens**        | `integer`   | indicating the number of prompt tokens sent to the server for the given request.                                                                                                                           |
| **completion_tokens**    | `integer`   | indicating the number of completion tokens sent to the server for the given request.                                                                                                                       |
| **submodel**             | `character` | indicating the exact (sub)model used for screening.                                                                                                                                                        |
| **run_time**             | `numeric`   | indicating the time it took to obtain a response from the server for the given request.                                                                                                                    |
| **run_date**             | `character` | indicating the date the given response was received.                                                                                                                                                       |
| **n**                    | `integer`   | indicating iteration ID. Is only different from 1, when `reps > 1`.                                                                                                                                        |

  
If any requests failed, the `gpt` object contains an error dataset
(`error_data`) containing the same variables as `answer_data` but with
failed request references only.  

When the same question is requested multiple times, the
`answer_data_aggregated` data contains the following *mandatory*
variables:

|                            |             |                                                                                                                                                                                                                      |
|----------------------------|-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **studyid**                | `integer`   | indicating the study ID of the reference.                                                                                                                                                                            |
| **title**                  | `character` | indicating the title of the reference.                                                                                                                                                                               |
| **abstract**               | `character` | indicating the abstract of the reference.                                                                                                                                                                            |
| **promptid**               | `integer`   | indicating the prompt ID.                                                                                                                                                                                            |
| **prompt**                 | `character` | indicating the prompt.                                                                                                                                                                                               |
| **model**                  | `character` | indicating the specific gpt-model used.                                                                                                                                                                              |
| **question**               | `character` | indicating the final question sent to OpenAI's GPT API models.                                                                                                                                                       |
| **top_p**                  | `numeric`   | indicating the applied top_p.                                                                                                                                                                                        |
| **incl_p**                 | `numeric`   | indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract.                                                                                                |
| **final_decision_gpt**     | `character` | indicating the final decision reached by gpt - either 'Include', 'Exclude', or 'Check'.                                                                                                                              |
| **final_decision_gpt_num** | `integer`   | indicating the final numeric decision reached by gpt - either 1 or 0.                                                                                                                                                |
| **longest_answer**         | `character` | indicating the longest gpt response obtained across multiple repeated responses on the same title and abstract. Only included when `decision_description = TRUE`. See 'Examples' below for how to use this function. |
| **reps**                   | `integer`   | indicating the number of times the same question has been sent to OpenAI's GPT API models.                                                                                                                           |
| **n_mis_answers**          | `integer`   | indicating the number of missing responses.                                                                                                                                                                          |
| **submodel**               | `character` | indicating the exact (sub)model used for screening.                                                                                                                                                                  |

  

The `price_data` data contains the following variables:

|                         |             |                                                                           |
|-------------------------|-------------|---------------------------------------------------------------------------|
| **prompt**              | `character` | if multiple prompts are used this variable indicates the given prompt-id. |
| **model**               | `character` | the specific gpt model used.                                              |
| **iterations**          | `integer`   | indicating the number of times the same question was requested.           |
| **input_price_dollar**  | `integer`   | price for all prompt/input tokens for the correspondent gpt-model.        |
| **output_price_dollar** | `integer`   | price for all completion/output tokens for the correspondent gpt-model.   |
| **total_price_dollar**  | `integer`   | total price for all tokens for the correspondent gpt-model.               |

Find current token pricing at <https://openai.com/pricing> or
[model_prizes](https://mikkelvembye.github.io/AIscreenR/reference/model_prizes.md).

## References

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2025) *GPT API Models Can Function as Highly Reliable Second Screeners
of Titles and Abstracts in Systematic Reviews: A Proof of Concept and
Common Guidelines* <https://osf.io/preprints/osf/yrhzm>

Thomas, J. et al. (2024). Responsible AI in Evidence SynthEsis (RAISE):
guidance and recommendations. <https://osf.io/cn7x4>

Wickham H (2023). *httr2: Perform HTTP Requests and Process the
Responses*. <https://httr2.r-lib.org>, <https://github.com/r-lib/httr2>.

## Examples

``` r
if (FALSE) { # \dontrun{

library(future)

set_api_key()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

plan(multisession)

tabscreen_gpt(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract
  )

plan(sequential)

 # Get detailed descriptions of the gpt decisions.

 plan(multisession)

 tabscreen_gpt(
   data = filges2015_dat[1:2,],
   prompt = prompt,
   studyid = studyid,
   title = title,
   abstract = abstract,
   decision_description = TRUE
 )

plan(sequential)

} # }
```
