# Title and abstract screening with Anthropic's API models

**\[stable\]**  
  
This function supports title and abstract screening using Anthropic's
API models. This function uses the function calling feature of
Anthropic's API models, which allows for more structured and accurate
responses from the model. The function follows the same general
structure as the other screening functions in the package, but with some
specific arguments and features that are tailored to Anthropic's API
models. See [Vembye, Christensen, Mølgaard, and Schytt.
(2025)](https://psycnet.apa.org/record/2026-37236-001) for guidance on
how adequately to conduct title and abstract screening with GPT models.

## Usage

``` r
tabscreen_claude(data, prompt, studyid, title, abstract,
  api_url = "https://api.anthropic.com", model = "claude-sonnet-4-6",
  role = "user", tools = NULL,
  time_info = TRUE, token_info = TRUE, api_key = get_api_key_anthropic(),
  max_tries = 16, max_tokens = 1024, max_seconds = NULL, 
  is_transient = gpt_is_transient, backoff = NULL,
  after = NULL, rpm = 10000, reps = 1, seed_par = NULL, progress = TRUE,
  decision_description = FALSE, messages = TRUE, incl_cutoff_upper = NULL,
  incl_cutoff_lower = NULL, force = FALSE, custom_model = FALSE,
  reasoning_effort = "medium", overinclusive = TRUE, ...)
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

- api_url:

  Character string with the endpoint URL for Anthropic's API. Default is
  `"https://api.anthropic.com"`.

- model:

  Character string with the name of the completion model. Can take
  multiple models. Default is the latest `"claude-sonnet-4-6"`. Find
  available model at
  <https://platform.claude.com/docs/en/about-claude/models/overview>.

- role:

  Character string indicating the role of the user. Default is `"user"`.

- tools:

  This argument allows this user to apply customized functions. See
  <https://platform.claude.com/docs/en/agents-and-tools/tool-use/overview>.
  Default is `NULL`. If not specified the default function calls from
  `AIscreenR` are used.

- time_info:

  Logical indicating whether the run time of each request/question
  should be included in the data. Default is `TRUE`.

- token_info:

  Logical indicating whether token information should be included in the
  output data. Default is `TRUE`. When `TRUE`, the output object will
  include price information of the conducted screening.

- api_key:

  Character string with the API key. For Anthropic, use
  [`get_api_key_anthropic()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_anthropic.md).
  Default is
  [`get_api_key_anthropic()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_anthropic.md),
  which retrieves the API key from the environment variable
  `ANTHROPIC_API_KEY`.

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry' (Wickham, 2023). The default of `max_tries` is 16.

- max_tokens:

  Numerical value indicating the maximum number of tokens to be sent in
  the request body. Default is 1024.

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
  <https://platform.claude.com/docs/en/manage-claude/rate-limits-api>.
  Alternatively, use
  [`rate_limits_per_minute()`](https://mikkelvembye.github.io/AIscreenR/reference/rate_limits_per_minute.md).

- reps:

  Numerical value indicating the number of times the same question
  should be send to the server. This can be useful to test consistency
  between answers, and/or can be used to make inclusion judgments based
  on how many times a study has been included across a the given number
  of screenings. Default is `1`.

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
  requested multiple times (i.e., when any reps \> 1). Default is 0.1,
  indicating that titles and abstracts should only be included if GPT
  has included the study more than 10 percent of the times (e.g., 1 out
  of 10 screenings). This has been shown by Vembye et al. (2025) to work
  well with cheaper models.

- incl_cutoff_lower:

  Numerical value indicating the probability threshold above which
  studies should be checked by a human. ONLY relevant when the same
  questions is requested multiple times (i.e., when any reps \> 1) and
  `incl_cutoff_upper` \> 0.1. Records with inclusion probabilities
  between `incl_cutoff_lower` and `incl_cutoff_upper` will be flagged
  for human checking. Default is `NULL`, which means that no studies
  will be flagged for human checking.

- force:

  Logical argument indicating whether to force the function to use more
  than 10 iterations and run screening costing more than 15 USD. Default
  is `FALSE`.

- custom_model:

  Logical indicating whether a fine-tuned or custom model is used.
  Default is `FALSE`.

- reasoning_effort:

  Character string indicating the level of reasoning effort required for
  the task. Default is `"none"`. Can be either `"low"`, `"medium"`,
  `"high"`, `"xhigh"` or `"max"`. `"max"` is available only using Claude
  Mythos Preview, Claude Opus 4.7, Claude Opus 4.6, and Claude Sonnet
  4.6. `"xhigh"` is available only using Claude Opus 4.7. See
  <https://platform.claude.com/docs/en/build-with-claude/adaptive-thinking>
  for more information.

- overinclusive:

  Logical indicating whether uncertain decisions (`"1.1"`) should be
  allowed in the default function calling setup. Default is `TRUE`,
  which means that the default function calling setup will allow for
  uncertain decisions. If `FALSE`, the default function calling setup
  will not allow for uncertain decisions and will only return binary
  decisions (i.e., "1" or "0"). This argument only affects the default
  function calling setup.

- ...:

  Further argument to pass to the request body. See
  <https://platform.claude.com/docs/en/api/messages/create>.

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

|  |  |  |
|----|----|----|
| **studyid** | `integer` | indicating the study ID of the reference. |
| **title** | `character` | indicating the title of the reference. |
| **abstract** | `character` | indicating the abstract of the reference. |
| **promptid** | `integer` | indicating the prompt ID. |
| **prompt** | `character` | indicating the prompt. |
| **model** | `character` | indicating the specific gpt-model used. |
| **iterations** | `numeric` | indicating the number of times the same question has been sent to Anthropic's API models. |
| **question** | `character` | indicating the final question sent to Anthropic's API models. |
| **decision_gpt** | `character` | indicating the raw gpt decision - either `"1", "0", "1.1"` for inclusion, exclusion, or uncertainty, respectively. |
| **detailed_description** | `character` | indicating detailed description of the given decision made by Anthropic's API models. ONLY included if the detailed function calling function is used. See 'Examples' below for how to use this function. |
| **decision_binary** | `integer` | indicating the binary gpt decision, that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case. |
| **prompt_tokens** | `integer` | indicating the number of prompt tokens sent to the server for the given request. |
| **completion_tokens** | `integer` | indicating the number of completion tokens sent to the server for the given request. |
| **submodel** | `character` | indicating the exact (sub)model used for screening. |
| **run_time** | `numeric` | indicating the time it took to obtain a response from the server for the given request. |
| **run_date** | `character` | indicating the date the given response was received. |
| **n** | `integer` | indicating iteration ID. Is only different from 1, when `reps > 1`. |

  
If any requests failed, the `gpt` object contains an error dataset
(`error_data`) containing the same variables as `answer_data` but with
failed request references only.  

When the same question is requested multiple times, the
`answer_data_aggregated` data contains the following *mandatory*
variables:

|  |  |  |
|----|----|----|
| **studyid** | `integer` | indicating the study ID of the reference. |
| **title** | `character` | indicating the title of the reference. |
| **abstract** | `character` | indicating the abstract of the reference. |
| **promptid** | `integer` | indicating the prompt ID. |
| **prompt** | `character` | indicating the prompt. |
| **model** | `character` | indicating the specific gpt-model used. |
| **question** | `character` | indicating the final question sent to Anthropic's API models. |
| **incl_p** | `numeric` | indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract. |
| **final_decision_gpt** | `character` | indicating the final decision reached by gpt - either 'Include', 'Exclude', or 'Check'. |
| **final_decision_gpt_num** | `integer` | indicating the final numeric decision reached by gpt - either 1 or 0. |
| **longest_answer** | `character` | indicating the longest gpt response obtained across multiple repeated responses on the same title and abstract. Only included when `decision_description = TRUE`. See 'Examples' below for how to use this function. |
| **reps** | `integer` | indicating the number of times the same question has been sent to Anthropic's API models. |
| **n_mis_answers** | `integer` | indicating the number of missing responses. |
| **submodel** | `character` | indicating the exact (sub)model used for screening. |

  

The `price_data` data contains the following variables:

|  |  |  |
|----|----|----|
| **prompt** | `character` | if multiple prompts are used this variable indicates the given prompt-id. |
| **model** | `character` | the specific gpt model used. |
| **iterations** | `integer` | indicating the number of times the same question was requested. |
| **input_price_dollar** | `integer` | price for all prompt/input tokens for the correspondent gpt-model. |
| **output_price_dollar** | `integer` | price for all completion/output tokens for the correspondent gpt-model. |
| **total_price_dollar** | `integer` | total price for all tokens for the correspondent gpt-model. |

Find current token pricing at
<https://docs.mistral.ai/models/model-selection-guide> or
[model_prizes](https://mikkelvembye.github.io/AIscreenR/reference/model_prizes.md).

## References

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2025). Generative Pretrained Transformer Models Can Function as Highly
Reliable Second Screeners of Titles and Abstracts in Systematic Reviews:
A Proof of Concept and Common Guidelines. *Psychological Methods*.
[doi:10.1037/met0000769](https://doi.org/10.1037/met0000769)

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

tabscreen_claude(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract
  )

plan(sequential)

 # Get detailed descriptions of the gpt decisions.

 plan(multisession)

 tabscreen_claude(
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
