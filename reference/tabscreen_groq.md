# Title and abstract screening with GROQ API models using function calls via the tools argument

This function supports the conduct of title and abstract screening with
Groq API models in R. Specifically, it allows the user to draw on
Groq-hosted models. The function allows to run title and abstract
screening across multiple prompts and with repeated questions to check
for consistency across answers. All of which can be done in parallel.
The function draws on function calling which is called via the tools
argument in the request body. Function calls ensure more reliable and
consistent responses to ones requests. See [Vembye, Christensen,
Mølgaard, and Schytt. (2025)](https://osf.io/preprints/osf/yrhzm) for
guidance on how adequately to conduct title and abstract screening with
GPT models.

## Usage

``` r
tabscreen_groq(data, prompt, studyid, title, abstract, 
api_url = "https://api.groq.com/openai/v1/chat/completions",
 ..., model = "llama-3.1-8b-instant", role = "user", 
tools = NULL, tool_choice = NULL, top_p = 1, 
time_info = TRUE, token_info = TRUE, api_key = get_api_key_groq(), 
max_tries = 16, max_seconds = NULL, is_transient = .groq_is_transient, 
backoff = NULL, after = NULL, rpm = 10000, reps = 1, seed_par = NULL,
progress = TRUE, decision_description = FALSE, overinclusive = TRUE, 
messages = TRUE, incl_cutoff_upper = NULL, incl_cutoff_lower = NULL, 
force = FALSE)
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

  Character string with the endpoint URL for Groq's API. Default is
  `"https://api.groq.com/openai/v1/chat/completions"`.

- ...:

  Further argument to pass to the request body.

- model:

  Character string with the name of the completion model. Can take
  multiple Groq models. Default = `"llama3-70b-8192"`. Find available
  models at <https://console.groq.com/docs/models>.

- role:

  Character string indicate the role of the user. Default is `"user"`.

- tools:

  List of function definitions for tool calling. Default behavior is set
  based on `decision_description` parameter. For detailed responses, the
  function uses tools that include detailed description capabilities.

- tool_choice:

  Specification for which tool to use. Default behavior is set based on
  `decision_description` parameter. For simple responses uses
  "inclusion_decision_simple", for detailed responses uses
  "inclusion_decision".

- top_p:

  'An alternative to sampling with temperature, called nucleus sampling,
  where the model considers the results of the tokens with top_p
  probability mass. So 0.1 means only the tokens comprising the top 10%
  probability mass are considered. We generally recommend altering this
  or temperature but not both.' (Groq). Default is 1.

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
  <https://console.groq.com/keys>. Set with
  `Sys.setenv(GROQ_API_KEY = "your-api-key")` or use
  [`get_api_key_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_groq.md).

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry'. (Wickham, 2023). The default of `max_tries` is 16.

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
  available for the specified model.

- reps:

  Numerical value indicating the number of times the same question
  should be sent to Groq's API models. This can be useful to test
  consistency between answers. Default is `1`.

- seed_par:

  Numerical value for a seed to ensure that proper, parallel-safe random
  numbers are produced.

- progress:

  Logical indicating whether a progress line should be shown when
  running the title and abstract screening in parallel. Default is
  `TRUE`.

- decision_description:

  Logical indicating whether to include detailed descriptions of
  decisions. Default is `FALSE`. When conducting large-scale screening,
  we generally recommend not using this feature as it will substantially
  increase the cost of the screening. We generally recommend using it
  when encountering disagreements between GPT and human decisions.

- overinclusive:

  Logical indicating whether uncertain decisions (`"1.1"`) should be
  allowed in the default function calling setup. Default is `TRUE`,
  which means that the default function calling setup will allow for
  uncertain decisions. If `FALSE`, the default function calling setup
  will not allow for uncertain decisions and will only return binary
  decisions (i.e., "1" or "0"). This argument only affects the default
  function calling setup.

- messages:

  Logical indicating whether to print messages embedded in the function.
  Default is `TRUE`.

- incl_cutoff_upper:

  Numerical value indicating the probability threshold for which a
  studie should be included. ONLY relevant when the same questions is
  requested multiple times (i.e., when any reps \> 1). Default is 0.5,
  which indicates that titles and abstracts that Groq's API model has
  included more than 50 percent of the times should be included.

- incl_cutoff_lower:

  Numerical value indicating the probability threshold above which
  studies should be check by a human. ONLY relevant when the same
  questions is requested multiple times (i.e., when any reps \> 1).
  Default is 0.4, which means that if you ask Groq's API model the same
  questions 10 times and it includes the title and abstract 4 times, we
  suggest that the study should be check by a human.

- force:

  Logical argument indicating whether to force the function to use more
  than 10 iterations. This argument is developed to avoid the conduct of
  wrong and extreme sized screening. Default is `FALSE`.

## Value

An object of class `"gpt"`. The object is a list containing the
following components:

- answer_data_aggregated:

  dataset with the summarized, probabilistic inclusion decision for each
  title and abstract across multiple repeated questions (only when reps
  \> 1).

- answer_data:

  dataset with all individual answers.

- price_dollar:

  numerical value indicating the total price (in USD) of the screening.

- price_data:

  dataset with prices across all models used for screening.

- error_data:

  dataset with failed requests (only included if errors occurred).

- run_date:

  date when the screening was conducted.

## Note

The `answer_data_aggregated` data (only present when reps \> 1) contains
the following mandatory variables:

|                            |             |                                                                                                                                                                                                                |
|----------------------------|-------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **studyid**                | `integer`   | indicating the study ID of the reference.                                                                                                                                                                      |
| **title**                  | `character` | indicating the title of the reference.                                                                                                                                                                         |
| **abstract**               | `character` | indicating the abstract of the reference.                                                                                                                                                                      |
| **promptid**               | `integer`   | indicating the prompt ID.                                                                                                                                                                                      |
| **prompt**                 | `character` | indicating the prompt.                                                                                                                                                                                         |
| **model**                  | `character` | indicating the specific model used.                                                                                                                                                                            |
| **question**               | `character` | indicating the final question sent to Groq's API models.                                                                                                                                                       |
| **top_p**                  | `numeric`   | indicating the applied top_p.                                                                                                                                                                                  |
| **incl_p**                 | `numeric`   | indicating the probability of inclusion calculated across multiple repeated responses on the same title and abstract.                                                                                          |
| **final_decision_gpt**     | `character` | indicating the final decision reached by model - either 'Include', 'Exclude', or 'Check'.                                                                                                                      |
| **final_decision_gpt_num** | `integer`   | indicating the final numeric decision reached by model - either 1 or 0.                                                                                                                                        |
| **longest_answer**         | `character` | indicating the longest response obtained across multiple repeated responses on the same title and abstract. Only included if the detailed function is used. See 'Examples' below for how to use this function. |
| **reps**                   | `integer`   | indicating the number of times the same question has been sent to Groq's API models.                                                                                                                           |
| **n_mis_answers**          | `integer`   | indicating the number of missing responses.                                                                                                                                                                    |

  
The `answer_data` data contains the following mandatory variables:

|                          |             |                                                                                                                                                                                     |
|--------------------------|-------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **studyid**              | `integer`   | indicating the study ID of the reference.                                                                                                                                           |
| **title**                | `character` | indicating the title of the reference.                                                                                                                                              |
| **abstract**             | `character` | indicating the abstract of the reference.                                                                                                                                           |
| **promptid**             | `integer`   | indicating the prompt ID.                                                                                                                                                           |
| **prompt**               | `character` | indicating the prompt.                                                                                                                                                              |
| **model**                | `character` | indicating the specific model used.                                                                                                                                                 |
| **iterations**           | `numeric`   | indicating the number of times the same question has been sent to Groq's API models.                                                                                                |
| **question**             | `character` | indicating the final question sent to Groq's API models.                                                                                                                            |
| **top_p**                | `numeric`   | indicating the applied top_p.                                                                                                                                                       |
| **decision_gpt**         | `character` | indicating the raw decision - either `"1", "0", "1.1"` for inclusion, exclusion, or uncertainty, respectively.                                                                      |
| **detailed_description** | `character` | indicating detailed description of the given decision made by Groq's API models. Only included if the detailed function is used. See 'Examples' below for how to use this function. |
| **decision_binary**      | `integer`   | indicating the binary decision, that is 1 for inclusion and 0 for exclusion. 1.1 decision are coded equal to 1 in this case.                                                        |
| **prompt_tokens**        | `integer`   | indicating the number of prompt tokens sent to the server for the given request.                                                                                                    |
| **completion_tokens**    | `integer`   | indicating the number of completion tokens sent to the server for the given request.                                                                                                |
| **run_time**             | `numeric`   | indicating the time it took to obtain a response from the server for the given request.                                                                                             |
| **n**                    | `integer`   | indicating request ID.                                                                                                                                                              |

  
If any requests failed to reach the server, the object contains an error
data set (`error_data`) having the same variables as `answer_data` but
with failed request references only.  

The `price_data` data contains the following variables:

|                         |             |                                                                     |
|-------------------------|-------------|---------------------------------------------------------------------|
| **model**               | `character` | model name.                                                         |
| **input_price_dollar**  | `integer`   | price for all prompt/input tokens for the correspondent model.      |
| **output_price_dollar** | `integer`   | price for all completion/output tokens for the correspondent model. |
| **price_total_dollar**  | `integer`   | total price for all tokens for the correspondent model.             |

Find current token pricing at <https://groq.com/pricing>.

## References

Vembye, M. H., Christensen, J., Mølgaard, A. B., & Schytt, F. L. W.
(2024) *GPT API Models Can Function as Highly Reliable Second Screeners
of Titles and Abstracts in Systematic Reviews: A Proof of Concept and
Common Guidelines* <https://osf.io/preprints/osf/yrhzm>

Thomas, J. et al. (2024). Responsible AI in Evidence SynthEsis (RAISE):
guidance and recommendations. <https://osf.io/cn7x4>

Wickham H (2023). *httr2: Perform HTTP Requests and Process the
Responses*. <https://httr2.r-lib.org>, <https://github.com/r-lib/httr2>.

## Examples

``` r
if (FALSE) { # \dontrun{

set_api_key_groq()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

plan(multisession)

tabscreen_groq(
  data = filges2015_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = "llama3-70b-8192",
  max_tries = 2
  )
plan(sequential)

 # Get detailed descriptions of the decisions by using the
 # decision_description option.
plan(multisession)

 tabscreen_groq(
   data = filges2015_dat[1:2,],
   prompt = prompt,
   studyid = studyid,
   title = title,
   abstract = abstract,
   model = "llama3-70b-8192",
   decision_description = TRUE,
   max_tries = 2
 )
plan(sequential)
} # }
```
