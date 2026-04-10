# Generic function to re-screen failed title and abstract requests.

This is a generic function to re-screen failed title and abstract
requests. It reuses the arguments captured during the original screening
and only re-submits the rows stored in `object$error_data` to the
appropriate backend.

## Usage

``` r
screen_errors(
  object,
  api_key = NULL,
  max_tries = NULL,
  max_seconds = NULL,
  is_transient = NULL,
  backoff = NULL,
  after = NULL,
  studyid = NULL,
  title = NULL,
  abstract = NULL,
  ...
)
```

## Arguments

- object:

  An object of either class `'gpt'` or `'groq'`, as returned by
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  or
  [`tabscreen_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_groq.md).
  Objects of class `'ollama'` are not supported.

- api_key:

  Optional API key. If omitted, the selected backend uses its own
  default (e.g.,
  [`get_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md)
  for GPT and
  [`get_api_key_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key_groq.md)
  for Groq).

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry' (Wickham, 2023). If missing, the values from the
  original screening (stored in `attr(object, "arg_list")`) will be
  reused.

- is_transient:

  'A predicate function that takes a single argument (the response) and
  returns `TRUE` or `FALSE` specifying whether or not the response
  represents a transient error' (Wickham, 2023). If missing, the
  `is_transient` function from the original screening will be used.

- backoff:

  'A function that takes a single argument (the number of failed
  attempts so far) and returns the number of seconds to wait' (Wickham,
  2023). If missing, the `backoff` value from the original screening
  will be used.

- after:

  'A function that takes a single argument (the response) and returns
  either a number of seconds to wait or `NULL`, which indicates that a
  precise wait time is not available and that the `backoff` strategy
  should be used instead' (Wickham, 2023). If missing, the `after` value
  from the original screening will be used.

- studyid:

  Optional column (unquoted) for study id. Defaults to 'studyid'.

- title:

  Optional column (unquoted) for title. If omitted, inferred (e.g.,
  title, ti, t1).

- abstract:

  Optional column (unquoted) for abstract. If omitted, inferred (e.g.,
  abstract, ab, abs).

- ...:

  Further arguments forwarded to the underlying backend function
  ([`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  or
  [`tabscreen_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_groq.md)).
  If arguments were supplied in the original screening and should differ
  for re-screening, pass them again here.

## Value

An object of class `'gpt'` or `'groq'` similar to the object returned by
the original screening function, with:

- `answer_data` updated to include newly successful rows,

- `error_data` updated to include only remaining failures. Other fields
  (e.g., `price_data`, `price_dollar`, and `arg_list`) are preserved or
  updated by the backend.

## Details

The backend is derived from `class(object)` and mapped to either
[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
or
[`tabscreen_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_groq.md).
Only rows in `object$error_data` are re-submitted. To avoid name
collisions during unnesting in the backend, columns that will be
regenerated (currently `decision_binary`, `decision_description`,
`error_message`, `res`) are dropped from `error_data` before the call.
The original arguments from the first screening are taken from
`attr(object, "arg_list")` and are combined with any non-`NULL`
overrides provided here.

## See also

[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md),
[`tabscreen_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_groq.md)

## Examples

``` r
if (FALSE) { # \dontrun{

# Example with openai
set_api_key()
prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

obj_with_error <-
  tabscreen_gpt(
    data = filges2015_dat[1:2,],
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "gpt-4o-mini"
    )

obj_rescreened <-
  obj_with_error |>
  screen_errors()


# Example with groq
prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

obj_with_error <-
  tabscreen_groq(
    data = filges2015_dat[1:2,],
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = "llama-3.3-70b-versatile"
  )

obj_rescreened <-
  obj_with_error |>
  screen_errors()
} # }
```
