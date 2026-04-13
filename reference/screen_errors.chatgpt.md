# Re-screen failed requests.

**\[superseded\]**  
  

This function supports re-screening of all failed title and abstract
requests screened with
[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md).
This function has been deprecated because OpenAI has deprecated the
function_call and functions argument that was used in
[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md).

## Usage

``` r
screen_errors.chatgpt(
  object,
  ...,
  api_key = get_api_key(),
  max_tries = 4,
  max_seconds,
  is_transient,
  backoff,
  after
)
```

## Arguments

- object:

  An object of class `'chatgpt'`.

- ...:

  Further argument to pass to the request body. See
  <https://developers.openai.com/api/reference/resources/chat>. If used
  in the original screening (e.g., with
  [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md)),
  the argument(s) must be specified again here.

- api_key:

  Numerical value with your personal API key.

- max_tries, max_seconds:

  'Cap the maximum number of attempts with `max_tries` or the total
  elapsed time from the first request with `max_seconds`. If neither
  option is supplied (the default),
  [`httr2::req_perform()`](https://httr2.r-lib.org/reference/req_perform.html)
  will not retry' (Wickham, 2023). Default `max_tries` is 4. If missing,
  the value of `max_seconds` from the original screening (e.g.,
  conducted with
  [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md))
  will be used.

- is_transient:

  'A predicate function that takes a single argument (the response) and
  returns `TRUE` or `FALSE` specifying whether or not the response
  represents a transient error' (Wickham, 2023). If missing, the
  `is_transient` function from the original screening (e.g., conducted
  with
  [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md))
  will be used.

- backoff:

  'A function that takes a single argument (the number of failed
  attempts so far) and returns the number of seconds to wait' (Wickham,
  2023). If missing, the `backoff`value from the original screening
  (e.g., conducted with
  [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md))
  will be used.

- after:

  'A function that takes a single argument (the response) and returns
  either a number of seconds to wait or `NULL`, which indicates that a
  precise wait time is not available that the `backoff` strategy should
  be used instead' (Wickham, 2023). If missing, the `after` value from
  the original screening (e.g., conducted with
  [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md))
  will be used.

## Value

Object of class `'chatgpt'` similar to the object returned by
[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md).
See documentation value for
[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md).

## References

Wickham H (2023). *httr2: Perform HTTP Requests and Process the
Responses*. https://httr2.r-lib.org, https://github.com/r-lib/httr2.

## See also

[`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md)

## Examples

``` r
if (FALSE) { # \dontrun{

set_api_key()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

obj_with_error <-
  tabscreen_gpt(
    data = filges2015_dat[1:2,],
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-16k-0613"),
    max_tries = 1,
    reps = 10
    )

obj_rescreened <-
  obj_with_error |>
  screen_error()

# Alternatively re-set max_tries if errors still appear
obj_rescreened <-
  obj_with_error |>
  screen_error(max_tries = 16)
} # }
```
