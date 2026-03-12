# Generic function to re-screen failed title and abstract requests.

**\[experimental\]**  
  

This is a generic function to re-screen of failed title and abstract
requests.

## Usage

``` r
screen_errors(
  object,
  api_key = get_api_key(),
  max_tries = 4,
  max_seconds,
  is_transient,
  backoff,
  after,
  ...
)
```

## Arguments

- object:

  An object of either class `'gpt'` or `'chatgpt'`.

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
  will not retry' (Wickham, 2023). Default `max_tries` is 16. If
  missing, the value of `max_seconds` from the original screening
  conducted with
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  will be used.

- is_transient:

  'A predicate function that takes a single argument (the response) and
  returns `TRUE` or `FALSE` specifying whether or not the response
  represents a transient error' (Wickham, 2023). If missing, the
  `is_transient` function from the original screening conducted with
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  will be used.

- backoff:

  'A function that takes a single argument (the number of failed
  attempts so far) and returns the number of seconds to wait' (Wickham,
  2023). If missing, the `backoff`value from the original screening
  conducted with
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  will be used.

- after:

  'A function that takes a single argument (the response) and returns
  either a number of seconds to wait or `NULL`, which indicates that a
  precise wait time is not available that the `backoff` strategy should
  be used instead' (Wickham, 2023). If missing, the `after` value from
  the original screening conducted with
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  will be used.

- ...:

  Further argument to pass to the request body. See
  <https://platform.openai.com/docs/api-reference/chat/create>. If used
  in the original screening in
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md),
  the argument(s) must be specified here again.

## Value

An object of class `'gpt'` or `'chatgpt'` similar to the object returned
by
[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md).
See documentation for
[`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md).

## See also

[`screen_errors.gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.gpt.md),
[`screen_errors.chatgpt()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.chatgpt.md)

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
    model = "gpt-4o-mini"
    )

obj_rescreened <-
  obj_with_error |>
  screen_error()

} # }
```
