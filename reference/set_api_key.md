# Creating a temporary R environment API key variable

This function automatically sets/creates an interim R environment
variable with the API key to call a given AI model (e.g. ChatGPT).
Thereby users avoid exposing their API keys. If the API key is set in
the console, it will/can be revealed via the .Rhistory. Find more
information about this issue at
<https://httr2.r-lib.org/articles/wrapping-apis.html>.

## Usage

``` r
set_api_key(key, env_var = "CHATGPT_KEY")
```

## Arguments

- key:

  Character string with an (ideally encrypt) API key. See how to encrypt
  key here:
  <https://httr2.r-lib.org/articles/wrapping-apis.html#basics>. If not
  provided, it returns a password box in which the true API key can be
  secretly entered.

- env_var:

  Character string indicating the name of the temporary R environment
  variable with the API key and the used AI model. Currently, the
  argument only takes `env_var = "CHATGPT_KEY"`.

## Value

A temporary environment variable with the name from `env_var`. If `key`
is missing, it returns a password box in which the true API key can be
entered.

## Details

When set_api_key() has successfully been executed,
[`get_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md)
automatically retrieves the API key from the R environment and the users
do not need to specify the API when running functions from the package
that call the API. The API key can be permanently set by using
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).
Then write `CHATGPT_KEY=[insert your api key here]` and close the
`.Renviron` window and restart R.

## Note

Find your personal API key at
<https://platform.openai.com/account/api-keys>.

## See also

[`get_api_key`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_api_key()
} # }
```
