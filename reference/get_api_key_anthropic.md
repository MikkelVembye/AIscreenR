# Get Anthropic API key from R environment variable.

Get Anthropic API key from R environment variable.

## Usage

``` r
get_api_key_anthropic(env_var = "ANTHROPIC_API_KEY")
```

## Arguments

- env_var:

  Character string indicating the name of the temporary R environment
  variable with the API key and the used AI model. Currently, the
  argument only takes `env_var = "ANTHROPIC_API_KEY"`. See
  [`set_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md)
  to set/create this variable.

## Value

The specified API key (NOTE: Avoid exposing this in the console).

## Details

`get_api_key_anthropic()` can be used after executing
[`set_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md)
or by adding the api key permanently to your R environment by using
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).
Then write `ANTHROPIC_API_KEY=[insert your api key here]` and close the
`.Renviron` window and restart R.

## Note

Find your personal API key via the Anthropic at
<https://platform.claude.com/settings/keys>.

## See also

[`set_api_key`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md).

## Examples

``` r

if (FALSE) { # \dontrun{
get_api_key_anthropic()
} # }
```
