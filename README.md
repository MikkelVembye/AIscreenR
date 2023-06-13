
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/AIscreenR_hex.png" align="right" width="180"/>

# AIscreenR: AI screening tools in R for systematic reviewing

<!-- badges: start -->

[![R-CMD-check](https://github.com/MikkelVembye/AIscreenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MikkelVembye/AIscreenR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/MikkelVembye/AIscreenR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/MikkelVembye/AIscreenR?branch=main)
<!-- badges: end -->

The goal of AIscreenR is to use AI tools to support screening processes
(including title and abstract screening) in systematic reviews and
related literature reviews.

## Installation

You can install the development version of AIscreenR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MikkelVembye/AIscreenR")
```

Small example of how to ask a question in ChatGPT

``` r
# Find your api key at https://platform.openai.com/account/api-keys 
# Thereafter, either encrypt it with the secret functions from the httr2 package
# see https://httr2.r-lib.org/reference/secrets.html or run set_api_key() 
# and then enter you key.
library(AIscreenR)
library(httr2)
#> Warning: package 'httr2' was built under R version 4.2.3

set_api_key(AIscreenR:::testing_key_chatgpt())

q <- "What is a carrot?"
ask_chatgpt(q, time_info = TRUE)
#> # A tibble: 1 × 2
#>   answer                                                                run_time
#>   <chr>                                                                    <dbl>
#> 1 A carrot is a root vegetable that has a firm, crunchy texture and a …     6.13
```

``` r
# Find documentation behind by running the below code
?AIscreenR::ask_chatgpt
```
