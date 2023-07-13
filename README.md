
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
library(revtools)
library(tibble)
library(dplyr)

set_api_key(AIscreenR:::testing_key_chatgpt())

rate_limits <- rate_limits_per_minute()
rate_limits
#> # A tibble: 1 × 3
#>   model              requests_per_minute tokens_per_minute
#>   <chr>                            <dbl>             <dbl>
#> 1 gpt-3.5-turbo-0613                3500             90000

q <- "What is a banana?"
ask_gpt_0301(q, rpm = rate_limits$requests_per_minute)
#> # A tibble: 1 × 3
#>   answer                                                         run_time tokens
#>   <chr>                                                             <dbl>  <int>
#> 1 A banana is a long, curved fruit with a yellow skin and a pul…      2.1     91
```

``` r
# Find documentation behind by running the below code
?AIscreenR::ask_gpt_0301
```

How to load ris files. In this example we have downloaded the ris files
from the [EPPI-Reviewer](https://eppi.ioe.ac.uk/EPPIReviewer-Web/home).

``` r
# References included for full-text screening
include <- c(45542056, 84953535, 84964488, 84956376, 84971042, 84966265)

ris_dat <- revtools::read_bibliography("other/ris files/Pilotscreen_executive_functions.ris") |> 
  suppressWarnings() |> 
  select(eppi_id, title, abstract) |> 
  as_tibble() |> 
  mutate(
    human_code = if_else(eppi_id %in% include, 1, 0)
  )

ris_dat |> head(5)
#> # A tibble: 5 × 4
#>   eppi_id  title                                             abstract human_code
#>   <chr>    <chr>                                             <chr>         <dbl>
#> 1 84952933 A 4-component scale of working memory: Developin… Introdu…          0
#> 2 84955993 A quick and easy strategy to reduce test anxiety… The neg…          0
#> 3 84972741 How do 3-year-olds use relevance inferencing to … If a ch…          0
#> 4 84952367 The impact of Texas HIPPY on school readiness an… This st…          0
#> 5 84959326 The effect of two different approaches to teachi… In Janu…          0
```

NOTE: More extensive examples of how to use this tool will soon be
added!
