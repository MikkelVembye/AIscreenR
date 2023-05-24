
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/AIscreenR_hex.png" align="right" width="180"/>

# AIscreenR: AI screening tools in R for systematic reviewing

<!-- badges: start -->
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
library(AIscreenR)
```

Small example of how to ask a question in ChatGPT

``` r

# Find your own api key at https://platform.openai.com/account/api-keys
api_key <- 123456789
q <- "What is a carrot?"
ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = TRUE, reps = 1)

# Find documentation behind by running the below code
?AIscreenR::ask_chatgpt
```
