
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="https://mikkelvembye.github.io/AIscreenR/"><img src="man/figures/AIscreenR_hex.png" align="right" width="180" /></a>

# AIscreenR: AI screening tools in R for systematic reviewing

<!-- badges: start -->

[![License: GPL
(\>=3)](https://img.shields.io/badge/license-GPL-blue)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/MikkelVembye/AIscreenR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MikkelVembye/AIscreenR/actions/workflows/R-CMD-check.yaml)
[![status](https://img.shields.io/badge/status-under%20review-orange)](https://github.com/MikkelVembye/AIscreenR)
[![devel
Version](https://img.shields.io/badge/devel-0.0.0.9999-brightgreen.svg)](https://mikkelvembye.github.io/AIscreenR/)
<!-- badges: end -->

The goal of AIscreenR is to use AI tools to support screening processes
(including title and abstract screening) in systematic reviews and
related literature reviews. At the current stage, the main aim of the
`AIscreenR` package is test and use OpenAI’s GPT API models as second
screeners of titles and abstracts or alternatively to reduce the number
of references needed to be screened by humans. The package allows user
to utilize OpenAI’s GPT API models from the
[https://api.openai.com/v1/chat/completions](https://platform.openai.com/docs/models/model-endpoint-compatibility)
endpoint. In future developments, we may add further LLMs, such as API
models from Claude 2. For now, we invite other researchers to test this
software, so that we, as a review community, can get a better
understanding of the performance of OpenAI’s GPT API models for title
and abstract screening in high-quality reviews.

## Installation

Install the latest release from CRAN:

``` r
install.packages("AIscreenR")
```

You can install the development version of AIscreenR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MikkelVembye/AIscreenR")
```

Setting API key and checking rate limits

``` r
# Find your api key at https://platform.openai.com/account/api-keys 
# Thereafter, either encrypt it with the secret functions from the httr2 package
# see https://httr2.r-lib.org/reference/secrets.html or run set_api_key() 
# and then enter you key.
library(AIscreenR)
library(revtools)
library(tibble)
library(dplyr)
library(future)

# Setting API
set_api_key()

# Obtain rate limits info (Default is "gpt-4o-mini")
rate_limits <- rate_limits_per_minute()
rate_limits
#> # A tibble: 1 × 3
#>   model       requests_per_minute tokens_per_minute
#>   <chr>                     <dbl>             <dbl>
#> 1 gpt-4o-mini               30000         150000000
```

How to load RIS files. In this example we have downloaded the RIS files
from the
[EPPI-Reviewer](https://eppi.ioe.ac.uk/cms/Default.aspx?tabid=2914).

``` r

excl_path <- system.file("extdata", "excl_tutorial.ris", package = "AIscreenR")

# Loading RIS file data via revtools
ris_dat_excl <- revtools::read_bibliography("excl_path") |> 
  suppressWarnings() |> 
  select(studyid = eppi_id, title, abstract) |> 
  as_tibble() |> 
  mutate(
    human_code = 0 # Indicating exclusion
  )

incl_path <- system.file("extdata", "incl_tutorial.ris", package = "AIscreenR")

ris_dat_incl <- revtools::read_bibliography(incl_path) |> 
  suppressWarnings() |> 
  select(studyid = eppi_id, title, abstract) |> 
  as_tibble() |> 
  mutate(
    human_code = 1 # Indicating inclusion
  )

filges2015_dat<- bind_rows(ris_dat_excl, ris_dat_incl)
head(filges2015_dat, 10)
#> # A tibble: 10 × 4
#>    studyid title                                             abstract human_code
#>    <chr>   <chr>                                             <chr>         <dbl>
#>  1 9434957 Estimating and communicating prognosis in advanc… "Progno…          0
#>  2 9433838 Self-Directed Behavioral Family Intervention: Do… "Behavi…          0
#>  3 9431171 Frequency domain source localization shows state… "The to…          0
#>  4 9433968 A Review of: 'Kearney, C. A. (2010). Helping Chi… "The ar…          0
#>  5 9434460 Topographic differences in the adolescent matura… "STUDY …          0
#>  6 9433554 BOOK REVIEW                                       "The ar…          0
#>  7 9435130 Rapid improvement of depression and quality of l… "Backgr…          0
#>  8 9432040 Pictorial cognitive task solving and dynamics of… "AIMS: …          0
#>  9 9434093 Enhancing the Impact of Parent Training Through … "New an…          0
#> 10 9431505 EEG spectrum as information carrier               "Sponta…          0
```

Example of how to enter a prompt in R. Can also be done in Word (see
vignette).

``` r
prompt <- "Evaluate the following study based on the selection criteria
for a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use.
A family-based intervention (FFT) is equivalent to a behavior focused
family therapy, where young people’s drug use is understood in relation to family
behavior problems. Family-based interventions also includes manual-based family therapies as
it targets young people and their families as a system throughout treatment, and thereby recognizes
the important role of the family system in the development and treatment of young people’s drug use
problems. FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse
(NIDA). The development of FFT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
contingency contracting, impulse control strategies specific to drug use,
and increased emphasis on involvement of family members in treatment.
FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions
have been enhanced to address several mental health related problems commonly occurring
as comorbid conditions in drug use treatment participant.  For each study,
I would like you to assess:  1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or
Behavioral Family Therapy? (Outpatient manual-based interventions of any
duration delivered to young people and their families). If not, exclude study.
2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use? 3) Are the participants within age 11–21?"
```

Approximate price of screening before running the screening.

``` r
app_obj <- 
  approximate_price_gpt(
    data = filges2015_dat,
    prompt = prompt,
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = "gpt-4o-mini",
    rep = 1 
  )

app_obj
#> The approximate price of the (simple) screening will be around $0.0476.

app_obj$price_dollar
#> [1] 0.0476
app_obj$price_data
#> # A tibble: 1 × 6
#>   prompt   model       iterations input_price_dollar output_price_dollar
#>   <chr>    <chr>            <dbl>              <dbl>               <dbl>
#> 1 Prompt 1 gpt-4o-mini          1             0.0458             0.00178
#> # ℹ 1 more variable: total_price_dollar <dbl>
```

Example of how to conduct simple screening, returning `1` if a reference
should be included, `0` if excluded, and `1.1` if uncertain.

``` r
# Subsetting the number of references to speed up the tutorial screening
plan(multisession)
test_obj <- 
  tabscreen_gpt(
    data = filges2015_dat[c(1:5, 266:270),],
    prompt = prompt, 
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = "gpt-4o-mini",
    reps = 1 # Number of times the same question is asked to ChatGPT
  ) 
#> * The approximate price of the current (simple) screening will be around $0.0017.
#> * Consider removing references without abstracts since these can distort the accuracy of the screening.
#> Progress: ───────────────────────────────────────────────────────────────────────────────────── 100%
plan(sequential)
test_obj
#> 
#> Find the final result dataset via result_object$answer_data

# Data sets in object
price_dat <- test_obj$price_data
price_dat
#> # A tibble: 1 × 6
#>   prompt model       iterations input_price_dollar output_price_dollar
#>    <int> <chr>            <dbl>              <dbl>               <dbl>
#> 1      1 gpt-4o-mini          1             0.0016           0.0000432
#> # ℹ 1 more variable: total_price_dollar <dbl>

all_dat <- test_obj$answer_data
all_dat |> select(human_code, decision_binary)
#> # A tibble: 10 × 2
#>    human_code decision_binary
#>         <dbl>           <dbl>
#>  1          0               0
#>  2          0               0
#>  3          0               0
#>  4          0               0
#>  5          0               0
#>  6          1               0
#>  7          1               1
#>  8          1               0
#>  9          1               1
#> 10          1               1
```
