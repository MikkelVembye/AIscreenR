
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

Setting API key and checking rate limits

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
library(revtools)

set_api_key(AIscreenR:::testing_key_chatgpt())

rate_limits <- rate_limits_per_minute()
rate_limits
#> # A tibble: 1 × 3
#>   model              requests_per_minute tokens_per_minute
#>   <chr>                            <dbl>             <dbl>
#> 1 gpt-3.5-turbo-0613                3500             90000
```

How to load ris files. In this example we have downloaded the ris files
from the [EPPI-Reviewer](https://eppi.ioe.ac.uk/EPPIReviewer-Web/home).

``` r
# Loading ris file data via revtools
ris_dat_excl <- revtools::read_bibliography("path/FFT_exclude.ris") |> 
  suppressWarnings() |> 
  select(studyid = eppi_id, title, abstract) |> 
  as_tibble() |> 
  mutate(
    human_code = 0 # Indicating exclusion
  )

ris_dat_incl <- revtools::read_bibliography("path/FFT_include.ris") |> 
  suppressWarnings() |> 
  select(studyid = eppi_id, title, abstract) |> 
  as_tibble() |> 
  mutate(
    human_code = 1 # Indicating inclusion
  )

FFT_dat <- bind_rows(ris_dat_excl, ris_dat_incl)
head(FFT_dat, 10)
```

    #> # A tibble: 10 × 4
    #>    studyid title                                             abstract human_code
    #>      <int> <chr>                                             <chr>         <dbl>
    #>  1       1 "The characteristics of monomorphic ventricular … Cardiac…          0
    #>  2       2 "Signal-averaged ECG parameters in cardiac norma… There i…          0
    #>  3       3 "Propofol and thiopental for refractory status e… To asse…          0
    #>  4       4 "The effects of ethanol on EEG activity in males… The pre…          0
    #>  5       5 "Temperature dependent physiological changes by … Footbat…          0
    #>  6       6 "Evaluation of scalp and auricular acupuncture o… In this…          0
    #>  7       7 "Experiments in dysarthric speech recognition us… In this…          0
    #>  8       8 "Frequency domain source localization shows stat… The top…          0
    #>  9       9 "Unexpected effects of cognitive-behavioural the… Backgro…          0
    #> 10      10 "Accuracy and time efficiency of two ASSR analys… Backgro…          0

Example of how to enter a prompt. Can also be done in word.

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

Approximate price of screening

``` r
app_obj <- 
  approximate_price_gpt(
    data = FFT_dat,
    prompt = prompt,
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = c("gpt-3.5-turbo-0613"),
    rep = 10 
  )

app_obj
#> The approximate price of the screening will be around $1.947

app_obj$price_dollar
#> [1] 1.947
app_obj$price_data
#> # A tibble: 1 × 4
#>   model              input_price_dollar output_price_dollar price_total_dollar
#>   <chr>                           <dbl>               <dbl>              <dbl>
#> 1 gpt-3.5-turbo-0613               1.91               0.033               1.95
```

Example of how to conduct simple screening, getting return 1 if a
reference should be included, 0 if excluded, and 1.1 if a firm decision
cannot be reached.

``` r
# Subsetting the number of references to speed up the tutorial screening

test_obj <- 
  tabscreen_gpt(
    data = FFT_dat[c(149:150),],
    prompt = prompt, 
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = c("gpt-3.5-turbo-0613"),
    reps = 2 # Number of times the same question is asked to ChatGPT
  ) |> 
  suppressWarnings()
#> * The approximate price of the current (simple) screening will be around $0.0037.

test_obj
#> Find data with all answers by executing
#>  object_name$answer_data_all 
#> 
#> Find data with the result aggregated across multiple answers by executing
#>  object_name$answer_data_sum
#> 
#> Find total price for the screening by executing
#>  object_name$price_dollor

# Data sets in object
price_dat <- test_obj$price_dat
price_dat
#> # A tibble: 1 × 4
#>   model              input_price_dollar output_price_dollar price_total_dollar
#>   <chr>                           <dbl>               <dbl>              <dbl>
#> 1 gpt-3.5-turbo-0613             0.0037              0.0001             0.0038


all_dat <- test_obj$answer_data_all
all_dat |> select(decision_gpt:n)
#> # A tibble: 4 × 7
#>   decision_gpt decision_binary prompt_tokens completion_tokens run_time top_p
#>   <chr>                  <dbl>         <int>             <int>    <dbl> <dbl>
#> 1 1.1                        1           538                13      1       1
#> 2 1.1                        1           538                12      0.9     1
#> 3 1                          1           699                10      0.9     1
#> 4 1                          1           699                10      1.2     1
#> # ℹ 1 more variable: n <int>


sum_dat <- test_obj$answer_data_sum
sum_dat |> select(incl_p:n_mis_answers)
#> # A tibble: 2 × 5
#>   incl_p final_decision final_decision_num  reps n_mis_answers
#>    <dbl> <chr>                       <dbl> <int>         <int>
#> 1      1 Include                         1     2             0
#> 2      1 Include                         1     2             0
```
