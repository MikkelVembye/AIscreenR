
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/AIscreenR_hex.png" align="right" width="170" height = "170"/>

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
library(revtools)
library(tibble)
library(dplyr)

# Setting API
set_api_key(AIscreenR:::testing_key_chatgpt())

# Obtain rate limits info (Default is "gpt-3.5-turbo-0613")
rate_limits <- rate_limits_per_minute()
rate_limits
#> # A tibble: 1 × 3
#>   model              requests_per_minute tokens_per_minute
#>   <chr>                            <dbl>             <dbl>
#> 1 gpt-3.5-turbo-0613               10000           1000000

# Obtain rate limits info across multiple models
rate_limits <- rate_limits_per_minute(model = c("gpt-3.5-turbo-0613", "gpt-4"))
rate_limits
#> # A tibble: 2 × 3
#>   model              requests_per_minute tokens_per_minute
#>   <chr>                            <dbl>             <dbl>
#> 1 gpt-3.5-turbo-0613               10000           1000000
#> 2 gpt-4                              200             10000
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

    #> # A tibble: 10 × 6
    #>    author                              eppi_id studyid title abstract human_code
    #>    <chr>                               <chr>     <int> <chr> <chr>         <dbl>
    #>  1 Holloway R G and Gramling R and Ke… 9434957       1 Esti… "Progno…          0
    #>  2 Morawska Alina and Stallman Helen … 9433838       2 Self… "Behavi…          0
    #>  3 Michel C M and Pascual-Marqui R D … 9431171       3 Freq… "The to…          0
    #>  4 Paul Howard A                       9433968       4 A Re… "The ar…          0
    #>  5 Feinberg I and De Bie E and Davis … 9434460       5 Topo… "STUDY …          0
    #>  6 Hamburg Sam R                       9433554       6 BOOK… "The ar…          0
    #>  7 Park H Y and Lee B J and Kim J H a… 9435130       7 Rapi… "Backgr…          0
    #>  8 Petrek J                            9432040       8 Pict… "AIMS: …          0
    #>  9 Schwartzman Meredith P and Wahler … 9434093       9 Enha… "New an…          0
    #> 10 Faber J and Srutova L and Pilarova… 9431505      10 EEG … "Sponta…          0

Example of how to enter a prompt. Can also be done in word (see
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

Approximate price of screening

``` r
app_obj <- 
  approximate_price_gpt(
    data = filges2015_dat,
    prompt = prompt,
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = c("gpt-3.5-turbo-0613"),
    rep = 10 
  )

app_obj
#> The approximate price of the (simple) screening will be around $4.477.

app_obj$price_dollar
#> [1] 4.477
app_obj$price_data
#> # A tibble: 1 × 5
#>   model     iterations input_price_dollar output_price_dollar total_price_dollor
#>   <chr>          <dbl>              <dbl>               <dbl>              <dbl>
#> 1 gpt-3.5-…         10               4.42              0.0594               4.48
```

Example of how to conduct simple screening, getting return 1 if a
reference should be included, 0 if excluded, and 1.1 if a firm decision
cannot be reached.

``` r
# Subsetting the number of references to speed up the tutorial screening

test_obj <- 
  tabscreen_gpt(
    data = filges2015_dat[c(140:150),],
    prompt = prompt, 
    studyid = studyid, # indicate the variable with the studyid in the data
    title = title, # indicate the variable with the titles in the data
    abstract = abstract, # indicate the variable with the abstracts in the data
    model = c("gpt-3.5-turbo-0613"),
    reps = 2 # Number of times the same question is asked to ChatGPT
  ) 
#> * The approximate price of the current (simple) screening will be around $0.036.

test_obj
#> Find data with all answers by executing
#>  x$answer_data_all
#> 
#> Find data with the result aggregated across multiple answers by executing
#>  x$answer_data_sum
#> 
#> Find total price for the screening by executing
#>  x$price_dollor

# Data sets in object
price_dat <- test_obj$price_dat
price_dat
#> # A tibble: 1 × 5
#>   model     iterations input_price_dollar output_price_dollar price_total_dollar
#>   <chr>          <dbl>              <dbl>               <dbl>              <dbl>
#> 1 gpt-3.5-…          2             0.0323            0.000482             0.0328


all_dat <- test_obj$answer_data_all
all_dat |> select(human_code, decision_binary)
#> # A tibble: 22 × 2
#>    human_code decision_binary
#>         <dbl>           <dbl>
#>  1          0               0
#>  2          0               0
#>  3          0               0
#>  4          0               0
#>  5          0               0
#>  6          0               0
#>  7          0               0
#>  8          0               0
#>  9          0               0
#> 10          0               0
#> # ℹ 12 more rows


sum_dat <- test_obj$answer_data_sum
sum_dat |> select(human_code, final_decision_gpt:final_decision_gpt_num)
#> # A tibble: 11 × 3
#>    human_code final_decision_gpt final_decision_gpt_num
#>         <dbl> <chr>                               <dbl>
#>  1          0 Exclude                                 0
#>  2          0 Exclude                                 0
#>  3          0 Exclude                                 0
#>  4          0 Exclude                                 0
#>  5          0 Exclude                                 0
#>  6          0 Exclude                                 0
#>  7          0 Exclude                                 0
#>  8          0 Exclude                                 0
#>  9          0 Exclude                                 0
#> 10          0 Exclude                                 0
#> 11          0 Exclude                                 0
```
