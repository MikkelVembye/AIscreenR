# Function to write/save fine tune dataset in required jsonl format

This function creates `jsonl` training data that can be used to fine
tune models from OpenAI. To generate a fine tuned model, this written
data can be uploaded to <https://platform.openai.com/finetune/>.

## Usage

``` r
save_fine_tune_data(
  data,
  role_and_subject,
  file,
  true_answer,
  roles = c("system", "user", "assistant")
)
```

## Arguments

- data:

  The dataset with questions strings that should be used for training.
  The data must be of class `'fine_tune_data'`, containing two variables
  named question and true_answer.

- role_and_subject:

  Descriptions of the role of the GPT model and the subject under
  review, respectively.

- file:

  A character string naming the file to write to. If not specified the
  written file name and format will be `"fine_tune_data.jsonl"`.

- true_answer:

  Optional name of the variable containing the true answers/decisions
  used for training. Only relevant, if the the dataset contains a
  variable with the name true_answer.

- roles:

  String variable defining the various role the model should take.
  Default is `roles = c("system", "user", "assistant")`.

## Value

A `jsonl` dataset to the set working directory.

## See also

[`create_fine_tune_data()`](https://mikkelvembye.github.io/AIscreenR/reference/create_fine_tune_data.md)

## Examples

``` r
# Extract 5 irrelevant and relevant records, respectively.
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

dat <- filges2015_dat[c(1:5, 261:265),]

prompt <- "Is this study about functional family therapy?"

ft_dat <-
  create_fine_tune_data(
    data = dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract
    ) |>
    mutate(true_answer = if_else(human_code == 1, "Include", "Exclude"))

role_subject <- paste0(
  "Act as a systematic reviewer that is screening study titles and ",
  "abstracts for your systematic reviews regarding the the effects ",
  "of family-based interventions on drug abuse reduction for young ",
  "people in treatment for non-opioid drug use."
)

# Saving data in jsonl format (required format by OpenAI)
fil <- tempfile("fine_tune_data", fileext = ".jsonl")

save_fine_tune_data(
  data = ft_dat,
  role_and_subject = role_subject,
  file = fil
)
```
