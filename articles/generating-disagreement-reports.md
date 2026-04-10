# Generating Disagreement Reports

We highly recommend that reviewers attach a disagreement report showing
the differences between human and AI screening decisions. This helps
assess whether humans are generally too overinclusive or whether the AI
is simply incorrect. In this article, we explain how to use the
[`report()`](https://mikkelvembye.github.io/AIscreenR/reference/report.md)
function to generate a structured document that highlights disagreements
between human and AI screening decisions.

## Setup

First, we load the necessary packages.

``` r
library(dplyr)
library(AIscreenR)
```

## Step 1: Prepare Disagreement Data

The
[`report()`](https://mikkelvembye.github.io/AIscreenR/reference/report.md)
function requires a data frame containing studies where the human
decision and the AI decision do not match. For this example, we’ll use a
small dataset from the package. The data can be found here:
`inst/extdata/dis_sub.rda`.

## Step 2: Generate the Report

With the `disagreements` data frame ready, we can now use the
[`report()`](https://mikkelvembye.github.io/AIscreenR/reference/report.md)
function to generate a document. The function will create a Quarto
(`.qmd`) file and render it into the format you specify (e.g., HTML,
PDF, or Word).

The report will list each study, its title and abstract, the AI’s
answer, and a space for a human reviewer to add comments, making it easy
to resolve each conflict.

In this example, we will generate an HTML report.

``` r
# This code will generate a report in your current working directory
# The report will open automatically if 'open = TRUE'
report(
  data = disagreements,
  studyid = studyid,
  title = title,
  abstract = abstract,
  gpt_answer = longest_answer,
  human_code = human_code,
  final_decision_gpt_num = final_decision_gpt_num,
  file = "disagreement_report.qmd",
  format = "html",
  document_title = "Screening Disagreement Review",
  open = TRUE
)
```

### Automatic Subtitle Generation

Note that we did not provide a `document_subtitle`. The
[`report()`](https://mikkelvembye.github.io/AIscreenR/reference/report.md)
function automatically detects the nature of the disagreements in the
data and generates an appropriate subtitle. In our example, since the
data contains both types of disagreements (human included/AI excluded
and vice-versa), the subtitle will be “Disagreement between humans and
GPT”.

## Report Output

Running the code above will produce an HTML file named
`disagreement_report.html`. This will look like this:

![](../../inst/figures/report.png)

*Example disagreement report*
