# Disagreement sample data

Example rows where human screening decisions differ from GPT decisions.
Each row is a (study × prompt) screening outcome.

## Usage

``` r
disagreements
```

## Format

A tibble/data.frame with one row per screened (studyid, promptid) and 17
columns:

|                            |           |                                                     |
|----------------------------|-----------|-----------------------------------------------------|
| **author**                 | character | Study authors                                       |
| **human_code**             | numeric   | Human screening decision (1 include, 0 exclude)     |
| **studyid**                | integer   | Unique study identifier                             |
| **title**                  | character | Study title                                         |
| **abstract**               | character | Study abstract                                      |
| **promptid**               | integer   | Prompt identifier                                   |
| **prompt**                 | character | Original short screening prompt text                |
| **model**                  | character | Model used for the run                              |
| **question**               | character | Full constructed question sent to model             |
| **top_p**                  | numeric   | Nucleus sampling parameter                          |
| **incl_p**                 | numeric   | Estimated probability of inclusion (if repetitions) |
| **final_decision_gpt**     | character | GPT final label: Include / Exclude / Check          |
| **final_decision_gpt_num** | numeric   | Numeric GPT decision (1 include/check, 0 exclude)   |
| **longest_answer**         | character | Longest rationale text returned                     |
| **reps**                   | integer   | Number of repetitions attempted                     |
| **n_mis_answers**          | integer   | Count of missing answers across reps                |
| **submodel**               | character | Specific model variant (if applicable)              |
