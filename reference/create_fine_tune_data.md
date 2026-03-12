# Function to generate dataset to be used for fine-tuning models

This function creates the initial data that can be used to fine tune
models from OpenAI.

## Usage

``` r
create_fine_tune_data(data, prompt, studyid, title, abstract)
```

## Arguments

- data:

  Dataset containing the titles and abstracts.

- prompt:

  Prompt(s) to be added before the title and abstract.

- studyid:

  Unique Study ID. If missing, this is generated automatically.

- title:

  Name of the variable containing the title information.

- abstract:

  Name of variable containing the abstract information.

## Value

A dataset of class `'fine_tune_data'`.

## Note

The dataset contains at least the following variables:

|              |                            |                                                                             |
|--------------|----------------------------|-----------------------------------------------------------------------------|
| **studyid**  | `integer/character/factor` | indicating the study ID of the reference.                                   |
| **title**    | `character`                | indicating the title of the reference.                                      |
| **abstract** | `character`                | indicating the abstract of the reference.                                   |
| **question** | `character`                | indicating the final question sent to OpenAI's GPT API models for training. |

## See also

[`save_fine_tune_data()`](https://mikkelvembye.github.io/AIscreenR/reference/save_fine_tune_data.md)

## Examples

``` r
# Extract 5 irrelevant and relevant records, respectively.
dat <- filges2015_dat[c(1:5, 261:265),]

prompt <- "Is this study about functional family therapy?"

dat <-
  create_fine_tune_data(
    data = dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract
   )

dat
#> # A tibble: 10 × 7
#>    author                     eppi_id human_code studyid title abstract question
#>    <chr>                      <chr>        <dbl> <fct>   <chr> <chr>    <chr>   
#>  1 Holloway R G and Gramling… 9434957          0 1       Esti… "Progno… "Is thi…
#>  2 Morawska Alina and Stallm… 9433838          0 2       Self… "Behavi… "Is thi…
#>  3 Michel C M and Pascual-Ma… 9431171          0 3       Freq… "The to… "Is thi…
#>  4 Paul Howard A              9433968          0 4       A Re… "The ar… "Is thi…
#>  5 Feinberg I and De Bie E a… 9434460          0 5       Topo… "STUDY … "Is thi…
#>  6 James S and Alemi Q and Z… 9434933          1 261     Effe… "Purpos… "Is thi…
#>  7 Hendriks V and van der Sc… 9435035          1 262     Matc… "Backgr… "Is thi…
#>  8 Mccart M R and Henggeler … 9435045          1 263     Syst… "This s… "Is thi…
#>  9 Danielson C K and Mccart … 9435049          1 264     Redu… "The cu… "Is thi…
#> 10 Smeerdijk M and Keet R an… 9435056          1 265     Moti… "Backgr… "Is thi…
```
