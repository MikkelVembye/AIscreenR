#' RIS file data from Functional Family Therapy (FFT) systematic review
#'
#' Bibliometric toy data from a systematic review regarding
#' Functional Family Therapy (FFT) for Young People in Treatment for
#' Non-opioid Drug Use (Filges et al., 2015). The data includes all 90 included and 180 excluded
#' randomly sampled references from the literature search of the
#' systematic review.
#'
#'
#' @format A \code{tibble} with 270 rows/studies and 6 variables/columns
#'
#' \tabular{lll}{
#'  \bold{author} \tab \code{character} \tab indicating the authors of the reference \cr
#'  \bold{eppi_id} \tab \code{character} \tab indicating a unique eppi-ID for each study \cr
#'  \bold{studyid} \tab \code{numeric} \tab indicating a unique study-ID for each study \cr
#'  \bold{title} \tab \code{character} \tab with the title of the study \cr
#'  \bold{abstract}  \tab \code{character} \tab with the study abstract \cr
#'  \bold{human_code} \tab \code{numeric} \tab indicating the human screening decision.
#'  1 = included, 0 = excluded. \cr
#' }
#'
#'
#' @references Filges, T., Andersen, D, & Jørgensen, A-M. K (2015).
#' Functional Family Therapy (FFT) for Young People in Treatment for Non-opioid Drug Use: A Systematic Review
#' \emph{Campbell Systematic Reviews}, \doi{10.4073/csr.2015.14}
#'

"filges2015_dat"


#' Model prize data (last updated August 14, 2025)
#'
#' Data set containing input and output prizes for all OpenAI's GPT API models.
#'
#' @format A \code{data.frame} containing 22 rows/models and 3 variables/columns
#'
#' \tabular{lll}{
#'  \bold{model} \tab \code{character} \tab indicating the specific GPT model \cr
#'  \bold{price_in_per_token} \tab \code{character} \tab indicating the input prize per token \cr
#'  \bold{price_out_per_token} \tab \code{character} \tab indicating the output prize per token \cr
#' }
#'
#' @references OpenAI. \emph{Pricing}. \url{https://openai.com/api/pricing/}

"model_prizes"

#' Disagreement sample data
#'
#' Example rows where human screening decisions differ from GPT decisions.
#' Each row is a (study × prompt) screening outcome.
#'
#' @format A tibble/data.frame with one row per screened (studyid, promptid) and 17 columns:
#' \tabular{lll}{
#'  \bold{author} \tab character \tab Study authors \cr
#'  \bold{human_code} \tab numeric \tab Human screening decision (1 include, 0 exclude) \cr
#'  \bold{studyid} \tab integer \tab Unique study identifier \cr
#'  \bold{title} \tab character \tab Study title \cr
#'  \bold{abstract} \tab character \tab Study abstract \cr
#'  \bold{promptid} \tab integer \tab Prompt identifier \cr
#'  \bold{prompt} \tab character \tab Original short screening prompt text \cr
#'  \bold{model} \tab character \tab Model used for the run \cr
#'  \bold{question} \tab character \tab Full constructed question sent to model \cr
#'  \bold{top_p} \tab numeric \tab Nucleus sampling parameter \cr
#'  \bold{incl_p} \tab numeric \tab Estimated probability of inclusion (if repetitions) \cr
#'  \bold{final_decision_gpt} \tab character \tab GPT final label: Include / Exclude / Check \cr
#'  \bold{final_decision_gpt_num} \tab numeric \tab Numeric GPT decision (1 include/check, 0 exclude) \cr
#'  \bold{longest_answer} \tab character \tab Longest rationale text returned \cr
#'  \bold{reps} \tab integer \tab Number of repetitions attempted \cr
#'  \bold{n_mis_answers} \tab integer \tab Count of missing answers across reps \cr
#'  \bold{submodel} \tab character \tab Specific model variant (if applicable) \cr
#' }
"disagreements"
