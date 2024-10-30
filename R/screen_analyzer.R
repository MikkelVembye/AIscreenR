#' @title Analyze performance between the human and AI screening.
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' When both the human and AI title and abstract screening has been done, this function
#' allows you to calculate performance measures of the screening, including the overall
#' accuracy, specificity and sensitivity of the screening, as well as
#' inter-rater reliability kappa statistics.
#'
#' @references
#' Gartlehner, G., Wagner, G., Lux, L., Affengruber, L., Dobrescu, A., Kaminski-Hartenthaler, A., & Viswanathan, M. (2019).
#' Assessing the accuracy of machine-assisted abstract screening with DistillerAI: a user study.
#' *Systematic Reviews*, 8(1), 277. \doi{10.1186/s13643-019-1221-3}
#'
#' McHugh, M. L. (2012).
#' Interrater reliability: The kappa statistic.
#' *Biochemia Medica*, 22(3), 276-282. \url{https://pubmed.ncbi.nlm.nih.gov/23092060/}
#'
#' Syriani, E., David, I., & Kumar, G. (2023).
#' Assessing the Ability of ChatGPT to Screen Articles for Systematic Reviews.
#' *ArXiv Preprint ArXiv:2307.06464*.
#'
#' @param x An object of either class`'gpt'` or `'chatgpt'`
#'    or a dataset of either class `'gpt_tbl'`, `'chatgpt_tbl'`, or `'gpt_agg_tbl'`
#' @param human_decision Indicate the variable in the data that contains the human_decision.
#'    This variable must be numeric, containing 1 (for included references) and 0 (for excluded references) only.
#' @param key_result Logical indicating if only the raw agreement, recall, and specificity measures should be returned.
#'    Default is `TRUE`.
#'
#' @return A `tibble` with screening performance measures. The `tibble` includes the following variables:
#' \tabular{lll}{
#'  \bold{promptid} \tab \code{integer} \tab indicating the prompt ID. \cr
#'  \bold{model} \tab \code{character}   \tab indicating the specific gpt-model used. \cr
#'  \bold{reps}  \tab \code{integer}  \tab indicating the number of times the same question was sent to ChatGPT. \cr
#'  \bold{top_p} \tab \code{numeric}  \tab indicating the applied top_p. \cr
#'  \bold{n_screened} \tab \code{integer} \tab indicating the number of screened references. \cr
#'  \bold{n_missing} \tab \code{numeric} \tab indicating the number of missing responses. \cr
#'  \bold{n_refs} \tab \code{integer} \tab indicating the number of references expected to be screened for the given condition. \cr
#'  \bold{human_in_gpt_ex} \tab \code{numeric}  \tab indicating the number of references included by humans and excluded by gpt. \cr
#'  \bold{human_ex_gpt_in} \tab \code{numeric}  \tab indicating the number of references excluded by humans and included by gpt. \cr
#'  \bold{human_in_gpt_in} \tab \code{numeric}  \tab indicating the number of references included by humans and included by gpt. \cr
#'  \bold{human_ex_gpt_ex} \tab \code{numeric}  \tab indicating the number of references excluded by humans and excluded by gpt. \cr
#'  \bold{accuracy} \tab \code{numeric}  \tab indicating the overall percent disagreement between human and gpt (Gartlehner et al., 2019). \cr
#'  \bold{p_agreement} \tab \code{numeric} \tab indicating the overall percent agreement between human and gpt. \cr
#'  \bold{precision}  \tab \code{numeric}  \tab "measures the ability to include only articles that should be included" (Syriani et al., 2023). \cr
#'  \bold{recall}  \tab \code{numeric} \tab "measures the ability to include all articles that should be included" (Syriani et al., 2023). \cr
#'  \bold{npv}  \tab \code{numeric}  \tab Negative predictive value (NPV) "measures the ability to exclude only articles that should be excluded" (Syriani et al., 2023). \cr
#'  \bold{specificity}  \tab \code{numeric} \tab "measures the ability to exclude all articles that should be excluded" (Syriani et al., 2023). \cr
#'  \bold{bacc}  \tab \code{numeric}  \tab "capture the accuracy of deciding both inclusion and exclusion classes" (Syriani et al., 2023). \cr
#'  \bold{F2}  \tab \code{numeric} \tab F-measure that "consider the cost of getting false negatives twice as costly as getting false positives" (Syriani et al., 2023). \cr
#'  \bold{mcc}  \tab \code{numeric} \tab indicating percent agreement for excluded references (Gartlehner et al., 2019). \cr
#'  \bold{irr}  \tab \code{numeric}  \tab indicating the inter-rater reliability as described in McHugh (2012). \cr
#'  \bold{se_irr} \tab \code{numeric} \tab indicating standard error for the inter-rater reliability. \cr
#'  \bold{cl_irr} \tab \code{numeric} \tab indicating lower confidence interval for the inter-rater reliability. \cr
#'  \bold{cu_irr} \tab \code{numeric} \tab indicating upper confidence interval for the inter-rater reliability. \cr
#'  \bold{level_of_agreement} \tab \code{character} \tab interpretation of the inter-rater reliability as suggested by McHugh (2012). \cr
#' }
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(future)
#'
#' set_api_key()
#'
#' prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"
#'
#' plan(multisession)
#'
#' res <- tabscreen_gpt(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract
#'   )
#'
#' plan(sequential)
#'
#' res |> screen_analyzer()
#'
#' }


screen_analyzer <- function(x, human_decision = human_code, key_result = TRUE){


  if (all(!is_chatgpt(x), !is_chatgpt_tbl(x), !is_gpt(x), !is_gpt_tbl(x), !is_gpt_agg_tbl(x))){
    stop("The object must be of either class 'gpt, 'gpt_tbl', 'gpt_agg_tbl', 'chatgpt' or 'chatgpt_tbl'.")
  }

  if (is_chatgpt(x)) dat <- x$answer_data_sum
  if (is_chatgpt_tbl(x)) dat <- x

  names <- names(x)

  if(!"answer_data_aggregated" %in% names & is_gpt(x)){
    dat <- x$answer_data |> dplyr::rename(final_decision_gpt_num = decision_binary, reps = iterations, top_p = topp)
  } else if("answer_data_aggregated" %in% names & is_gpt(x)) {
    dat <- x$answer_data_aggregated
  }

  if (is_gpt_tbl(x)) dat <- x |> dplyr::rename(final_decision_gpt_num = decision_binary, reps = iterations, top_p = topp)
  if (is_gpt_agg_tbl(x)) dat <- x

  hum_decision_name <- as.character(substitute(human_decision))

  if(!hum_decision_name %in% names(dat)) stop("You must have an variable with the human decision to use this function.")

  res <-
    dat |>
    dplyr::mutate(n_ref = n_distinct(studyid)) |>
    dplyr::filter(!is.na(final_decision_gpt_num)) |>
    summarise(
      n_screened = n(),
      n_refs = unique(n_ref),
      n_missing = n_refs - n_screened,
      human_in_gpt_ex = sum(if_else({{ human_decision }} == 1 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),
      human_ex_gpt_in = sum(if_else({{ human_decision }}  == 0 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      human_in_gpt_in = sum(if_else({{ human_decision }}  == 1 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      human_ex_gpt_ex = sum(if_else({{ human_decision }}  == 0 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),

      accuracy = (human_ex_gpt_in + human_in_gpt_ex)/n_refs,
      p_agreement = 1 - accuracy,

      # Eq. 2 (Syriani et al., 2023)
      precision = human_in_gpt_in/(human_in_gpt_in + human_ex_gpt_in),
      # Eq. 3 (Syriani et al., 2023)
      recall = human_in_gpt_in / (human_in_gpt_in + human_in_gpt_ex),
      # Eq. 4 (Syriani et al., 2023)
      npv = human_ex_gpt_ex/(human_ex_gpt_ex + human_in_gpt_ex),
      # Eq. 5 (Syriani et al., 2023)
      specificity = human_ex_gpt_ex / (human_ex_gpt_ex + human_ex_gpt_in),

      .by = c(promptid, model, reps, top_p)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # Eq. 6 (Syriani et al., 2023)
      bacc = (recall + specificity)/2,
      # Eq. 7 (Syriani et al., 2023)
      F2 = 5 * ((precision*recall)/(4*(precision+recall))),
      # Eq. 8 (Syriani et al., 2023)
      nominator = (human_in_gpt_in*human_ex_gpt_ex)-(human_ex_gpt_in*human_in_gpt_ex),
      denominator = (
        2*sqrt((human_in_gpt_in+human_ex_gpt_in)*(human_in_gpt_in+human_in_gpt_ex)*
                 (human_ex_gpt_ex+human_ex_gpt_in)*(human_ex_gpt_ex+human_in_gpt_ex))
        ),

      mcc = nominator/denominator + 0.5,


      rm1 = human_ex_gpt_ex + human_in_gpt_ex,
      rm2 = human_ex_gpt_in + human_in_gpt_in,
      cm1 = human_ex_gpt_ex + human_ex_gpt_in,
      cm2 = human_in_gpt_ex + human_in_gpt_in,

      pe = 1/n_refs * ((cm1*rm1)/n_refs + (cm2*rm2)/n_refs),


      irr = (p_agreement - pe)/(1-pe),
      se_irr = sqrt( (p_agreement*(1-p_agreement)) / n_refs*(1-pe)^2),
      cl_irr = irr - 1.96 * se_irr,
      cu_irr = irr + 1.96 * se_irr,

      level_of_agreement = case_when(
        irr <= .2 ~ "None",
        irr > .2 & irr <= .39 ~ "Minimal",
        irr > .39 & irr <= .59 ~ "Weak",
        irr > .59 & irr <= .79 ~ "Moderate",
        irr > .79 & irr <= .90 ~ "Strong",
        irr != 1 & irr > .9 ~ "Almost perfect",
        irr == 1 ~ "Perfect",
        TRUE ~ NA_character_

      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(rm1:pe, nominator:denominator)) |>
    dplyr::relocate(n_refs, .before = n_screened)

  if (key_result) res <- res |> dplyr::select(promptid, model, reps, top_p, p_agreement, recall, specificity)

#  if (names(answer_data_aggregated) %in% x || is_gpt_agg_tbl(x)){
#
#  }

  res

}
