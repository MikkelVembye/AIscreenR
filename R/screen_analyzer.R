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
#'  \bold{reps}  \tab \code{integer}  \tab indicating the number of times the same question was sent to GPT server. \cr
#'  \bold{top_p} \tab \code{numeric}  \tab indicating the applied top_p. \cr
#'  \bold{n_screened} \tab \code{integer} \tab indicating the number of screened references. \cr
#'  \bold{n_missing} \tab \code{numeric} \tab indicating the number of missing responses. \cr
#'  \bold{n_refs} \tab \code{integer} \tab indicating the total number of references expected to be screened for the given condition. \cr
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

  # Stopping rules
  if (all(!is_chatgpt(x), !is_chatgpt_tbl(x), !is_gpt(x), !is_gpt_tbl(x), !is_gpt_agg_tbl(x))){
    stop("The object must be of either class 'gpt, 'gpt_tbl', 'gpt_agg_tbl', 'chatgpt' or 'chatgpt_tbl'.")
  }

  # Preparing data
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

  # Ensuring human decision variable
  hum_decision_name <- as.character(substitute(human_decision))

  if(!hum_decision_name %in% names(dat)) stop("You must have an variable with the human decision to use this function.")

  # Calculating results performance
  res <- .calc_perform(dat, hum_decision = {{ human_decision }})

  if (key_result) res <- res |> dplyr::select(promptid, model, reps, top_p, p_agreement, recall, specificity)

  if ("answer_data_aggregated" %in% names || is_gpt_agg_tbl(x)){

    res_p_incl <- purrr::map(1:10/10, ~ {
      dat |>
        dplyr::filter(reps > 1) |>
        dplyr::mutate(
          final_decision_gpt = dplyr::if_else(incl_p >= .x, "Include", "Exclude"),
          final_decision_gpt_num = dplyr::if_else(final_decision_gpt == "Include", 1, 0)
        ) |>
        .calc_perform(hum_decision = {{ human_decision }}) |>
        dplyr::mutate(
          incl_p = .x,
          criteria = dplyr::if_else(
            .x != 1,
            paste0("Studies have been included in at least ", .x*100, "% of the ", reps, " screenings."),
            paste0("Studies have been included in all of the ", reps, " screenings.")
          )
        )

    }) |>
      purrr::list_rbind()

    if (key_result) {
      res_p_incl <-
        res_p_incl |>
        dplyr::select(promptid, model, reps, top_p, p_agreement, recall, specificity, incl_p, criteria)
    }

    attr(res, "p_incl_data") <- res_p_incl

  }

  if (is_gpt_agg_tbl(dat)) {

   p_incl_cut <- attr(dat, "incl_cutoff_lower")

    res <-
      res |>
      dplyr::mutate(
        incl_p_cutoff = dplyr::if_else(reps == 1, NA_real_, p_incl_cut),
        criteria = dplyr::case_when(
          incl_p_cutoff == 1 ~ paste0("Studies have been included in all of the ", reps, " screenings."),
          incl_p_cutoff != 1 ~ paste0("Studies have been included in at least ", incl_p_cutoff * 100, "% of the ", reps, " screenings."),
          is.na(incl_p_cutoff) ~ "One screening used.",
          .default = NA_character_
        )
      ) |>
      dplyr::rename(incl_p = incl_p_cutoff)

 }


  res

}
