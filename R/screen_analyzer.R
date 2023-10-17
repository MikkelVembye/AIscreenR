#' @title Analyzing screening performance between the human and AI screening.
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' When both the human and AI title and abstract screening has been done, this function
#' allows you to calculate performance measures of the screening, including the overall
#' accuracy, specificity and sensitivity of the screening, as well as
#' interrater reliability kappa statistics.
#'
#'
#' @param x Either an object of class `'chatgpt'` or a data set of class `'chatgpt_tbl'`
#' @param human_decision Indicate the variable in the data that contains the human_decision.
#' This variable must be numeric containing 1 (for included references) and 0 (for excluded references) only.
#'
#' @return An `tibble` with performance measures
#'
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' x <- AIscreenR:::result_object
#' screen_analyzer(x)
#' }

screen_analyzer <- function(x, human_decision = human_code){

  if (all(!is_chatgpt(x), !is_chatgpt_tbl(x))) stop("The object must be of either class 'chatgpt' or 'chatgpt_tbl'.")

  if (is_chatgpt(x)) dat <- x$answer_data_sum
  if (is_chatgpt_tbl(x)) dat <- x

  hum_decision_name <- as.character(substitute(human_decision))

  if(!hum_decision_name %in% names(dat)) stop("You must have an variable with the human decision to use this function.")

  res <-
    dat |>
    mutate(n_ref = n_distinct(studyid)) |>
    filter(!is.na(final_decision_gpt_num)) |>
    summarise(
      n_screened = n(),
      n_missing = unique(n_ref) - n_screened,
      n_refs = unique(n_ref),
      n_false_ex = sum(if_else({{ human_decision }} == 1 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),
      n_false_in = sum(if_else({{ human_decision }}  == 0 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      n_true_in = sum(if_else({{ human_decision }}  == 1 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      n_true_ex = sum(if_else({{ human_decision }}  == 0 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),

      accuracy = (n_false_in + n_false_ex)/n_refs,
      p_agreement = 1 - accuracy,
      sensitivity = n_true_in / (n_true_in + n_false_ex),

      specificity = n_true_ex / (n_true_ex + n_false_in),

      rm1 = n_true_ex + n_false_ex,
      rm2 = n_false_in + n_true_in,
      cm1 = n_true_ex + n_false_in,
      cm2 = n_false_ex + n_true_in,

      pe = 1/n_refs * ((cm1*rm1)/n_refs + (cm2*rm2)/n_refs),


      kappa = (p_agreement - pe)/(1-pe),
      # Add interpreation of kappa her
      level_of_agreement = case_when(
        kappa <= .2 ~ "None",
        kappa > .2 & kappa <= .39 ~ "Minimal",
        kappa > .39 & kappa <= .59 ~ "Weak",
        kappa > .59 & kappa <= .79 ~ "Moderate",
        kappa > .79 & kappa <= .90 ~ "Strong",
        kappa > .9 ~ "Almost perfect",
        TRUE ~ NA_character_

      ),

      SE_kappa = sqrt( (p_agreement*(1-p_agreement)) / n_refs*(1-pe)^2),
      CL_kappa = kappa - 1.96 *SE_kappa,
      CU_kappa = kappa + 1.96 *SE_kappa,

      .by = c(promptid, model, reps, top_p)
    ) |>
    select(-c(rm1:pe))


  res

}
