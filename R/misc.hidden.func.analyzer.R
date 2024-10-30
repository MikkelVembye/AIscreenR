.calc_perform <- function(data, hum_decision){

  perform_res <-
    data |>
    dplyr::mutate(n_ref = n_distinct(studyid)) |>
    dplyr::filter(!is.na(final_decision_gpt_num)) |>
    summarise(
      n_screened = n(),
      n_refs = unique(n_ref),
      n_missing = n_refs - n_screened,
      human_in_gpt_ex = sum(if_else({{ hum_decision }} == 1 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),
      human_ex_gpt_in = sum(if_else({{ hum_decision }}  == 0 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      human_in_gpt_in = sum(if_else({{ hum_decision }}  == 1 & final_decision_gpt_num == 1, 1, 0, missing = NA), na.rm = TRUE),
      human_ex_gpt_ex = sum(if_else({{ hum_decision }}  == 0 & final_decision_gpt_num == 0, 1, 0, missing = NA), na.rm = TRUE),

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

  perform_res

}
