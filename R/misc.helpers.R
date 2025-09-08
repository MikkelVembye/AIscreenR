
#' @importFrom utils globalVariables

# Manage dplyr behavior

utils::globalVariables(
  c("answer", "run_time", "n", "question", "question_raw", "res", "n_error",
    "still_error", "prompt_tokens", "completion_tokens", "model",
    "requests_per_minute", "tokens_per_minute",
    "input_price_dollar", "output_price_dollar", "price_total_dollar",
    "decision_gpt", "decision_binary", "detailed_description",
    "final_decision_gpt_num", "n_words_answer", "longest_answer", "final_decision_gpt",
    "req_per_min", "topp", "iterations", "input_price", "output_price", "max_reps",
    "promptid", "SE_kappa", "accuracy", "cm1", "cm2", "human_code", "human_in_gpt_ex", "human_ex_gpt_in", "n_ref",
    "n_refs", "n_screened", "human_ex_gpt_ex", "human_in_gpt_in", "p_agreement", "pe", "reps", "rm1", "rm2",
    "studyid", "top_p", "n_mis_answers", "screen_errros", "max_tries", "max_seconds", "irr", "se_irr", "cl_irr", "cu_irr",
    "level_of_agreement", "precision", "recall", "npv", "specificity", "bacc", "F2", "mcc", "denominator", "nominator",
    "model_prizes", "price_in_per_token", "price_out_per_token",
    "submodel", "prompt", "data", "tools", "tool_choice", "is.gpt", "criteria", "incl_p_cutoff", "incl_p", "title","supplementary","file_path","file_content","combination_idx","rep_num",
    "current_file_path","safe_basename","current_prompt","current_model",
    "current_rep_num","current_study_id","current_prompt_id",
    "current_vector_store_name","precomputed_supplementary","total_price_dollar"
    )
)


#----------------------------------------------------------------
#
#  Helpers
#
#----------------------------------------------------------------

status_code <- function(){

  resp <- httr2::last_response()

  if (!is.null(resp)){

    code <- resp |> httr2::resp_status()

  } else {

    code <- 999

  }

  code

}

# New
error_message <- function(){

  resp_last <- httr2::last_response()

  if (!is.null(resp_last)){

    code <- resp_last |> httr2::resp_status()

    error_body <- httr2::resp_body_json(resp_last)

    message <- paste0("Error ", code, ": ", error_body$error$message)


  } else {

    message <- "Error: Could not reach host"

  }

  message

}

# Old version (i.e., used in tabscreen_gpt.original)
status_code_text <- function(){

  resp_last <- httr2::last_response()

  check_string <- "[possibly overload on server - check https://platform.openai.com/docs/guides/error-codes]"

  if (!is.null(resp_last)){

    code <- resp_last |> httr2::resp_status()

    text <- paste("Error", code)

    if (code == 400) text <- paste("Error", code, "Bad request [check/clean body parameters]")
    if (code == 401) text <- paste("Error", code, "Unauthorized [invalid api]")
    if (code == 404) text <- paste("Error", code, "[check gpt model]")
    if (code == 429 | code == 500) text <- paste("Error", code, check_string)
    if (code == 503) text <- paste("Error", code, "Service Unavailable")

    s_code <- text

  } else {

    s_code <- "Error [could not reach host]"

  }

  s_code

}


#gpt_is_transient <- function(resp){
#  status_code() == 400 ||  status_code() == 429 || status_code() == 500 || status_code() ==  503
#}

gpt_is_transient <- function(resp){
  status_code() %in% c(429, 500:503)
}


# Function with encrypt code string

testing_key_chatgpt <- function() {
  httr2::secret_decrypt(
    "4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw",
    "AISCREENR_KEY"
  )
}

# Backup key
#testing_key_chatgpt <- function() {
#  httr2::secret_decrypt(
#    "2FCsUZ0-nA0Cf5h3Oqd72dunFxDf7sbQrC3OIsSaiI-DV4YsYICBMQzqwcgmOFiY6QIrfJbfPYexjW6T1BKKDC-VCg",
#    "AISCREENR_KEY"
#  )
#}

#----------------------------------------------------------------
#
#  Question needed to evaluate the gpt_engine
#
#----------------------------------------------------------------

question <- "Evaluate the following study based on the selection criteria for
a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use. A family-based intervention (FBT)
is equivalent to a behavior focused family therapy, where young peoples drug use
is understood in relation to family behavior problems. Family-based interventions
also includes manual-based family therapies as it targets young people and their
families as a system throughout treatment, and thereby recognizes the important
role of the family system in the development and treatment of young peoples drug
use problems. FBT was developed in the late 1980s on request from
the US National Institute on Drug Abuse (NIDA).The development
of FBT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the
environmentto reinforce non-alcohol associated activities. FBT developed to
have more emphasis oncontingency contracting, impulse control strategies specific
to drug use, and increased emphasis on involvement of family members in treatment.
FBT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FBT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions have
been enhanced to address several mental health related problems commonly occurringas
comorbid conditions in drug use treatment participant.
For each study, I would like you to assess: 1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or Behavioral Family Therapy?
(Outpatient manual-based interventions of any duration delivered to young people and their families).
If no to this answer exclude the study. 2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use? 3) Are the participants within age 11a\"21?
Now, evaluate the following titles and abstracts for Study 150:
Title: Treatment Outcomes for Adolescent Substance Abuse at 4 and 7 Month Assessments-Abstract:
This randomized clinical trial evaluated individual cognitive-behavioral therapy (CBT),
family therapy, combined individual and family therapy, and a group intervention for
114 substance-abusing adolescents. Outcomes were percentage of days marijuana was used and
percentage of youths achieving minimal use. Each intervention demonstrated some efficacy,
although differences occurred for outcome measured, speed of change, and maintenance of change.
From pretreatment to 4 months, significantly fewer days of use were found for the family therapy
alone and the combined interventions. Significantly more youths had achieved minimal use levels
in the family and combined conditions and in CBT. From pretreatment to 7 months,
reductions in percentage of days of use were significant for the combined and group interventions,
and changes in minimal use levels were significant for the family, combined, and group interventions."

#-----------------------------------------------------------------
# GROQ functions
#-----------------------------------------------------------------

.groq_is_transient <- function(resp) {
  status_code() %in% c(429, 500:503)
}

status_code_text_GROQ <- function() {
  resp_last <- httr2::last_response()
  check_string <- "[possibly overload on server]"
  
  if (!is.null(resp_last)) {
    code <- resp_last |> httr2::resp_status()
    text <- paste("Error", code)
    
    if (code == 400) text <- paste("Error", code, "Bad request [check/clean body parameters]")
    if (code == 401) text <- paste("Error", code, "Unauthorized [invalid api]")
    if (code == 404) text <- paste("Error", code, "[check model]")
    if (code == 429 | code == 500) text <- paste("Error", code, check_string)
    if (code == 503) text <- paste("Error", code, "Service Unavailable")
    
    s_code <- text
  } else {
    s_code <- "Error [could not reach host]"
  }
  s_code
}

# Function with encrypt code string
testing_key_groq <- function() {
  httr2::secret_decrypt(
    "4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw",
    "GROQ_KEY"
  )
}