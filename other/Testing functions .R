
AIscreenR::set_api_key(AIscreenR:::testing_key_chatgpt())
AIscreenR:::set_api_key()

#body2 <- body <- list(
#  model = "gpt-3.5-turbo-0613",
#  messages = list(
#    list(
#      role = "user",
#      content = "What is a carrot?"
#    )
#  ),
#  functions = incl_function_simple,
#  function_call = list(name = "inclusion_decision_simple"),
#  top_p = 1
#)
models <- c("gpt-3.5-turbo-0613", "gpt-4")
rl <- rate_limits_per_minute(model = models)

prompt <- "Evaluate the following study based on the selection criteria
for a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use.
A family-based intervention (FFT) is equivalent to a behavior focused
family therapy, where young people’s drug use is understood in relation to family
behavior problems. Family-based interventions also includes manual-based family therapies as
it targets young people and their families as a system throughout treatment, and thereby recognizes
the important role of the family system in the development and treatment of young people’s drug use problems.
FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse (NIDA).
The development of FFT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
contingency contracting, impulse control strategies specific to drug use,
and increased emphasis on involvement of family members in treatment.
FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions
have been enhanced to address several mental health related problems commonly occurring
as comorbid conditions in drug use treatment participant.  For each study,
I would like you to assess:  1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or
Behavioral Family Therapy? (Outpatient manual-based interventions of any
duration delivered to young people and their families). If not, exclude study.
2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use? 3) Are the participants within age 11–21?"

prompt2 <- "Evaluate the following study based on the selection criteria
for a systematic review on the effects of family-based interventions on drug abuse
reduction for young people in treatment for non-opioid drug use.
A family-based intervention (FFT) is equivalent to a behavior focused
family therapy, where young people’s drug use is understood in relation to family
behavior problems. Family-based interventions also includes manual-based family therapies as
it targets young people and their families as a system throughout treatment, and thereby recognizes
the important role of the family system in the development and treatment of young people’s drug use problems.
FFT was developed in the late 1980s on request from the US National Institute on Drug Abuse (NIDA).
The development of FFT was initially heavily inspired by the alcohol abuse program
Community Reinforcement Approach (CRA), which was aimed at restructuring the environment
to reinforce non-alcohol associated activities. FFT developed to have more emphasis on
contingency contracting, impulse control strategies specific to drug use,
and increased emphasis on involvement of family members in treatment.
FFT is designed to accommodate diverse populations of youths with a variety of behavioral,
cultural and individual preferences. FFT has evolved for use in severe behavioral disturbances
known to co-exist with substance use and dependence, and the core interventions
have been enhanced to address several mental health related problems commonly occurring
as comorbid conditions in drug use treatment participant.  For each study,
I would like you to assess:  1) Is the study about a family-based intervention,
such as Functional Family Therapy, Multidimensional Family Therapy, or
Behavioral Family Therapy? (Outpatient manual-based interventions of any
duration delivered to young people and their families). If not, exclude study.
2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use?"



app_obj <- approximate_price_gpt(
  data = filges2015_dat[c(1:150),],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613", "gpt-4"),
  reps = c(10, 1),
  top_p = c(0.2, 1)
)

app_obj

app_obj$price_data
app_obj$price_dollar

#CONTINUE HERE!

library(future)
plan(multisession)


test_obj <- tabscreen_gpt(
  data = filges2015_dat[c(1:2),],
  prompt = c(prompt),
  studyid = studyid, # indicate the variable with the studyid in the data
  title = title, # indicate the variable with the titles in the data
  abstract = abstract,
  api_key = 1234
  #functions = AIscreenR:::incl_function,
  #function_call_name = list(name = "inclusion_decision"),
  #max_tries = 0,
  #reps = 1 # Number of times the same question is asked to ChatGPT
  #max_tokens = 40
); test_obj

price_in_dollar <- test_obj$price_dollar
price_in_dollar

price_dat <- test_obj$price_dat
price_dat

all_dat <- test_obj$answer_data_all
all_dat

sum_dat <- test_obj$answer_data_sum
sum_dat

plan(sequential)

#all_dat$detailed_description


#is_chatgpt(test_obj)


#func_list_test = list(name = "inclusion_decision_simple")
#func_list_test$name == "inclusion_decision"


# Data manipulation

#if (length(model) > 1 && length(reps) > 1 && length(model) != length(reps)){
#  stop("model and reps must be of the same length.")
#}
#
#if (length(model) > 1 && length(rpm) > 1 && length(model) != length(rpm)){
#  stop("model and rpm must be of the same length.")
#}

model <- c("gpt-3.5-turbo")
reps <- c(10)
prompt <- paste("Dette er prompt", 1:5)
rpm <- c(3500)
top_p <- c(0.5, 1)
dat <- filges2015_dat[1:5,]

# mp = multiplier
mp_reps <- if (length(reps) > 1) 1 else length(model)
mp_rpm <- if (length(rpm) > 1) 1 else length(model)

reps_models <- tibble::tibble(model = model, reps = reps)

question_dat <-
  dat |>
  dplyr::mutate(
    dplyr::across(c(title, abstract), ~ dplyr::if_else(
      is.na(.x) | .x == "" | .x == " ", "No information", .x, missing = "No information")
    )
  ) |>
  ## Make promptid variable
  dplyr::slice(rep(1:nrow(dat), length(prompt))) |>
  dplyr::mutate(
    promptid = rep(1:length(prompt), each = dplyr::n_distinct(studyid)),
    prompt = rep(prompt, each = dplyr::n_distinct(studyid))
  ) |>
  dplyr::slice(rep(1:dplyr::n(), each = length(model))) |>
  dplyr::mutate(
    model = rep(model, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)),
    iterations = rep(reps, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)*mp_reps),
    req_per_min = rep(rpm, dplyr::n_distinct(studyid)*dplyr::n_distinct(prompt)*mp_rpm),
    question_raw = paste0(
      prompt,
      " Now, evaluate the following title and abstract for",
      " Study ", studyid, ":",
      " -Title: ", title,
      " -Abstract: ", abstract
    ),
    question = stringr::str_replace_all(question_raw, "\n\n", " "),
    question = stringr::str_remove_all(question, "\n")
  ) |>
  dplyr::select(-question_raw) |>
  dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
  mutate(
    topp = rep(top_p, n_distinct(studyid)*n_distinct(prompt)*n_distinct(model))
  ) |>
  dplyr::arrange(prompt, model, topp, studyid)

question_dat

gpt4_nrow <-
  question_dat |>
  filter(stringr::str_detect(model, "gpt-4")) |>
  nrow()




  summarise(
    max_reps = max(iterations, na.rm = TRUE)
  ) |>
  pull(max_reps)

# Approximate price

app_price_dat <-
  question_dat |>
  mutate(
    prompt_tokens = round(stringr::str_count(question, '\\w+') * 1.6),
    completion_tokens = 11 # Average number of completion tokens for the inclusion_decision_simple function

  ) |>
  filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
  dplyr::rowwise() |>
  mutate(

    input_price = case_when(
      any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(prompt_tokens * (0.0015/1000) * iterations, 4),
      any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(prompt_tokens * (0.003/1000) * iterations, 4),
      any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(prompt_tokens * (0.03/1000) * iterations, 4),
      any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(prompt_tokens * (0.06/1000) * iterations, 4),
      TRUE ~ NA_real_
    ),

    output_price = case_when(
      any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ completion_tokens * (0.002/1000) * iterations,
      any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ completion_tokens * (0.004/1000) * iterations,
      any(c("gpt-4", "gpt-4-0613") %in% model) ~ completion_tokens * (0.06/1000) * iterations,
      any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ completion_tokens * (0.12/1000) * iterations,
      TRUE ~ NA_real_
    )

  ) |>
  ungroup() |>
  summarise(

    iterations = unique(iterations),
    input_price_dollar = sum(input_price, na.rm = TRUE),
    output_price_dollar = sum(output_price, na.rm = TRUE),
    total_price_dollor = input_price_dollar + output_price_dollar,

    .by = model

  ); app_price_dat

app_price <- round(sum(app_price_dat$total_price_dollor, na.rm = TRUE), 4)
app_price


#filter(!is.na(prompt_tokens) | !is.na(completion_tokens)) |>
#  summarise(
#
#    input_price_dollar = case_when(
#      any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.0015/1000), 4),
#      any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.003/1000), 4),
#      any(c("gpt-4", "gpt-4-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.03/1000), 4),
#      any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ round(sum(prompt_tokens, na.rm = TRUE) * (0.06/1000), 4),
#      TRUE ~ NA_real_
#    ),
#
#
#    output_price_dollar = case_when(
#      any(c("gpt-3.5-turbo", "gpt-3.5-turbo-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.002/1000),
#      any(c("gpt-3.5-turbo-16k", "gpt-3.5-turbo-16k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.004/1000),
#      any(c("gpt-4", "gpt-4-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.06/1000),
#      any(c("gpt-4-32k", "gpt-4-32k-0613") %in% model) ~ sum(completion_tokens, na.rm = TRUE) * (0.12/1000),
#      TRUE ~ NA_real_
#    ),
#
#    #input_price_dollar = input_price_dollar,
#    #output_price_dollar = output_price_dollar,
#    #price_total_dollar = input_price_dollar + output_price_dollar,
#
#    .by = model
#
#  )


