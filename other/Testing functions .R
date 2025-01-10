
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
  data = filges2015_dat,
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613", "gpt-4"),
  reps = c(10, 1),
  top_p = c(0.2, 1)
)


app_obj <- approximate_price_gpt(
  data = filges2015_dat[c(1:20),],
  prompt = prompt,
  #studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613", "gpt-4"),
  reps = 1
)

app_obj

app_obj$price_data
app_obj$price_dollar

#CONTINUE HERE!

library(future)
plan(multisession)


test_obj <- tabscreen_gpt.tools(
  data = filges2015_dat[c(1:2),],
  prompt = c(prompt),
  studyid = studyid, # indicate the variable with the studyid in the data
  title = title, # indicate the variable with the titles in the data
  abstract = abstract,
  model = c("gpt-4o"),
  reps = 1
  #top_p = c(0.2, 1),
  #functions = AIscreenR:::incl_function,
  #function_call_name = list(name = "inclusion_decision")
  #max_tries = 12
  #max_tokens = 40
)

#screen_errors(); print(test_obj)

price_in_dollar <- test_obj$price_dollar
price_in_dollar

price_dat <- test_obj$price_dat
price_dat

all_dat <- test_obj$answer_data_all
all_dat

sum_dat <- test_obj$answer_data_sum
sum_dat

if ("error_data" %in% names(test_obj)){
error_dat <- test_obj$error_data
error_dat
}

plan(sequential)

#rescreen_errors <- function(object){
#
#  if (!is_chatgpt(x)) stop("Find framing in metafor")
#
#}

x <- AIscreenR:::result_object

library(future)
plan(multisession)
rescreening <- x |> screen_errors()
plan(sequential)

x |> screen_analyzer() |> print(width = 200)

#error_test_dat <- x$error_data |> dplyr::filter(stringr::str_detect(decision_gpt, "future_map failied") | is.na(n))
#is_chatgpt_tbl(error_test_dat)

#plan(multisession)
test_err_dat <- screen_errors(x)
#plan(sequential)
test_err_dat

error_test_dat2 <- x$error_data |> dplyr::filter(stringr::str_detect(decision_gpt, "rror") & !is.na(n))
is_chatgpt_tbl(error_test_dat2)


test_error_obj <- tabscreen_gpt(error_test_dat2, api_key = 1234)
test_error_obj

FFT_res2 <- readRDS("R:/Chatgpt in R/FFT/FFT_res2.rds")

library(future)
plan(multisession)
FFT_no_err <-
  FFT_res2 |>
  screen_errors()
plan(sequential)

plan(multisession)
FFT_no_err <-
  FFT_no_err |>
  screen_errors()
plan(sequential)

FFT_res2$error_data |>
  filter(stringr::str_detect(decision_gpt, "400")) |>
  pull(studyid) |>
  unique()

recovered <- FFT_res2$error_data |>
  filter(stringr::str_detect(decision_gpt, "400")) |>
  mutate(
    question = base::gsub("<.*?>", "", question),
    question = stringr::str_remove_all(question, "[:punct:]|[:symbol:]"),
    question = stringr::str_remove_all(question, "\\&")
  )

filges2015_dat
dat <- filges2015_dat[c(1:5, 261:265),]
prompt <- "Is this study about functional family therapy"
dat <- generate_ft_data(data = dat, prompt = prompt, studyid = studyid, title = title, abstract = abstract)
dat

dat <- dat |> transform(true_answer = ifelse(human_code == 1, "Include", "Exclude"))

dat <- filges2015_dat[c(1:5, 261:265),]

prompt <- "Is this study about functional family therapy?"

ft_dat <-
  generate_ft_data(
    data = dat,
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract
  ) |>
  dplyr::mutate(true_answer = dplyr::if_else(human_code == 1, "Include", "Exclude"))

role_subject <- paste0(
  "Act as a systematic reviewer that is screening study titles and",
  "abstracts for your systematic reviews regarding the the effects of family-based",
  " interventions on drug abuse reduction for young people in treatment for non-opioid drug use"
)

# Saving data in jsonl format (required format by OpenAI)
write_ft_data(
  data = ft_dat,
  role_and_subject = role_subject,
  file = "fine_tune_data.jsonl"
)

