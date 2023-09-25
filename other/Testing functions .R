
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
purrr::map(models, ~ rate_limits_per_minute(model = .x )) |>
  purrr::list_rbind()

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

app_obj <- approximate_price_gpt(
  data = FFT_dat[c(1:150),],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-4"),
  rep = 120
)

app_obj



library(future)
plan(multisession)


test_obj <- tabscreen_gpt(
  data = FFT_dat[c(149:150),],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613"), # "gpt-3.5-turbo-16k-0613", "gpt-4"
  #functions = AIscreenR:::incl_function,
  #function_call_name = list(name = "inclusion_decision"),
  max_tries = 1,
  reps = 2,
  messages = TRUE
  #incl_cutoff_upper = 0.5,
  #incl_cutoff_lower = 0.3
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

