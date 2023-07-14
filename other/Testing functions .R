
AIscreenR::set_api_key(AIscreenR:::testing_key_chatgpt())
AIscreenR:::set_api_key()

body2 <- body <- list(
  model = "gpt-3.5-turbo-0613",
  messages = list(
    list(
      role = "user",
      content = "What is a carrot?"
    )
  ),
  functions = incl_function_simple,
  function_call = list(name = "inclusion_decision_simple"),
  top_p = 1
)

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
duration delivered to young people and their families). If no exclude the
abstract and write '0' followed by the
detailed response. 2) Are the participants in outpatient drug treatment primarily
for non-opioid drug use? 3) Are the participants within age 11–21?"

library(future)
plan(multisession)

test_dat <- tabscreen_gpt(
  data = FFT_dat[1:2,],
  prompt = prompt,
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-3.5-turbo-0613", "gpt-3.5-turbo-16k-0613"),
  max_tries = 1,
  reps = 2

); test_dat




#get_api_key()

q <- "What is a carrot?"
ask_chatgpt(
  q,
  sleep_time = 0,
  time_info = TRUE,
  reps = 1
)

#purrr::map_dfr(1:10, \(i) ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = TRUE))

library(future)

plan(sequential)

system.time(
  res_seq <- ask_chatgpt(
    q,
    time_info = TRUE,
    reps = 3
  )
)


plan(multisession, workers = 7)


system.time(
  res_par <- ask_chatgpt(
    q,
    time_info = TRUE,
    reps = 10
  )
)

res_par

#tib_dat <- tibble::tibble(answer = "Yes", run_time = 1.1, n = 1:3)
#
#tib_dat |>
#  tidyr::pivot_wider(
#    names_from = n,
#    names_glue = "{.value}_{n}",
#    values_from = c(answer, run_time)
#  )

fic_dat <- tibble::tibble(
  Studyid = 1:10,
  Title = paste("Text", 1:10),
  Abstract = paste("Abstract text", 1:10)
)

prompts <- paste("Prompt\n\ntext\n", 1:2)
text1 <- paste("First text", 1:10)
text2 <- paste("Second text", 1:10)

paste(
  prompts,
  "Some other text",
  text1,
  text2
)

#fic_dat |> dplyr::slice(rep(1:length(fic_dat), length(prompts))) |> dplyr::pull(abstract)


tab_dat <-
tabscreen_chatgpt(
 data = fic_dat,
 prompt = prompts,
 studyid = Studyid,
 title = Title,
 abstract = Abstract
); tab_dat
