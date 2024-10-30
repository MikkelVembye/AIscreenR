# Testing new tabscreen function

test_dat <-
  filges2015_dat[c(1:3, 241:243, 251),]

paths <- system.file("extdata", "test_prompts.rds", package = "AIscreenR")

prompts <- readRDS(paths)

library(future)
plan(multisession)

return_dat <-
  tabscreen_gpt(
  data = test_dat,
  prompt = prompts[1],
  studyid = studyid,
  title = title,
  abstract = abstract,
  model = c("gpt-4o-mini"),
  top_p = 1,
  rpm = 10000,
  reps = 5,
  #messages = FALSE,
  decision_description = FALSE,
  progress = TRUE,
  #tools = tools_detailed,
  #tool_choice = "inclusion_decision",
  #incl_cutoff_upper = 0.5,
  #incl_cutoff_lower = 40,
  #token_info = TRUE,
  force = TRUE,
  fine_tuned = TRUE
); return_dat

plan(sequential)

#return_dat2 <- return_dat |> screen_errors()

analyze_dat <- return_dat |> screen_analyzer(key_result = FALSE)
analyze_dat |> attr("p_incl_data") |> View()

prompt <- "Is this study about a Functional Family Therapy (FFT) intervention?"

test_x <-
  tabscreen_gpt(
    data = filges2015_dat[1:4,],
    prompt = prompt,
    studyid = studyid,
    title = title,
    abstract = abstract,
    decision_description = FALSE,
    model = "gpt-4o-mini",
    reps = 10
 )



# Create body for test

tools_choice_name <- list(
  type = "function",
  "function" = list(
    name = "inclusion_decision_simple"
  )
)

body1 <- list(
  model = "o1-mini",
  messages = list(list(
    role = "user",
    content = question
  )),
  tools = tools_simple,
  tool_choice = tools_choice_name,
  top_p = 1
)

#debugonce(gpt_engine)

.gpt_engine(
  body = body1,
  RPM = 10000,
  timeinf = T,
  tokeninf = T,
  key = get_api_key(),
  max_t = 4,
  max_s = 4,
  is_trans = gpt_is_transient,
  back = NULL,
  aft = NULL
)


library(future)
plan(multisession)

furrr::future_map_dfr(
  1:20, \(i) .gpt_engine(
    body = body1,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key(),
    max_t = 4,
    max_s = 4,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL
  ),
  .options = furrr::furrr_options(seed = TRUE)
)

.rep_gpt_engine(
  question = question,
  model_gpt = "gpt-4o-mini",
  topp = 1,
  role_gpt = "user",
  tool = tools_simple,
  t_choice = "inclusion_decision_simple",
  iterations = 10,
  req_per_min = 10000,
  seeds = NULL,
  time_inf = T,
  token_inf = T,
  apikey = get_api_key(),
  maxt = 4,
  maxs = 10,
  istrans = gpt_is_transient,
  ba = NULL,
  af = NULL
)

plan(sequential)

ex_dat <- filges2015_dat[1:200,] |> as.data.frame()
## Alternative sampling function
# shuffle abstracts randomly to allow random selection
df_shuffle <- df[sample(nrow(ex_dat)), ]

# subset 10 of the records
random_selection <- sample(nrow(df_shuffle), floor(nrow(df_shuffle) * 0.1))

# shuffle random selection
df_randomSelection <- df_shuffle[random_selection,  ]




