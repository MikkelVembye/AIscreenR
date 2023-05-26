
q <- "What is a carrot?"
ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = TRUE, reps = 2)

#purrr::map_dfr(1:10, \(i) ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = TRUE))

library(future)

plan(sequential)

system.time(
  res_seq <- ask_chatgpt(
    q,
    api_key = api_key,
    sleep_time = 0,
    time_info = TRUE,
    reps = 3
  )
)


plan(multisession, workers = 7)


system.time(
  res_par <- ask_chatgpt(
    q,
    api_key = api_key,
    sleep_time = 0,
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
