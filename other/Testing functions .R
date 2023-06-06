
AIscreenR:::set_api_key(
  key = httr2::secret_decrypt(
    "4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw",
    "AISCREENR_KEY"
  )
)

#get_api_key()

q <- "What is a carrot?"
ask_chatgpt(
  q,
  sleep_time = 0,
  time_info = TRUE,
  reps = 2
)

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
