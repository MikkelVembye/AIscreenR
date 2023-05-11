
q <- "What is a carrot?"
ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = FALSE)

purrr::map_dfr(1:10, \(i) ask_chatgpt(q, api_key = api_key, sleep_time = 0, time_info = TRUE))
