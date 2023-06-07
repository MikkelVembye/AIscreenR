

test_that("API call and reps behaves properly",{


  key <- httr2::secret_decrypt("4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw", "AISCREENR_KEY")
  set_api_key(key)

  q <- "What is a carrot?"
  answer_dat <- ask_chatgpt(q, time_info = TRUE, rep = 2)


  expect_true(is.numeric(answer_dat$run_time))
  expect_equal(nrow(answer_dat), 2)

})
