


test_that("Testthat works in get_api_key()", {

  #skip("To avoid GHA-failure")
  key <- httr2::secret_decrypt("4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw", "AISCREENR_KEY")
  expect_equal(get_api_key(), key)

})


test_that("AISCREENR_KEY works and secret_decrypt behaves properly",{

  #skip("Cannot get the Action secret to work when pushed to GitHub")

  key <- httr2::secret_decrypt("4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw", "AISCREENR_KEY")
  AIscreenR:::set_api_key(key = key)

  q <- "What is a carrot?"
  answer_dat <- ask_chatgpt(q, time_info = TRUE)


  expect_true(is.numeric(answer_dat$run_time))

})
