test_that("Testthat works in get_api_key()", {

  skip_on_cran()

  key <- httr2::secret_decrypt("4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw", "AISCREENR_KEY")
  expect_equal(get_api_key(), key)

  expect_identical(is.character(get_api_key()), TRUE)

  set_api_key(key = key)
  expect_identical(Sys.getenv("CHATGPT_KEY"), key)

})

test_that("get_api_key() error.",{

  expect_error(
    get_api_key(env_var = "something_else")
  )

})

test_that("get_api_key_groq() error. ", {

  expect_error(
    get_api_key_groq(env_var = "something_else")
  )

})