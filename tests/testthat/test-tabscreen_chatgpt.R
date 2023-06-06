test_that("Testthat works in get_api_key()", {

  expect_equal(get_api_key(), httr2::secret_decrypt("4UAcFSIHVz8Z4zED1WEj3k65xFBWlJ8dzavRDGG4dz0pBxEOXtvSkLwK6_fZaZqCr94oVtKBD6DQo82vwa2gljJMTw", "AISCREENR_KEY"))

})
