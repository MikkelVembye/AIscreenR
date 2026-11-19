# Helper for tests that must never reach a real API.
#
# httr2::local_mocked_responses() answers every request with `mock(req)`, but
# httr2 does not record the mocked reply, while the screening engines read it
# with httr2::last_response(). This helper mocks both, for the rest of the
# test that calls it.
#
# `mock` is a function that gets the request and returns a response, e.g.
# httr2::response_json(status_code = 404, body = list(error = list(message = "x"))).
# The request is available as req$body$data (body), req$headers and req$url.
local_mock_api <- function(mock, env = parent.frame()) {
  last <- NULL
  httr2::local_mocked_responses(function(req) {
    last <<- mock(req)
    last
  }, env = env)
  testthat::local_mocked_bindings(
    last_response = function() last,
    .package = "httr2",
    .env = env
  )
}
