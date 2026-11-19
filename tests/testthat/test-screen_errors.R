# Tests for screen_errors() (R/screen_errors.R)
#
# Every test below makes screen_errors() stop with an error BEFORE it calls
# tabscreen_gpt()/tabscreen_groq(), so no request is ever sent to an API.

# Object with the columns screen_errors() needs, but no prompt anywhere.
make_obj <- function(error_data, cls = "gpt") {
  structure(list(error_data = error_data), class = cls)
}

ok_cols <- data.frame(studyid = 1, title = "t", abstract = "a")

test_that("screen_errors() stops for unknown and unsupported backends", {
  expect_error(
    screen_errors(make_obj(ok_cols, cls = "something")),
    "Unknown object backend"
  )

  expect_error(
    screen_errors(make_obj(ok_cols, cls = "ollama")),
    "Rescreening not supported for Ollama"
  )
})

test_that("screen_errors() finds the backend from api_url", {
  obj <- make_obj(ok_cols, cls = "something")

  attr(obj, "arg_list") <- list(api_url = "http://localhost:11434/api/chat")
  expect_error(screen_errors(obj), "Rescreening not supported for Ollama")

  # A known url gets past the backend check and stops at the next one
  attr(obj, "arg_list") <- list(api_url = "https://api.openai.com/v1/responses")
  expect_error(screen_errors(obj), "could not find the original prompt")
})

test_that("screen_errors() needs a studyid column", {
  expect_error(
    screen_errors(make_obj(data.frame(title = "t", abstract = "a"))),
    "needs a 'studyid' column"
  )
})

test_that("screen_errors() needs title and abstract columns it can find", {
  odd <- data.frame(studyid = 1, tt = "t", aa = "a")

  expect_error(screen_errors(make_obj(odd)), "could not infer title/abstract")

  # Naming the columns explicitly gets past that check and stops at the prompt
  expect_error(
    screen_errors(make_obj(odd), title = tt, abstract = aa),
    "could not find the original prompt"
  )
})

test_that("screen_errors() infers common column names, ignoring case", {
  alias <- data.frame(ID = 1, TI = "t", AB = "a")

  # No title/abstract error means the columns were found; it stops at the prompt
  expect_error(screen_errors(make_obj(alias)), "could not find the original prompt")
})
