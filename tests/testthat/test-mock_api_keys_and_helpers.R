test_that("get_api_key*() read environment variables", {
  withr::local_envvar(
    OPENAI_API_KEY = "openai-key", CHATGPT_KEY = "", GROQ_API_KEY = "groq-key",
    MISTRAL_API_KEY = "mistral-key", GEMINI_API_KEY = "gemini-key", ANTHROPIC_API_KEY = "claude-key"
  )
  expect_equal(get_api_key(), "openai-key")
  expect_equal(get_api_key_groq(), "groq-key")
  expect_equal(get_api_key_mistral(), "mistral-key")
  expect_equal(get_api_key_gemini(), "gemini-key")
  expect_equal(get_api_key_anthropic(), "claude-key")
})

test_that("get_api_key() falls back to CHATGPT_KEY", {
  withr::local_envvar(OPENAI_API_KEY = "", CHATGPT_KEY = "legacy-key")
  expect_equal(get_api_key(), "legacy-key")
})

test_that("get_api_key*() use the testing keys while testing", {
  withr::local_envvar(
    OPENAI_API_KEY = "", CHATGPT_KEY = "", GROQ_API_KEY = "", MISTRAL_API_KEY = "",
    GEMINI_API_KEY = "", ANTHROPIC_API_KEY = "", TESTTHAT = "true"
  )
  local_mocked_bindings(
    testing_key_chatgpt = \() "t-openai", testing_key_groq = \() "t-groq",
    testing_key_mistral = \() "t-mistral", testing_key_gemini = \() "t-gemini",
    testing_key_anthropic = \() "t-claude"
  )
  expect_equal(get_api_key(), "t-openai")
  expect_equal(get_api_key(env_var = "CHATGPT_KEY"), "")
  expect_equal(get_api_key_groq(), "t-groq")
  expect_equal(get_api_key_mistral(), "t-mistral")
  expect_equal(get_api_key_gemini(), "t-gemini")
  expect_equal(get_api_key_anthropic(), "t-claude")
})

test_that("get_api_key*() fail without a key outside of tests", {
  withr::local_envvar(
    OPENAI_API_KEY = "", CHATGPT_KEY = "", GROQ_API_KEY = "", MISTRAL_API_KEY = "",
    GEMINI_API_KEY = "", ANTHROPIC_API_KEY = "", TESTTHAT = "false"
  )
  expect_error(get_api_key(), "No API key found")
  expect_error(get_api_key_groq(), "No API key found")
  expect_error(get_api_key_mistral(), "No API key found")
  expect_error(get_api_key_gemini(), "No API key found")
  expect_error(get_api_key_anthropic(), "No API key found")
})

test_that("get_api_key*() reject unknown variables", {
  expect_error(get_api_key_mistral("x"), "MISTRAL_API_KEY")
  expect_error(get_api_key_gemini("x"), "GEMINI_API_KEY")
  expect_error(get_api_key_anthropic("x"), "ANTHROPIC_API_KEY")
})

test_that("set_api_key() sets requested variables", {
  withr::local_envvar(OPENAI_API_KEY = NA, CHATGPT_KEY = NA)
  set_api_key("abc")
  expect_equal(Sys.getenv("OPENAI_API_KEY"), "abc")
  set_api_key("def", env_var = "CHATGPT_KEY")
  expect_equal(Sys.getenv("CHATGPT_KEY"), "def")

  local_mocked_bindings(askpass = \(prompt) "typed-key", .package = "askpass")
  set_api_key()
  expect_equal(Sys.getenv("OPENAI_API_KEY"), "typed-key")
})

test_that("status_code() and error_message() describe the last response", {
  local_mocked_bindings(last_response = \() NULL, .package = "httr2")
  expect_equal(status_code(), 999)
  expect_equal(error_message(), "Error: Could not reach host")
  expect_equal(status_code_text(), "Error [could not reach host]")
  expect_equal(status_code_text_GROQ(), "Error [could not reach host]")

  resp <- json_resp(list(error = list(message = "bad key")), 401L)
  local_mocked_bindings(last_response = \() resp, .package = "httr2")
  expect_equal(status_code(), 401)
  expect_equal(error_message(), "Error 401: bad key")

  resp <- json_resp(list(message = "top level"), 400L)
  expect_equal(error_message(), "Error 400: top level")

  resp <- json_resp(list(other = "x"), 400L)
  expect_match(error_message(), "^Error 400")

  resp <- text_resp("<html>Gateway down</html>", 502L)
  expect_equal(error_message(), "Error 502: <html>Gateway down</html>")

  resp <- httr2::response(status_code = 500L, headers = list(`Content-Type` = "application/json"), body = charToRaw("{not json"))
  expect_equal(error_message(), "Error 500: [Could not parse error details]")
})

test_that("status_code_text*() map HTTP codes to messages", {
  for (fun in list(status_code_text, status_code_text_GROQ)) {
    for (code in c(400, 401, 404, 429, 500, 503, 418)) {
      resp <- json_resp(list(), code)
      local_mocked_bindings(last_response = \() resp, .package = "httr2")
      expect_match(fun(), paste("Error", code))
    }
  }
})

test_that("transient helpers flag retryable statuses", {
  for (code in c(200, 401, 429, 500, 503)) {
    resp <- json_resp(list(), code)
    local_mocked_bindings(last_response = \() resp, .package = "httr2")
    expect_equal(gpt_is_transient(resp), code %in% c(429, 500:503))
    expect_equal(.groq_is_transient(resp), code %in% c(429, 500:503))
  }
})

test_that("testing key helpers decrypt with the testing secret", {
  local_mocked_bindings(secret_decrypt = \(x, key) paste("decrypted", key), .package = "httr2")
  expect_equal(testing_key_chatgpt(), "decrypted AISCREENR_KEY")
  expect_equal(testing_key_groq(), "decrypted GROQ_KEY")
  expect_equal(testing_key_mistral(), "decrypted MISTRAL_KEY")
  expect_equal(testing_key_gemini(), "decrypted GEMINI_KEY")
  expect_equal(testing_key_anthropic(), "decrypted ANTHROPIC_KEY")
})

test_that("rate_limits_per_minute() reads rate limit headers", {
  headers <- function(...) {
    httr2::response(
      status_code = 200L, headers = list(`Content-Type` = "application/json", ...), body = charToRaw("{}")
    )
  }

  local_mock_api(\(req) headers(`x-ratelimit-limit-requests` = "500", `x-ratelimit-limit-tokens` = "30000"))
  res <- rate_limits_per_minute(model = c("gpt-5-nano", "gpt-5-mini"), api_key = "k")
  expect_equal(res$requests_per_minute, c(500, 500))
  expect_equal(res$tokens_per_minute, c(30000, 30000))

  groq_model <- AIscreenR::groq_model_prizes$model[1]
  res <- rate_limits_per_minute(model = groq_model, AI_tool = "Groq", api_key = "k")
  expect_equal(res$requests_per_minute, 500)

  local_mock_api(\(req) headers(`x-ratelimit-limit-req-minute` = "60", `x-ratelimit-limit-tokens-minute` = "1000"))
  mistral_model <- AIscreenR::mistral_model_prizes$model[1]
  res <- rate_limits_per_minute(model = mistral_model, AI_tool = "Mistral", api_key = "k")
  expect_equal(res$tokens_per_minute, 1000)
})

test_that("rate_limits_per_minute() reports failures and validates input", {
  local_mock_api(llm_handler("gpt", status = 401L))
  expect_match(rate_limits_per_minute(api_key = "k")$model, "401")

  local_mock_api(llm_handler("gpt"), internet = FALSE)
  expect_match(rate_limits_per_minute(api_key = "k")$model, "internet")

  expect_error(rate_limits_per_minute(model = "nope", AI_tool = "Groq", api_key = "k"), "Unknown model")
  expect_error(rate_limits_per_minute(model = "nope", AI_tool = "Mistral", api_key = "k"), "Unknown model")
  expect_error(rate_limits_per_minute(AI_tool = "Gemini"), "Google Cloud")
  expect_error(rate_limits_per_minute(AI_tool = "Claude"), "Anthropic")
  expect_error(rate_limits_per_minute(AI_tool = "Bard"), "AI_tool must be")
  expect_error(rate_limits_per_minute(AI_tool = "Bard", api_key = "k"), "AI_tool must be")
})

test_that("rate_limits_per_minute() picks the default key for each tool", {
  withr::local_envvar(OPENAI_API_KEY = "o", GROQ_API_KEY = "g", MISTRAL_API_KEY = "m")
  local_mock_api(llm_handler("gpt", status = 401L))
  expect_match(rate_limits_per_minute()$model, "401")
  expect_match(rate_limits_per_minute(model = AIscreenR::groq_model_prizes$model[1], AI_tool = "Groq")$model, "401")
  expect_match(rate_limits_per_minute(model = AIscreenR::mistral_model_prizes$model[1], AI_tool = "Mistral")$model, "401")
})
