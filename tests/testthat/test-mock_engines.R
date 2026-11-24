url <- "https://mock.test/v1"

chat_body <- function(tools = tools_simple_groq, choice = "inclusion_decision_simple") {
  list(
    model = "m", messages = list(list(role = "user", content = "q")), tools = tools,
    tool_choice = list(type = "function", `function` = list(name = choice))
  )
}

groq_engine <- function(raw, body = chat_body(), status = 200L, token_inf = TRUE, time_inf = TRUE, max_t = 1) {
  local_mock_api(\(req) json_resp(raw, status), .env = parent.frame())
  suppressMessages(.groq_engine(
    body = body, RPM = 1000, time_inf = time_inf, token_inf = token_inf, api_key = "k",
    max_t = max_t, max_s = NULL, is_trans = .groq_is_transient, back = NULL, aft = NULL, endpoint_url = url
  ))
}

chat_msg <- function(message) list(choices = list(list(message = message)), usage = list(prompt_tokens = 3, completion_tokens = 2))
chat_content <- function(x) chat_msg(list(content = x))
tool_call <- function(args, type = "function") {
  list(tool_calls = list(list(type = type, `function` = list(arguments = args))))
}

test_that(".groq_engine() parses tool calls and respects output flags", {
  res <- groq_engine(chat_msg(tool_call('{"decision_gpt":"1"}')), token_inf = FALSE, time_inf = FALSE)
  expect_equal(names(res), c("decision_gpt", "decision_binary"))
  expect_equal(res$decision_binary, 1)

  res <- groq_engine(chat_msg(tool_call('{"decision_gpt":"0"}')))
  expect_equal(res$decision_binary, 0)
  expect_equal(res$prompt_tokens, 3)

  res <- groq_engine(chat_msg(tool_call('{"decision_gpt":"1"}')), max_t = 0)
  expect_equal(res$decision_binary, 1)
})

test_that(".groq_engine() reports malformed tool calls", {
  expect_match(groq_engine(chat_msg(tool_call("{bad json")))$decision_gpt, "Failed to parse tool call")
  expect_match(groq_engine(chat_msg(list(tool_calls = list(list(type = "function", `function` = list(name = "f"))))))$decision_gpt, "Unexpected tool_call")
  expect_match(groq_engine(chat_msg(tool_call("{}", type = "other")))$decision_gpt, "Unexpected tool_call")
  expect_match(groq_engine(chat_msg(list(role = "assistant")))$decision_gpt, "No tool_calls and no content")
})

test_that(".groq_engine() falls back to message content", {
  detailed <- chat_body(tools_detailed_groq, "inclusion_decision")
  conf <- chat_body(tools_simple_groq_conf)

  res <- groq_engine(chat_content('{"decision_gpt":"1","confidence":80}'), body = conf)
  expect_equal(c(res$decision_binary, res$confidence), c(1, 80))

  res <- groq_engine(chat_content('{"decision":"0"}'))
  expect_equal(res$decision_binary, 0)

  expect_match(groq_engine(chat_content('{"other":"0"}'))$decision_gpt, "not in content")
  expect_match(groq_engine(chat_content("plain text"))$decision_gpt, "Failed to parse content")

  for (key in c("detailed_description", "description", "reasoning", "explanation")) {
    content <- jsonlite::toJSON(stats::setNames(list("1", "why"), c("decision_gpt", key)), auto_unbox = TRUE)
    res <- groq_engine(chat_content(as.character(content)), body = detailed)
    expect_equal(res$detailed_description, "why")
  }
  res <- groq_engine(chat_content('{"decision_gpt":"1"}'), body = detailed)
  expect_true(is.na(res$detailed_description))
})

test_that(".groq_engine() handles tool calls without detailed descriptions and failures", {
  detailed <- chat_body(tools_detailed_groq, "inclusion_decision")
  res <- groq_engine(chat_msg(tool_call('{"decision_gpt":"1"}')), body = detailed)
  expect_true(is.na(res$detailed_description))

  res <- groq_engine(list(error = list(message = "nope")), body = detailed, status = 500L, token_inf = FALSE, time_inf = FALSE)
  expect_match(res$decision_gpt, "500")
  expect_equal(names(res), c("decision_gpt", "decision_binary", "detailed_description")[c(1, 2, 3)])

  res <- groq_engine(list(error = list(message = "nope")), body = chat_body(tools_simple_groq_conf), status = 500L)
  expect_true("confidence" %in% names(res))

  local_mock_api(\(req) json_resp(list()), internet = FALSE)
  res <- .groq_engine(
    chat_body(tools_detailed_groq_conf, "inclusion_decision"), 1000, TRUE, TRUE, "k", 1, NULL,
    .groq_is_transient, NULL, NULL, url
  )
  expect_match(res$decision_gpt, "internet")
  expect_true(all(c("confidence", "detailed_description") %in% names(res)))
})

test_that(".rep_groq_engine() maps tool choices, extra args and handles request errors", {
  local_mock_api(llm_handler("chat"))
  run <- function(t_choice, tool = tools_simple_groq, ...) {
    .rep_groq_engine(
      question = "q", model_gpt = "m", topp = 1, iterations = 2, req_per_min = 1000,
      role_gpt = "user", tool = tool, t_choice = t_choice, seeds = 1, time_inf = FALSE,
      token_inf = FALSE, api_key = "k", max_t = 1, max_s = NULL, is_trans = .groq_is_transient,
      back = NULL, aft = NULL, endpoint_url = url, ...
    )
  }
  expect_equal(nrow(run("auto")), 2)
  expect_equal(nrow(run(NULL)), 2)
  expect_equal(nrow(run("required")), 2)
  expect_equal(nrow(run("inclusion_decision_simple", temperature = 0)), 2)
  expect_equal(nrow(run(list(type = "function", `function` = list(name = "inclusion_decision_simple")))), 2)
  expect_equal(nrow(run(list(type = "function", `function` = list(name = "inclusion_decision")), tool = tools_detailed_groq)), 2)
  expect_equal(nrow(run(list(name = "inclusion_decision_binary"), tool = tools_detailed_binary_groq)), 2)
})

test_that(".rep_groq_engine() returns error rows when the engine fails", {
  testthat::local_mocked_bindings(.groq_engine = \(...) stop("boom"))
  res <- .rep_groq_engine(
    question = "q", model_gpt = "m", topp = 1, iterations = 1, req_per_min = 1000,
    role_gpt = "user", tool = tools_detailed_groq_conf, t_choice = "inclusion_decision", seeds = NULL,
    time_inf = TRUE, token_inf = TRUE, api_key = "k", max_t = 1, max_s = NULL,
    is_trans = .groq_is_transient, back = NULL, aft = NULL, endpoint_url = url
  )
  expect_true(is.na(res$decision_binary))
  expect_true(all(c("detailed_description", "confidence", "prompt_tokens", "run_time") %in% names(res)))
})

# Ollama ---------------------------------------------------------------------

ollama_engine <- function(message, body = chat_body(tools_simple), status = 200L, ...) {
  local_mock_api(\(req) json_resp(list(message = message), status), .env = parent.frame())
  .ollama_engine(body = body, time_inf = TRUE, max_t = 1, max_s = NULL, back = NULL, aft = NULL, endpoint_url = url, ...)
}

ollama_call <- function(args, name = "inclusion_decision_simple") {
  list(tool_calls = list(list(`function` = list(name = name, arguments = args))))
}

test_that(".ollama_engine() validates tool call decisions", {
  expect_equal(ollama_engine(ollama_call(list(decision_gpt = "1")))$decision_binary, 1)
  expect_equal(ollama_engine(ollama_call(list(decision = "0")))$decision_binary, 0)
  expect_equal(ollama_engine(ollama_call(list(decision_gpt = "1.0")))$decision_gpt, "1")
  expect_equal(ollama_engine(ollama_call('{"decision_gpt":"1"}'))$decision_binary, 1)
  expect_equal(ollama_engine(ollama_call(list(decision_gpt = "1.1")))$decision_binary, 1)

  expect_match(ollama_engine(ollama_call(list(decision_gpt = "")))$decision_gpt, "empty decision")
  expect_match(ollama_engine(ollama_call(list(decision_gpt = "maybe")))$decision_gpt, "invalid decision")
  expect_match(ollama_engine(ollama_call(list(decision_gpt = "7")))$decision_gpt, "invalid decision")
  expect_match(ollama_engine(ollama_call(list(other = "1")))$decision_gpt, "missing decision")
  expect_match(ollama_engine(ollama_call("{bad"))$decision_gpt, "Failed to parse")
  expect_match(ollama_engine(ollama_call(list(decision_gpt = "1"), name = "other"))$decision_gpt, "wrong function call")
  expect_match(ollama_engine(list(tool_calls = list(list(other = 1))))$decision_gpt, "Missing function object")
  expect_match(ollama_engine(list(content = "hi"))$decision_gpt, "did not return a function call")
})

test_that(".ollama_engine() extracts descriptions and confidence", {
  detailed <- chat_body(tools_detailed_conf, "inclusion_decision")
  for (key in c("detailed_description", "description", "reasoning", "explanation")) {
    args <- stats::setNames(list("1", "because", 70), c("decision_gpt", key, "confidence"))
    res <- ollama_engine(ollama_call(args, "inclusion_decision"), body = detailed)
    expect_equal(c(res$detailed_description, res$confidence), c("because", "70"))
  }
  res <- ollama_engine(ollama_call(list(decision_gpt = "1"), "inclusion_decision"), body = detailed)
  expect_true(is.na(res$detailed_description))

  body <- chat_body(tools_simple, "inclusion_decision")
  body$tools <- NULL
  res <- ollama_engine(ollama_call(list(decision_gpt = "1"), "inclusion_decision"), body = body)
  expect_true("detailed_description" %in% names(res))
})

test_that(".ollama_engine() reads allowed decisions and tool names from flat tool definitions", {
  flat <- tools_simple_claude
  flat[[1]]$parameters <- flat[[1]]$input_schema
  body <- list(model = "m", messages = list(), tools = flat, tool_choice = NULL, stream = TRUE)
  res <- ollama_engine(ollama_call(list(decision_gpt = "1")), body = body)
  expect_equal(res$decision_binary, 1)

  decision_enum <- list(list(type = "function", `function` = list(
    name = "f", parameters = list(properties = list(decision = list(enum = list("yes", "no"))))
  )))
  body <- list(model = "m", messages = list(), tools = decision_enum)
  expect_equal(ollama_engine(ollama_call(list(decision = "yes"), "f"), body = body)$decision_gpt, "yes")

  expect_error(ollama_engine(list(), status = 500L), "500")
})

test_that(".rep_ollama_engine() builds requests and falls back to error rows", {
  run <- function(t_choice, tool = tools_simple, time_inf = TRUE, ...) {
    .rep_ollama_engine(
      question = "q", model_gpt = "llama3", topp = 1, iterations = 2, role_gpt = "user",
      tool = tool, t_choice = t_choice, seeds = 1, time_inf = time_inf, max_t = 1, max_s = NULL,
      back = NULL, aft = NULL, endpoint_url = url, ...
    )
  }
  local_mock_api(llm_handler("ollama"))
  expect_equal(nrow(run("inclusion_decision_simple", ctx_length = 2048)), 2)
  expect_equal(nrow(run(NULL)), 2)
  expect_equal(nrow(run("required")), 2)
  expect_equal(nrow(run("auto", system_guard_msg = "guard")), 2)
  expect_equal(nrow(run(list(type = "function", `function` = list(name = "inclusion_decision_simple")))), 2)
  expect_equal(nrow(run(list(name = "inclusion_decision"), tool = tools_detailed)), 2)

  local_mock_api(llm_handler("ollama", status = 500L))
  res <- run(list(type = "function", `function` = list(name = "inclusion_decision")), tool = tools_detailed_conf)
  expect_match(res$decision_gpt, "Request failed")
  expect_true(all(c("detailed_description", "confidence") %in% names(res)))

  res <- run(list(name = "inclusion_decision_binary"), time_inf = FALSE, tool = NULL)
  expect_false("run_time" %in% names(res))
})
