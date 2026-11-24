url <- "https://mock.test/v1"

mock_engine <- function(engine, raw, body, status = 200L, ..., env = parent.frame()) {
  local_mock_api(\(req) json_resp(raw, status), .env = env)
  args <- list(...)
  suppressMessages(do.call(engine, c(list(body = body), args)))
}

responses_body <- function(tools = tools_simple, name = "inclusion_decision_simple") {
  list(model = "m", input = "q", tools = tools, tool_choice = list(type = "function", name = name))
}

gpt_args <- list(RPM = 1000, timeinf = TRUE, tokeninf = TRUE, key = "k", max_t = 1, max_s = NULL,
                 is_trans = gpt_is_transient, back = NULL, aft = NULL, endpoint_url = url)

fn_call <- function(args) list(output = list(list(type = "function_call", arguments = args)),
                                usage = list(prompt_tokens = 4, completion_tokens = 3), model = "m")

test_that(".gpt_engine_responses() handles malformed and missing tool calls", {
  run <- function(raw, ..., body = responses_body()) {
    do.call(mock_engine, c(list(.gpt_engine_responses, raw, body, env = parent.frame()), utils::modifyList(gpt_args, list(...))))
  }
  res <- run(fn_call("{broken"))
  expect_true(is.na(res$decision_binary))
  expect_match(res$decision_gpt, "Error")

  res <- run(fn_call("{broken"), tokeninf = FALSE)
  expect_true(is.na(res$decision_binary))
  expect_false(any(c("prompt_tokens", "completion_tokens") %in% names(res)))

  res <- run(fn_call('{"decision_gpt":"1"}'), tokeninf = FALSE, timeinf = FALSE)
  expect_false(any(c("prompt_tokens", "run_time") %in% names(res)))

  res <- run(fn_call('{"decision_gpt":"1"}'), max_t = 0)
  expect_equal(res$decision_binary, 1)
  expect_equal(res$prompt_tokens, 4)

  res <- run(fn_call('{"decision_gpt":"maybe"}'))
  expect_true(is.na(res$decision_binary))

  conf_body <- responses_body(tools_simple_conf)
  res <- run(fn_call('{"decision_gpt":"1"}'), body = conf_body)
  expect_true(is.na(res$confidence))

  res <- run(fn_call('{"decision_gpt":"1","detailed_description":"d"}'), body = responses_body(tools_detailed, "inclusion_decision"))
  expect_equal(res$detailed_description, "d")
  res <- run(fn_call('{"decision_gpt":"maybe","detailed_description":"d"}'), body = responses_body(tools_detailed, "inclusion_decision"))
  expect_match(res$detailed_description, "Something went wrong")

})

test_that(".gpt_engine() handles malformed responses on chat completions", {
  chat <- function(args, usage = TRUE) {
    out <- list(model = "m", choices = list(list(message = list(
      tool_calls = list(list(`function` = list(arguments = args)))
    ))))
    if (usage) out$usage <- list(prompt_tokens = 1, completion_tokens = 1)
    out
  }
  run <- function(raw, body = list(model = "m", tools = tools_simple, tool_choice = "auto"), ...) {
    do.call(mock_engine, c(list(.gpt_engine, raw, body, env = parent.frame()), utils::modifyList(gpt_args, list(...))))
  }
  res <- run(chat("{broken"))
  expect_true(is.na(res$decision_binary))

  res <- run(chat('{"decision_gpt":"1"}'), body = list(model = "m", tools = tools_simple_conf), max_t = 0)
  expect_true(is.na(res$confidence))

  res <- run(chat('{"decision_gpt":"1","detailed_description":"d"}'), body = list(model = "m", tools = tools_detailed), timeinf = FALSE)
  expect_equal(res$decision_binary, 1)

  local_mock_api(llm_handler("chat", status = 500L))
  res <- .gpt_engine(list(model = "m", tools = tools_simple_conf), 1000, TRUE, TRUE, "k", 1, NULL, gpt_is_transient, NULL, NULL, url)
  expect_match(res$decision_gpt, "500")
})

test_that(".rep_gpt_engine() builds tool choices and optional body fields", {
  state <- local_mock_api(llm_handler("chat"))
  run <- function(t_choice, ..., model = "gpt-5-nano") {
    .rep_gpt_engine(
      question = "q", model_gpt = model, iterations = 2, req_per_min = 1000, role_gpt = "user",
      tool = tools_simple, t_choice = t_choice, seeds = 1, time_inf = TRUE, token_inf = TRUE, apikey = "k",
      maxt = 1, maxs = NULL, istrans = gpt_is_transient, ba = NULL, af = NULL, endpoint_url = url,
      reasoning_effort = "low", verbosity = "low", ...
    )
  }
  expect_equal(nrow(run("auto")), 2)
  expect_equal(nrow(run("required")), 2)
  expect_equal(nrow(run("inclusion_decision_simple")), 2)
  expect_equal(nrow(run(list(type = "function", `function` = list(name = "inclusion_decision_simple")))), 2)
  expect_equal(nrow(run("auto", topp = 0.5, model = "other-model")), 2)
  expect_error(run(1), "tool_choice must be")
  body <- state$reqs[[state$n]]$body$data
  expect_equal(body$top_p, 0.5)
  expect_null(body$reasoning_effort)
})

test_that(".rep_gpt_engine_responses() builds tool choices", {
  local_mock_api(llm_handler("gpt"))
  run <- function(t_choice, tool = tools_simple, ...) {
    .rep_gpt_engine_responses(
      question = "q", model_gpt = "gpt-5-nano", iterations = 1, req_per_min = 1000, tool = tool,
      t_choice = t_choice, seeds = NULL, time_inf = TRUE, token_inf = TRUE, apikey = "k", maxt = 1, maxs = NULL,
      istrans = gpt_is_transient, ba = NULL, af = NULL, endpoint_url = url, reasoning_effort = NULL,
      verbosity = NULL, ...
    )
  }
  expect_equal(nrow(run("auto")), 1)
  expect_equal(nrow(run("inclusion_decision_simple")), 1)
  expect_equal(nrow(run(list(type = "function", name = "inclusion_decision_simple"))), 1)
  flat <- list(list(type = "function", name = "inclusion_decision_simple", parameters = tools_simple[[1]]$`function`$parameters))
  expect_equal(nrow(run("auto", tool = flat)), 1)
})

# Claude ---------------------------------------------------------------------

claude_args <- list(RPM = 1000, time_inf = TRUE, token_inf = TRUE, api_key = "k", max_t = 1, max_s = NULL,
                    is_trans = gpt_is_transient, back = NULL, aft = NULL, endpoint_url = "https://mock.test")

claude_body <- function(tools = tools_simple_claude) list(model = "m", messages = list(), max_tokens = 10, tools = tools)

test_that(".claude_engine() copes with unusual responses", {
  run <- function(raw, body = claude_body(), ...) {
    do.call(mock_engine, c(list(.claude_engine, raw, body, env = parent.frame()), utils::modifyList(claude_args, list(...))))
  }
  tool_use <- function(input, usage = list(input_tokens = 2, output_tokens = 1)) {
    list(model = "m", content = list(list(type = "tool_use", input = input)), usage = usage)
  }

  res <- run(tool_use(list(decision_gpt = "1"), usage = list(prompt_tokens = 5, completion_tokens = 6)))
  expect_equal(c(res$prompt_tokens, res$completion_tokens), c(5, 6))

  res <- run(tool_use(list(decision_gpt = "1"), usage = list()))
  expect_true(is.na(res$prompt_tokens))


  res <- run(list(content = list()), token_inf = FALSE)
  expect_true(is.na(res$decision_binary))
  expect_false("prompt_tokens" %in% names(res))
  res <- run(list(content = list()), body = claude_body(tools_detailed_claude_conf))
  expect_true(all(c("confidence", "detailed_description") %in% names(res)))

  res <- run(tool_use(list(decision_gpt = "1")), body = claude_body(tools_simple_claude_conf), max_t = 0)
  expect_true(is.na(res$confidence))
  res <- run(tool_use(list(decision_gpt = "1", detailed_description = "x")), body = claude_body(tools_detailed_claude), time_inf = FALSE)
  expect_equal(res$detailed_description, "x")

  local_mock_api(llm_handler("claude", status = 500L))
  res <- .claude_engine(claude_body(tools_detailed_claude_conf), 1000, TRUE, TRUE, "k", 1, NULL, gpt_is_transient, NULL, NULL, "https://mock.test")
  expect_match(res$decision_gpt, "500")
  local_mock_api(llm_handler("claude"), internet = FALSE)
  res <- .claude_engine(claude_body(), 1000, TRUE, TRUE, "k", 1, NULL, gpt_is_transient, NULL, NULL, "https://mock.test")
  expect_match(res$decision_gpt, "internet")
})

test_that(".rep_claude_engine() adds thinking configuration", {
  state <- local_mock_api(llm_handler("claude"))
  run <- function(model, effort, ...) {
    .rep_claude_engine(
      question = "q", model_gpt = model, iterations = 1, req_per_min = 1000, reasoning_effort = effort,
      role_gpt = "user", tool = tools_simple_claude, seeds = NULL, time_inf = TRUE, token_inf = TRUE, api_key = "k",
      max_t = 1, max_s = NULL, is_trans = gpt_is_transient, back = NULL, aft = NULL,
      endpoint_url = "https://mock.test", ...
    )
  }
  run("claude-opus-4-7", "adaptive")
  body <- state$reqs[[state$n]]$body$data
  expect_equal(body$thinking$type, "adaptive")
  expect_null(body$output_config)

  run("claude-opus-4-7", "high", temperature = 0)
  body <- state$reqs[[state$n]]$body$data
  expect_equal(body$output_config$effort, "high")
  expect_equal(body$temperature, 0)

  run("claude-haiku-4-5", "high")
  expect_null(state$reqs[[state$n]]$body$data$thinking)
  run("claude-opus-4-7", "none")
  expect_null(state$reqs[[state$n]]$body$data$thinking)
})

# Gemini ---------------------------------------------------------------------

gemini_args <- list(RPM = 1000, timeinf = TRUE, tokeninf = TRUE, key = "k", max_t = 1, max_s = NULL,
                    is_trans = gpt_is_transient, back = NULL, aft = NULL, endpoint_url = url)

gemini_body <- function(tools = tools_simple_gemini) list(contents = list(), tools = list(list(functionDeclarations = tools)))

test_that(".gemini_engine() copes with unusual responses", {
  run <- function(raw, body = gemini_body(), ...) {
    do.call(mock_engine, c(list(.gemini_engine, raw, body, env = parent.frame()), utils::modifyList(gemini_args, list(...))))
  }
  call <- function(args, name = "inclusion_decision_simple") {
    list(candidates = list(list(content = list(parts = list(list(functionCall = list(name = name, args = args)))))),
         usageMetadata = list(promptTokenCount = 1, candidatesTokenCount = 1), modelVersion = "v")
  }

  res <- run(call(list(decision_gpt = "1")), tokeninf = FALSE, timeinf = FALSE)
  expect_false("prompt_tokens" %in% names(res))
  res <- run(call(list(decision_gpt = "1")), max_t = 0, body = gemini_body(tools_simple_gemini_conf))
  expect_true(is.na(res$confidence))
  res <- run(call(list(decision_gpt = "1", detailed_description = "z"), "inclusion_decision"), body = gemini_body(tools_detailed_gemini))
  expect_equal(res$detailed_description, "z")

  # Function call arguments that cannot become a tibble (a JSON array gives an unnamed list)
  res <- run(call(list("a", "b")))
  expect_true(is.na(res$decision_binary))
  expect_match(res$decision_gpt, "Error")
  expect_true(is.na(res$prompt_tokens))

  # The same without token columns (they do not exist when the response cannot be parsed)
  res <- run(call(list("a", "b")), tokeninf = FALSE)
  expect_true(is.na(res$decision_binary))
  expect_false(any(c("prompt_tokens", "completion_tokens") %in% names(res)))

  # A request without tools is not a detailed request
  res <- run(call(list(decision_gpt = "1")), body = list(contents = list()))
  expect_equal(res$decision_binary, 1)
  expect_false("detailed_description" %in% names(res))
})

test_that(".rep_gemini_engine() maps tool choices and thinking settings", {
  state <- local_mock_api(llm_handler("gemini"))
  run <- function(t_choice, model = "gemini-3.1-flash-lite", effort = NULL, topp = NULL, tool = tools_simple_gemini, ...) {
    .rep_gemini_engine(
      question = "q", model_gpt = model, topp = topp, iterations = 1, req_per_min = 1000, role_gpt = "user",
      tool = tool, t_choice = t_choice, seeds = NULL, time_inf = TRUE, token_inf = TRUE, apikey = "k",
      maxt = 1, maxs = NULL, istrans = gpt_is_transient, ba = NULL, af = NULL, endpoint_url = "https://mock.test",
      reasoning_effort = effort, ...
    )
  }
  mode_of <- \() state$reqs[[state$n]]$body$data$toolConfig$functionCallingConfig$mode

  run("none"); expect_equal(mode_of(), "NONE")
  run("auto"); expect_equal(mode_of(), "AUTO")
  run("any"); expect_equal(mode_of(), "ANY")
  run("required"); expect_equal(mode_of(), "ANY")
  run("inclusion_decision_simple"); expect_equal(mode_of(), "ANY")
  run(list(type = "function", `function` = list(name = "inclusion_decision_simple"))); expect_equal(mode_of(), "ANY")
  run(NULL); expect_null(state$reqs[[state$n]]$body$data$toolConfig)
  run(list(foo = 1)); expect_null(state$reqs[[state$n]]$body$data$toolConfig)

  run("auto", effort = "high", topp = 0.9)
  expect_equal(state$reqs[[state$n]]$body$data$generationConfig$thinkingConfig$thinkingLevel, "high")
  run("auto", model = "gemini-2.5-flash", effort = "medium")
  expect_equal(state$reqs[[state$n]]$body$data$generationConfig$thinkingConfig$thinkingBudget, 8192)
  run("auto", model = "gemini-2.5-flash", effort = "weird")
  expect_equal(state$reqs[[state$n]]$body$data$generationConfig$thinkingConfig$thinkingBudget, -1)
  run("auto", model = "gemini-2.5-pro", effort = "minimal")
  expect_equal(state$reqs[[state$n]]$body$data$generationConfig$thinkingConfig$thinkingBudget, 128)
  run("auto", model = "gemini-2.5-flash", effort = 2048)
  expect_equal(state$reqs[[state$n]]$body$data$generationConfig$thinkingConfig$thinkingBudget, 2048)
})

test_that(".rep_gemini_engine() sends no tools when tool is NULL", {
  # A fixed reply with a function call, whatever the request contains
  reply <- list(
    candidates = list(list(content = list(parts = list(
      list(functionCall = list(name = "inclusion_decision_simple", args = list(decision_gpt = "1")))
    )))),
    usageMetadata = list(promptTokenCount = 1, candidatesTokenCount = 1), modelVersion = "v"
  )
  state <- local_mock_api(\(req) json_resp(reply))

  res <- .rep_gemini_engine(
    question = "q", model_gpt = "gemini-3.1-flash-lite", topp = NULL, iterations = 1, req_per_min = 1000,
    role_gpt = "user", tool = NULL, t_choice = "auto", seeds = NULL, time_inf = TRUE, token_inf = TRUE,
    apikey = "k", maxt = 1, maxs = NULL, istrans = gpt_is_transient, ba = NULL, af = NULL,
    endpoint_url = "https://mock.test", reasoning_effort = NULL
  )

  expect_null(state$reqs[[state$n]]$body$data$tools)
  expect_equal(res$decision_binary, 1)
})
