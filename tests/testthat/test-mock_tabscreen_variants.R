two_models <- function(nm) {
  switch(
    nm,
    groq = list(model = groq_model_prizes$model[1:2], custom_model = NULL),
    ollama = list(model = c("llama3", "mistral"), custom_model = NULL),
    list(model = c("model-a", "model-b"), custom_model = TRUE)
  )
}

for (nm in names(providers)) {
  p <- providers[[nm]]

  test_that(paste(nm, "screens with several models and repetitions"), {
    m <- two_models(nm)
    args <- list(model = m$model, reps = c(1, 2), rpm = c(100, 200), custom_model = m$custom_model)
    if (nm == "ollama") args$rpm <- NULL
    if (nm == "groq") args$reps <- 2
    res <- do.call(screen, c(list(p), args))
    expect_equal(sort(unique(res$answer_data$model)), sort(m$model))
    expect_equal(nrow(res$answer_data), if (nm == "groq") 8 else 6)
    expect_gt(nrow(res$answer_data_aggregated), 0)
  })

  test_that(paste(nm, "validates model, rpm and reps lengths"), {
    skip_if(nm == "ollama", "ollama has no rpm argument")
    m <- two_models(nm)
    expect_error(screen(p, model = m$model, custom_model = m$custom_model, rpm = 1:3), "rpm must be of the same length")
    expect_error(screen(p, model = m$model, custom_model = m$custom_model, reps = 1:3), "reps must be of the same length")
  })

  test_that(paste(nm, "refuses duplicated prompts"), {
    expect_error(screen(p, prompt = c("a", "a")), "same prompt twice")
  })

  test_that(paste(nm, "refuses unknown models"), {
    skip_if(nm == "ollama", "ollama models are checked against the server")
    expect_error(screen(p, model = "does-not-exist"), "Unknown")
  })

  test_that(paste(nm, "passes a seed on to future"), {
    res <- screen(p, seed_par = 1, reps = 2)
    expect_equal(nrow(res$answer_data), 4)
  })

  test_that(paste(nm, "tolerates missing abstracts and informs about failures"), {
    local_mock_api(llm_handler(p$api))
    dat <- mock_dat(2)
    dat$abstract[1] <- ""
    scr <- function(...) {
      do.call(
        p$fun,
        c(
          list(
            data = dat, prompt = "p", studyid = quote(studyid), title = quote(title),
            abstract = quote(abstract), api_key = "k", progress = FALSE, max_tries = 1
          ),
          utils::modifyList(p$args, list(...))
        )
      )
    }
    expect_message(scr(), "abstract")

    calls <- 0
    ok <- llm_handler(p$api)
    local_mock_api(function(req) {
      calls <<- calls + 1
      if (calls == 1 || (nm == "ollama" && calls <= 3 && grepl("chat", req$url))) llm_handler(p$api, status = 500L)(req) else ok(req)
    })
    expect_message(scr(), "Requests failed for 1")
  })

  test_that(paste(nm, "accepts previous screening results as data"), {
    skip_if(nm == "ollama", "covered below")
    res <- screen(p)
    again <- screen(p)
    expect_s3_class(again, "gpt")
  })

  test_that(paste(nm, "accepts custom tools"), {
    tool <- switch(
      nm,
      claude = tools_simple_claude,
      gemini = tools_simple_gemini,
      groq = tools_simple_groq,
      tools_simple
    )
    res <- screen(p, tools = tool, tool_choice = "auto", max_completion_tokens = NULL)
    expect_equal(res$answer_data$decision_binary, c(1, 1))
  }) |>
    suppressWarnings()
}

test_that("providers accept earlier results as data", {
  for (nm in setdiff(names(providers), "ollama")) {
    p <- providers[[nm]]
    first <- screen(p, reps = 2)
    d1 <- first$answer_data_aggregated
    local_mock_api(llm_handler(p$api))
    again <- suppressMessages(do.call(
      p$fun,
      c(
        list(data = d1, prompt = "p", studyid = quote(studyid), title = quote(title), abstract = quote(abstract),
             api_key = "k", progress = FALSE, max_tries = 1),
        p$args
      )
    ))
    expect_s3_class(again, "gpt")
  }
})

test_that("providers stop when the approximate price is too high", {
  local_mocked_bindings(
    .price_gpt = \(x) tibble::tibble(total_price_dollar = 100)
  )
  for (nm in c("gpt", "claude", "gemini", "mistral", "groq")) {
    expect_error(screen(providers[[nm]]), "set 'force = TRUE'|force = TRUE")
  }
})

test_that("providers describe cost when asked for descriptive answers", {
  for (nm in c("gpt", "mistral", "ollama")) {
    p <- providers[[nm]]
    local_mock_api(llm_handler(p$api))
    expect_message(
      do.call(p$fun, c(
        list(
          data = mock_dat(1), prompt = "p", studyid = quote(studyid), title = quote(title),
          abstract = quote(abstract), api_key = "k", progress = FALSE, max_tries = 1,
          decision_description = TRUE
        ),
        p$args
      )),
      "detail|descriptive|increase"
    )
  }
})

test_that("tabscreen_gemini() and tabscreen_claude() validate reasoning settings", {
  expect_error(screen(providers$gemini, reasoning_effort = "extreme"), "reasoning_effort")
  expect_error(screen(providers$claude, reasoning_effort = "xhigh"), "xhigh")
  expect_error(screen(providers$claude, model = "claude-haiku-4-5", reasoning_effort = "max"), "only available")
  expect_message(screen(providers$claude, model = "claude-haiku-4-5", reasoning_effort = "low", quiet = FALSE), "Be aware")

  for (effort in c("minimal", "low", "medium", "high")) {
    res <- screen(providers$gemini, reasoning_effort = effort)
    expect_equal(nrow(res$answer_data), 2)
  }
  res <- screen(providers$gemini, model = "gemini-2.5-pro", reasoning_effort = "minimal")
  expect_equal(nrow(res$answer_data), 2)
  res <- screen(providers$gemini, model = "gemini-2.5-flash", reasoning_effort = "high")
  expect_equal(nrow(res$answer_data), 2)
})

test_that("tabscreen_claude() sends adaptive thinking only for supported models", {
  res <- screen(providers$claude, model = "claude-opus-4-6", reasoning_effort = "high")
  body <- attr(res, "mock_state")$reqs[[1]]$body$data
  expect_equal(body$thinking$type, "adaptive")
  expect_equal(body$output_config$effort, "high")

  res <- screen(providers$claude, model = "claude-haiku-4-5", reasoning_effort = "high")
  expect_null(attr(res, "mock_state")$reqs[[1]]$body$data$thinking)

  expect_equal(nrow(screen(providers$claude, max_tokens = 2048)$answer_data), 2)
})

test_that("tabscreen_gpt() sends reasoning settings in the request body", {
  res <- screen(providers$gpt, reasoning_effort = "high", verbosity = "medium")
  body <- attr(res, "mock_state")$reqs[[1]]$body$data
  expect_equal(body$reasoning$effort, "high")
  expect_equal(body$text$verbosity, "medium")
  expect_equal(body$tool_choice$name, "inclusion_decision_simple")
})

test_that("tabscreen_mistral() validates tool choices and reasoning effort", {
  p <- providers$mistral
  expect_error(screen(p, reasoning_effort = "medium"), "'none' or 'high'")
  expect_equal(nrow(screen(p, reasoning_effort = "high")$answer_data), 2)
  expect_error(screen(p, tool_choice = list(a = 1), tools = tools_simple), "tool_choice list format is invalid")
  expect_error(screen(p, tool_choice = 1, tools = tools_simple), "tool_choice must be NULL")

  for (choice in list("auto", "none", "any", "required", "inclusion_decision_simple")) {
    expect_equal(nrow(screen(p, tools = tools_simple, tool_choice = choice)$answer_data), 2)
  }
  res <- screen(p, tools = tools_simple, tool_choice = list(type = "function", `function` = list(name = "inclusion_decision_simple")))
  expect_equal(nrow(res$answer_data), 2)
  expect_equal(nrow(screen(p, tools = tools_simple)$answer_data), 2)
  expect_equal(nrow(screen(p, overinclusive = FALSE, decision_description = TRUE, confidence = TRUE)$answer_data), 2)
})

test_that("tabscreen_groq() works without a study id and with binary tools", {
  local_mock_api(llm_handler("chat"))
  res <- suppressMessages(tabscreen_groq(
    mock_dat(2), prompt = "p", title = title, abstract = abstract,
    api_key = "k", progress = FALSE, max_tries = 1
  ))
  expect_equal(nrow(res$answer_data), 2)

  for (conf in c(FALSE, TRUE)) {
    for (desc in c(FALSE, TRUE)) {
      res <- screen(providers$groq, overinclusive = FALSE, confidence = conf, decision_description = desc)
      expect_equal(nrow(res$answer_data), 2)
    }
  }
  expect_error(screen(providers$groq, max_tokens = 5), "tokens below 11")
  expect_error(screen(providers$groq, reps = 2, incl_cutoff_upper = 0.2, incl_cutoff_lower = 0.5), "must not exceed")
  res <- screen(providers$groq, reps = 2, incl_cutoff_upper = 0.8)
  expect_gt(nrow(res$answer_data_aggregated), 0)
})

test_that("tabscreen_ollama() normalizes and validates its endpoint", {
  p <- providers$ollama
  expect_error(screen(p, api_url = c("a", "b")), "single non-empty")
  expect_error(screen(p, api_url = ""), "single non-empty")
  expect_error(screen(p, api_url = "ftp://x"), "must start with")

  expect_message(screen(p, api_url = "http://localhost:11434/api", messages = TRUE, quiet = FALSE), "normalized")
  expect_message(screen(p, api_url = "http://localhost:11434", messages = TRUE, quiet = FALSE), "normalized")
  expect_message(screen(p, api_url = "http://localhost:11434/other", messages = TRUE, quiet = FALSE), "does not end with")
})

test_that("tabscreen_ollama() validates custom tools", {
  p <- providers$ollama
  tool <- tools_simple[[1]]
  expect_error(screen(p, tools = "x"), "must be of a list")
  expect_error(screen(p, tool_choice = "x"), "must provide a tool")
  expect_error(screen(p, tools = list(list(description = "no name"))), "must include a function name")
  expect_error(screen(p, tools = list(list(type = "function", `function` = list(name = ""))), tool_choice = "x"), "non-empty function name")
  expect_error(
    screen(p, tools = list(list(name = "f", parameters = list(properties = list(x = list(type = "string")))))),
    "decision_gpt"
  )

  flat <- list(name = "f", parameters = list(properties = list(decision = list(type = "string"))))
  res <- screen(p, tools = flat)
  expect_equal(nrow(res$answer_data), 2)
  res <- screen(p, tools = list(flat), tool_choice = list(type = "function", `function` = list(name = "f")))
  expect_equal(nrow(res$answer_data), 2)
  expect_equal(nrow(screen(p, tools = tools_simple, tool_choice = "required")$answer_data), 2)
})

test_that("tabscreen_ollama() checks models against the server", {
  p <- providers$ollama
  expect_error(screen(p, model = "unknown"), "available models")
  expect_error(screen(p, reps = 1:2), "same length")
  expect_error(screen(p, reps = 11), "force")
  expect_error(screen(p, reps = 2, incl_cutoff_upper = 0.1, incl_cutoff_lower = 0.5), "must not exceed")
  expect_error(screen(p, model = NULL), "provide a model")
  expect_error(screen(p, model = ""), "provide a model")

  local_mock_api(llm_handler("ollama", ollama_caps = "completion"))
  expect_error(
    suppressMessages(tabscreen_ollama(
      mock_dat(1), prompt = "p", studyid = studyid, title = title, abstract = abstract, model = "llama3"
    )),
    "do not support tools"
  )

  local_mock_api(llm_handler("ollama", ollama_models = character()))
  expect_message(
    tabscreen_ollama(
      mock_dat(1), prompt = "p", studyid = studyid, title = title, abstract = abstract,
      model = "llama3", progress = FALSE, max_tries = 1, messages = TRUE
    ),
    "Could not retrieve models"
  )
})

test_that("tabscreen_ollama() reports context length and descriptions, and records failures", {
  p <- providers$ollama
  expect_message(screen(p, context_length = 4096, decision_description = TRUE, messages = TRUE, quiet = FALSE), "context_length")

  res <- screen(p, status = 500L, reps = 2)
  expect_match(res$answer_data$decision_gpt, "Request failed", all = TRUE)
  expect_equal(nrow(res$error_data), 4)
  expect_gt(nrow(res$answer_data_aggregated), 0)

  res <- screen(p, overinclusive = FALSE, confidence = TRUE, decision_description = TRUE)
  expect_equal(nrow(res$answer_data), 2)
  res <- screen(p, overinclusive = FALSE)
  expect_equal(nrow(res$answer_data), 2)

  prev <- screen(p, reps = 2)
  local_mock_api(llm_handler("ollama"))
  scr <- \(d) suppressMessages(tabscreen_ollama(
    d, prompt = "p", studyid = studyid, title = title, abstract = abstract, model = "llama3",
    progress = FALSE, max_tries = 1
  ))
  expect_s3_class(scr(prev$answer_data_aggregated), "gpt")
})
