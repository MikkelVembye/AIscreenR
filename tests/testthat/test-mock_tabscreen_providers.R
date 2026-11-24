for (nm in names(providers)) {
  p <- providers[[nm]]

  test_that(paste(nm, "single repetition returns decisions"), {
    res <- screen(p)
    expect_s3_class(res, "gpt")
    expect_equal(nrow(res$answer_data), 2)
    expect_equal(res$answer_data$decision_binary, c(1, 1))
    expect_null(res$error_data)
    expect_null(res$answer_data_aggregated)
    expect_output(print(res))
  })

  test_that(paste(nm, "excludes when the model answers 0"), {
    res <- screen(p, decision = "0")
    expect_equal(res$answer_data$decision_binary, c(0, 0))
  })

  test_that(paste(nm, "aggregates repeated answers"), {
    res <- screen(p, reps = 3, incl_cutoff_upper = 0.8, incl_cutoff_lower = 0.2)
    expect_equal(nrow(res$answer_data), 6)
    expect_equal(res$answer_data_aggregated$incl_p, c(1, 1))
    expect_equal(res$answer_data_aggregated$final_decision_gpt, c("Include", "Include"))
    expect_output(print(res))
  })

  test_that(paste(nm, "aggregation uses default cutoffs"), {
    res <- screen(p, reps = 2, decision = "0")
    expect_equal(res$answer_data_aggregated$final_decision_gpt, c("Exclude", "Exclude"))
  })

  test_that(paste(nm, "returns detailed descriptions"), {
    res <- screen(p, decision_description = TRUE)
    expect_true("detailed_description" %in% names(res$answer_data))
    expect_equal(res$answer_data$detailed_description, rep("Mock reasoning.", 2))

    res <- screen(p, decision_description = TRUE, reps = 2)
    expect_true("longest_answer" %in% names(res$answer_data_aggregated))
  })

  test_that(paste(nm, "returns confidence scores"), {
    res <- screen(p, confidence = TRUE, reps = 2)
    expect_equal(res$answer_data$confidence, rep(0.9, 4))
    expect_true("mean_confidence" %in% names(res$answer_data_aggregated))

    res <- screen(p, confidence = TRUE, decision_description = TRUE)
    expect_true(all(c("confidence", "detailed_description") %in% names(res$answer_data)))
  })

  test_that(paste(nm, "supports non-overinclusive tools"), {
    res <- screen(p, overinclusive = FALSE)
    expect_equal(res$answer_data$decision_binary, c(1, 1))
    res <- screen(p, overinclusive = FALSE, decision_description = TRUE, confidence = TRUE)
    expect_equal(nrow(res$answer_data), 2)
  })

  test_that(paste(nm, "can drop time and token information"), {
    res <- screen(p, time_info = FALSE, token_info = FALSE)
    expect_false("run_time" %in% names(res$answer_data))
    expect_false("prompt_tokens" %in% names(res$answer_data))
  })

  test_that(paste(nm, "records failed requests"), {
    res <- screen(p, status = 401L)
    expect_equal(res$answer_data$decision_binary, c(NA_real_, NA_real_))
    expect_match(res$answer_data$decision_gpt, "401|Error", all = TRUE)
    expect_equal(nrow(res$error_data), 2)
  })

  test_that(paste(nm, "reports a missing internet connection"), {
    skip_if(nm == "ollama", "ollama engine does not check connectivity")
    res <- screen(p, internet = FALSE)
    expect_match(res$answer_data$decision_gpt, "Error", all = TRUE)
  })

  test_that(paste(nm, "handles multiple prompts"), {
    local_mock_api(llm_handler(p$api))
    res <- suppressMessages(do.call(
      p$fun,
      c(
        list(
          data = mock_dat(2), prompt = c("Prompt A", "Prompt B"), studyid = quote(studyid),
          title = quote(title), abstract = quote(abstract), api_key = "k", progress = FALSE, max_tries = 1
        ),
        p$args
      )
    ))
    expect_equal(nrow(res$answer_data), 4)
    expect_equal(sort(unique(res$answer_data$promptid)), 1:2)
  })

  test_that(paste(nm, "validates arguments"), {
    skip_if(nm == "ollama")
    expect_error(screen(p, reps = 11), "force")
    expect_error(screen(p, reps = 2, incl_cutoff_upper = 0.2, incl_cutoff_lower = 0.5), "must not exceed")
    expect_error(screen(p, tools = "x"), "list")
    if (nm != "groq") {
      expect_error(screen(p, reps = 2, incl_cutoff_lower = 0.5), "incl_cutoff_upper")
      expect_error(screen(p, reps = 2, incl_cutoff_upper = 2), "between 0 and 1")
    }
    if (nm != "claude") expect_error(screen(p, tool_choice = "auto"), "tool")
  })
}

test_that("tabscreen_gpt validates models and reasoning arguments", {
  p <- providers$gpt
  expect_error(screen(p, model = "gpt-4o"), "deprecated")
  expect_error(screen(p, model = "not-a-model"), "Unknown gpt model")
  expect_error(screen(p, model = "gpt-5-nano", reasoning_effort = "extreme"), "reasoning_effort")
  expect_error(screen(p, model = "gpt-5-nano", verbosity = "loud"), "verbosity")
  expect_error(screen(p, model = c("gpt-5-nano", "gpt-5-mini"), rpm = c(1, 2, 3)), "rpm")
  expect_error(screen(p, model = c("gpt-5-nano", "gpt-5-mini"), reps = c(1, 2, 3)), "reps")
  expect_error(screen(p, max_completion_tokens = 2), "tokens below 9")
  expect_error(screen(p, max_tokens = 2), "tokens below 9")
  expect_error(screen(p, decision_description = TRUE, max_tokens = 20), "descriptive")
})

test_that("tabscreen_gpt supports custom models, multiple models and deprecated args", {
  p <- providers$gpt
  res <- screen(p, model = "my-ft-model", custom_model = TRUE)
  expect_null(res$price_data)

  res <- screen(p, model = c("gpt-5-nano", "gpt-5-mini"), reps = c(1, 2), rpm = c(100, 200))
  expect_equal(length(unique(res$answer_data$model)), 2)
  expect_s3_class(res$price_data, "data.frame")

  expect_snapshot(res <- screen(p, model = "my-ft-model", fine_tuned = TRUE))
})

test_that("tabscreen_gpt accepts previous gpt results as data", {
  p <- providers$gpt
  res <- screen(p)
  res$answer_data_aggregated <- screen(p, reps = 2)$answer_data_aggregated
  local_mock_api(llm_handler("gpt"))
  scr <- \(d) suppressMessages(tabscreen_gpt(
    d, prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1
  ))
  expect_s3_class(scr(res$answer_data), "gpt")
  expect_s3_class(scr(res$answer_data_aggregated), "gpt")
})

test_that("tabscreen_gpt returns errors for flawed JSON", {
  local_mock_api(llm_handler("gpt", raw = list(
    model = "m", output = list(list(type = "function_call", arguments = "{not json")),
    usage = list(input_tokens = 1, output_tokens = 1)
  )))
  res <- suppressMessages(tabscreen_gpt(
    mock_dat(1), prompt = "p", studyid = studyid, title = title, abstract = abstract,
    api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1
  ))
  expect_true(is.na(res$answer_data$decision_binary))
})

test_that("tabscreen_gpt warns when expensive and handles missing abstracts", {
  dat <- mock_dat(2)
  dat$abstract[1] <- NA
  local_mock_api(llm_handler("gpt"))
  expect_message(
    tabscreen_gpt(
      dat, prompt = "p", studyid = studyid, title = title, abstract = abstract,
      api_key = "k", progress = FALSE, model = "gpt-5-nano", max_tries = 1
    ),
    "abstracts"
  )
})
