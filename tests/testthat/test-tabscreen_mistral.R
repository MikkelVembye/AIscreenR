test_that(".gpt_engine return errors correctly", {

  tools_choice_name <- list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple"
    )
  )

  # Specifying unknown model
  body <- list(
    model = "mistral-7",
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .gpt_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key_mistral(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )


  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "400"))

  # Specifying wrong api key
  body <- list(
    model = "open-mistral-nemo",
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .gpt_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = 1234,
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "401"))

  # Specifying ineligible role
  body <- list(
    model = "open-mistral-nemo",
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  res <- .gpt_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key_mistral(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "422"))

  # Specifying ineligible role
  body <- list(
    model = "open-mistral-nemo",
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  # Transient is correctly add to function
  .gpt_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient(),
    back = NULL,
    aft = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  ) |>
    expect_error()

})


test_that(".rep_gpt_engine controls errrors correctly", {

  iterations <- 3

  # Ineligible role
  res <- .rep_gpt_engine(
    question = question,
    model_gpt = "open-mistral-nemo",
    topp = 1,
    role_gpt = "use",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = get_api_key(),
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401")))

  # Ineligible api key
  res <- .rep_gpt_engine(
    question = question,
    model_gpt = "open-mistral-nemo",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = 1,
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401")))

  # Ineligible model
  res <- .rep_gpt_engine(
    question = question,
    model_gpt = "mistral-7",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = get_api_key_mistral(),
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://api.mistral.ai/v1/chat/completions"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400")))

})