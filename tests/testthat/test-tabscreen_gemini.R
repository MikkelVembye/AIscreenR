test_that(".gemini_engine return errors correctly", {

  function_declarations <- list(
  list(
    name = "inclusion_decision_simple")
)

  # Specifying unknown model
  model <- "gemini-1"
  body <- list(
      contents = list(
        list(
          role = "user",
          parts = list(
            list(text = question)
          )
        )
      ),
      tools = list(
        list(functionDeclarations = function_declarations)
      )
    )

  res <- .gemini_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key_gemini(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent")
  )


  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "404|Error"))

  # Specifying wrong api key
  model <- "gemini-3.1-flash-lite"
  body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = question)
        )
      )
    ),
    tools = list(
      list(functionDeclarations = function_declarations)
    )
  )

  res <- .gemini_engine(
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
    endpoint_url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent")
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "400"))

  # Specifying ineligible role
  body <- list(
    contents = list(
      list(
        role = "use",
        parts = list(
          list(text = question)
        )
      )
    ),
    tools = list(
      list(functionDeclarations = function_declarations)
        )
    )

  res <- .gemini_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key_gemini(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient,
    back = NULL,
    aft = NULL,
    endpoint_url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent")
  )

  expect_equal(ncol(res), 7)
  expect_true(stringr::str_detect(res$decision_gpt, "400"))

  # Transient is correctly add to function
  model <- "gemini-3.1-flash-lite"
  body <- list(
    contents = list(
      list(
        role = "user",
        parts = list(
          list(text = question)
        )
      )
    ),
    tools = list(
      list(functionDeclarations = function_declarations)
    )
   )
  .gemini_engine(
    body = body,
    RPM = 10000,
    timeinf = T,
    tokeninf = T,
    key = get_api_key_gemini(),
    max_t = 4,
    max_s = 10,
    is_trans = gpt_is_transient(),
    back = NULL,
    aft = NULL,
    endpoint_url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent")
  ) |>
    expect_error()

})


test_that(".rep_gpt_engine controls errrors correctly", {

  iterations <- 3

  # Ineligible role
  res <- .rep_gemini_engine(
    question = question,
    model_gpt = "gemini-3.1-flash-lite",
    topp = 1,
    role_gpt = "use",
    tool = tools_simple_gemini,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = get_api_key_gemini(),
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://generativelanguage.googleapis.com",
    reasoning_effort = "medium"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400")))

  # Ineligible api key
  res <- .rep_gemini_engine(
    question = question,
    model_gpt = "gemini-3.1-flash-lite",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple_gemini,
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
    endpoint_url = "https://generativelanguage.googleapis.com",
    reasoning_effort = "medium"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400")))

  # Ineligible model
  res <- .rep_gemini_engine(
    question = question,
    model_gpt = "gemini-1",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple_gemini,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    token_inf = T,
    apikey = get_api_key_gemini(),
    maxt = 4,
    maxs = 10,
    istrans = gpt_is_transient,
    ba = NULL,
    af = NULL,
    endpoint_url = "https://generativelanguage.googleapis.com",
    reasoning_effort = "medium"
  )

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "404")))

})