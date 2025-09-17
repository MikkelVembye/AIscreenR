test_that(".ollama_engine return errors correctly", {

  tools_choice_name <- list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple"
    )
  )

  # Specifying unknown model
  body <- list(
    model = "llama-7",
    messages = list(list(
      role = "user",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  expect_error(
    .ollama_engine(
      body = body,
      RPM = 10000,
      time_inf = TRUE,
      max_t = 4,
      max_s = 10,
      back = NULL,
      aft = NULL
    ),
    regexp = "404|Not Found|Error"
  )

  # Specifying ineligible role
  body <- list(
    model = "llama3-8b-8192",
    messages = list(list(
      role = "use",
      content = question
    )),
    tools = tools_simple,
    tool_choice = tools_choice_name,
    top_p = 1
  )

  expect_error(
    .ollama_engine(
      body = body,
      RPM = 10000,
      time_inf = TRUE,
      max_t = 4,
      max_s = 10,
      back = NULL,
      aft = NULL
    ),
    regexp = "HTTP 404 Not Found"
  )

})


test_that(".rep_ollama_engine controls errrors correctly", {

  iterations <- 3

  # Ineligible role
  res <- .rep_ollama_engine(
    question = question,
    model_gpt = "llama3-8b-8192",
    topp = 1,
    role_gpt = "use",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    max_t = 4,
    max_s = 10,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "400|Error")))

  # Ineligible api key
  res <- .rep_ollama_engine(
    question = question,
    model_gpt = "llama3-8b-8192",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    max_t = 4,
    max_s = 10,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(all(stringr::str_detect(res$decision_gpt, "401|Error")))

  # Ineligible model
  res <- .rep_ollama_engine(
    question = question,
    model_gpt = "llama-min",
    topp = 1,
    role_gpt = "user",
    tool = tools_simple,
    t_choice = "inclusion_decision_simple",
    iterations = iterations,
    req_per_min = 10000,
    seeds = NULL,
    time_inf = T,
    max_t = 4,
    max_s = 10,
    ba = NULL,
    af = NULL
  )

  expect_equal(ncol(res), 4)
  expect_equal(nrow(res), iterations)
  expect_equal(max(res$n), iterations)
  expect_true(any(stringr::str_detect(res$decision_gpt, "404|400|Error")))

})


paths <- system.file("extdata", "test_prompts.rds", package = "AIscreenR")
prompts <- readRDS(file = paths)


prompt <- prompts[1]
prompt2 <- prompts[2]


models <- c("llama-3.1-8b-instant", "openai/gpt-oss-20b")
reps <- c(2, 1)
rpm <- c(30, 30)

skip <- TRUE
skip_github_action <- FALSE

future::plan(future::multisession)

test_that("tabscreen_ollama() works with single parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      max_tries = 2
    )

  )

  expect_true(is.numeric(test_obj$answer_data$decision_binary))

  expect_equal(nrow(test_obj$answer_data), 1L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)


  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = c(prompt, prompt2),
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192"),
      reps = 1
    )

  )

  expect_equal(nrow(test_obj$answer_data), 2L)
  expect_null(test_obj$answer_data_aggregated)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)



})

test_that("tabscreen_ollama() works with multiple parameter values.",{

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 3L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2L)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

  expect_gt(nrow(test_obj$answer_data), nrow(test_obj$answer_data_aggregated))

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 6L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 4L)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)


  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192", "gemma-7b-it"),
      reps = c(2, 1),
      top_p = c(0.2, 1)
    )

  )

  expect_equal(nrow(test_obj$answer_data), 12L)
  expect_equal(nrow(test_obj$answer_data_aggregated), 2*2*2)
  expect_null(test_obj$price_data)
  expect_null(test_obj$price_dollar)

})

test_that("tabscreen_ollama() works with detailed functions and ... .", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1:2,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      decision_description = TRUE
    )

  ) |>
    suppressMessages()


  expect_true(is.character(test_obj$answer_data$detailed_description))
  expect_null(test_obj$answer_data_aggregated)

  # Working with temperature, i.e. ... calls

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 3,
      decision_description = TRUE,
      progress = FALSE,
      temperature = 0.001
    )

  ) |>
    suppressMessages()

  n_distinct_detailed_answers <- test_obj$answer_data$detailed_description |> dplyr::n_distinct()

  expect_lt(n_distinct_detailed_answers, 3)

})

test_that("Message behavior.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_no_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )

  expect_no_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      messages = FALSE,
      progress = FALSE
    )

  )
  expect_no_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gemma-7b-it",
      reps = 2
    )
  )
  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      decision_description = TRUE
    )
  ) |>
    suppressMessages()
  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "gemma-7b-it",
      reps = 1,
      decision_description = TRUE
    ),

    paste0(
      "* Be aware that getting descriptive, detailed responses will substantially increase",
      " the prize of the screening relative to the noted approximate prize."
    )

  ) |>
    suppressMessages()

  expect_message(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[148,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract
    ),

    paste0(
      "* Consider removing references without abstracts ",
      "since these can distort the accuracy of the screening."
    )

  ) |>
    suppressMessages()

})

test_that("tabscreen_ollama() expected errors.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1, 1)
    )

  )

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(30, 30, 30)
    )

  )

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192-x", "gemma-7b-it"),
      reps = reps,
      rpm = rpm
    )

  )

  # With detailed description functions

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1, 1),
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = models,
      reps = c(10, 1),
      rpm = c(30, 30, 30),
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = c("llama3-8b-8192-x", "gemma-7b-it"),
      reps = reps,
      rpm = rpm,
      decision_description = TRUE
    )

  )

  expect_error(

    test_obj <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      incl_cutoff_upper = 0.4,
      incl_cutoff_lower = 0.5
    )

  )

})

future::plan(future::sequential)

# Test parallel

test_that("That parallel processing works.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  future::plan(future::multisession, workers = future::availableCores())

  expect_message(

    tm_par <- system.time(
      test_obj <- tabscreen_ollama(
        data = filges2015_dat[1:2,],
        prompt = prompt,
        title = title,
        abstract = abstract,
        model = "llama3-8b-8192",
        reps = 10
      )
    )
  )

  future::plan(future::sequential)

  expect_lt(tm_par[["elapsed"]], sum(test_obj$answer_data$run_time))

  ollama_answers <-
    test_obj$answer_data |>
    filter(!is.na(decision_gpt)) |>
    pull(decision_gpt)

  # Testing function call work
  ollama_answers |>
    stringr::str_detect("1|0|rror") |>
    unique() |>
    expect_true()



})

test_that("Print and class expectation for ollama object.", {

  if(skip) skip()
  if(skip_github_action) skip_on_ci()
  skip_on_cran()

  expect_message(

    random_name <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
      max_tries = 2
    )

  )

  # class check
  expect_s3_class(random_name, "ollama")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")

  expect_output(print.ollama(random_name), "random_name\\$answer_data")
  expect_false("answer_data_aggregated" %in% names(random_name))
  expect_false("price_dollar" %in% names(random_name))

  print_out1 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data"
  )
  expect_output(print(random_name), print_out1)

  expect_message(

    random_name <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract,
    )
  ) |>
    suppressMessages()

  expect_identical(names(random_name$answer_data), names(random_name$error_data))

  expect_output(print(random_name), "random_name\\$answer_data")
  expect_output(print(random_name), "random_name\\$error_data")

  print_out2 <- paste0(
    "Find data with all answers by executing\\n random_name\\$answer_data"
  )

  expect_output(print(random_name), print_out2)

  #############
  expect_message(

    random_name <- tabscreen_ollama(
      data = filges2015_dat[1,],
      prompt = prompt,
      studyid = studyid,
      title = title,
      abstract = abstract,
      model = "llama3-8b-8192",
      reps = 1,
    )

  )

  # class check
  expect_s3_class(random_name, "ollama")
  expect_s3_class(random_name, "list")

  expect_output(print(random_name), "random_name")

  expect_output(print.ollama(random_name), "random_name\\$answer_data")

  expect_false("price_dollar" %in% names(random_name))

  expect_message(

    random_name <- tabscreen_ollama(
      data = filges2015_dat[c(1:2),],
      prompt = c(prompt),
      studyid = studyid, # indicate the variable with the studyid in the data
      title = title, # indicate the variable with the titles in the data
      abstract = abstract
    )
  ) |>
    suppressMessages()


  expect_output(print(random_name), "random_name\\$answer_data")
  expect_output(print(random_name), "random_name\\$error_data")

  expect_false("price_dollar" %in% names(random_name))


})

