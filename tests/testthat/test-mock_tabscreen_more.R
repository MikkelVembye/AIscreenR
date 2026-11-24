call_fun <- function(p, data = mock_dat(2), ..., cols = c("studyid", "title", "abstract"), env = parent.frame()) {
  local_mock_api(llm_handler(p$api), .env = env)
  args <- utils::modifyList(
    c(
      list(data = data, prompt = "p", api_key = "k", progress = FALSE, max_tries = 1),
      stats::setNames(lapply(cols, as.name), cols),
      p$args
    ),
    list(...)
  )
  suppressMessages(do.call(p$fun, args))
}

for (nm in setdiff(names(providers), "ollama")) {
  p <- providers[[nm]]

  test_that(paste(nm, "covers all tool variants"), {
    for (conf in c(FALSE, TRUE)) {
      for (desc in c(FALSE, TRUE)) {
        for (over in c(FALSE, TRUE)) {
          res <- call_fun(p, confidence = conf, decision_description = desc, overinclusive = over)
          expect_equal(nrow(res$answer_data), 2)
        }
      }
    }
  })

  test_that(paste(nm, "works without a study id"), {
    res <- call_fun(p, cols = c("title", "abstract"))
    expect_equal(nrow(res$answer_data), 2)
  })

  test_that(paste(nm, "accepts answer data from an earlier screening"), {
    first <- call_fun(p)
    res <- call_fun(p, data = first$answer_data)
    expect_s3_class(res, "gpt")
  })

  test_that(paste(nm, "validates cutoffs"), {
    skip_if(nm == "groq", "groq does not validate numeric cutoffs")
    f <- \(...) call_fun(p, reps = 2, ...)
    expect_error(f(incl_cutoff_upper = "a"), "numeric value between 0 and 1")
    expect_error(f(incl_cutoff_upper = 0.5, incl_cutoff_lower = "a"), "numeric value between 0 and 1")
    expect_error(f(incl_cutoff_upper = 0.5, incl_cutoff_lower = -0.5), "only takes values between 0 and 1")
    res <- f(incl_cutoff_upper = 0.7)
    expect_equal(attr(res$answer_data_aggregated, "incl_cutoff_lower"), 0.7)
  })

  test_that(paste(nm, "warns when titles or abstracts are missing"), {
    f <- \() tryCatch(call_fun(p, cols = c("studyid", "title")), error = \(e) NULL)
    skip_if(nm == "groq", "groq does not warn")
    expect_warning(f(), "title")
  })

  test_that(paste(nm, "refuses too small token limits"), {
    skip_if(nm == "claude", "claude uses max_tokens as a default argument")
    f <- \(...) call_fun(p, ...)
    if (nm != "groq") expect_error(f(max_completion_tokens = 2), "tokens below")
    expect_error(f(max_tokens = 2), "tokens below")
    skip_if(nm == "groq", "groq has a different descriptive check")
    expect_error(f(decision_description = TRUE, max_tokens = 20), "descriptive")
  })

  test_that(paste(nm, "defaults tool_choice to auto when only tools are given"), {
    tool <- switch(nm, claude = tools_simple_claude, gemini = tools_simple_gemini, groq = tools_simple_groq, tools_simple)
    res <- call_fun(p, tools = tool)
    expect_equal(nrow(res$answer_data), 2)
  })
}

test_that("tabscreen_claude() validates its remaining arguments", {
  p <- providers$claude
  f <- \(...) call_fun(p, ...)
  expect_error(f(max_completion_tokens = 2), "tokens below 9")
  expect_error(f(reasoning_effort = "xhigh", model = "claude-sonnet-4-6"), "xhigh")
  expect_snapshot(res <- tryCatch(f(reasoning_effort = "bogus"), error = \(e) conditionMessage(e)))
})

test_that("tabscreen_mistral() requires tools for any/required choices", {
  p <- providers$mistral
  f <- \(...) call_fun(p, ...)
  expect_error(f(tool_choice = "required", tools = NULL), "tool")
})
