# Provider-aware fake LLM server.
# Builds a reply that matches the tool schema found in the request body, so a
# single handler works for the simple/detailed/confidence variants.

tool_info <- function(body, provider) {
  tool <- body$tools[[1]]
  if (provider == "gemini") tool <- tool$functionDeclarations[[1]]
  if (!is.null(tool$`function`)) tool <- tool$`function`
  props <- if (provider == "claude") tool$input_schema$properties else tool$parameters$properties
  list(name = tool$name, props = names(props))
}

llm_args <- function(info, decision, description = "Mock reasoning.", confidence = 0.9) {
  args <- list(decision_gpt = decision)
  if ("detailed_description" %in% info$props) args$detailed_description <- description
  if ("confidence" %in% info$props) args$confidence <- confidence
  args
}

llm_handler <- function(
  provider = c("gpt", "chat", "claude", "gemini", "ollama", "gpt_chat", "legacy"),
  decision = "1",
  status = 200L,
  error_body = list(error = list(message = "mock failure")),
  raw = NULL,
  ollama_models = c("llama3", "mistral"),
  ollama_caps = c("completion", "tools")
) {
  provider <- match.arg(provider)
  if (provider == "gpt_chat") {
    # Superseded screen_errors.gpt() posts chat-completions style requests
    gpt <- llm_handler("gpt", decision, status, error_body, raw)
    chat <- llm_handler("chat", decision, status, error_body, raw)
    return(function(req) {
      resp <- gpt(req)
      if (status != 200L) return(resp)
      json_resp(utils::modifyList(httr2::resp_body_json(resp), httr2::resp_body_json(chat(req))))
    })
  }
  function(req) {
    if (provider == "ollama" && grepl("/api/tags$", req$url)) {
      return(json_resp(list(models = lapply(ollama_models, \(m) list(name = m)))))
    }
    if (provider == "ollama" && grepl("/api/show$", req$url)) {
      return(json_resp(list(capabilities = as.list(ollama_caps))))
    }
    if (status != 200L) return(json_resp(error_body, status))
    if (!is.null(raw)) return(json_resp(raw))
    body <- req$body$data
    info <- if (provider == "legacy") {
      list(name = body$function_call$name, props = names(body$functions[[1]]$parameters$properties))
    } else {
      tool_info(body, provider)
    }
    args <- llm_args(info, decision)
    args_json <- as.character(jsonlite::toJSON(args, auto_unbox = TRUE, pretty = provider == "legacy"))

    out <- switch(
      provider,
      gpt = list(
        model = "gpt-mock",
        output = list(list(type = "function_call", arguments = args_json)),
        usage = list(input_tokens = 11, output_tokens = 7)
      ),
      legacy = list(
        model = "legacy-mock",
        choices = list(list(message = list(function_call = list(arguments = args_json)))),
        usage = list(prompt_tokens = 11, completion_tokens = 7)
      ),
      chat = list(
        model = "chat-mock",
        choices = list(list(message = list(
          tool_calls = list(list(type = "function", `function` = list(name = info$name, arguments = args_json)))
        ))),
        usage = list(prompt_tokens = 11, completion_tokens = 7)
      ),
      claude = list(
        model = "claude-mock",
        content = list(list(type = "tool_use", input = args)),
        usage = list(input_tokens = 11, output_tokens = 7)
      ),
      gemini = list(
        modelVersion = "gemini-mock",
        candidates = list(list(content = list(parts = list(
          list(functionCall = list(name = info$name, args = args))
        )))),
        usageMetadata = list(promptTokenCount = 11, candidatesTokenCount = 7)
      ),
      ollama = list(
        message = list(tool_calls = list(list(`function` = list(name = info$name, arguments = args))))
      )
    )
    json_resp(out)
  }
}

# Calls a tabscreen_* function against the fake server with sane test defaults.
# The mock state (recorded requests) is attached as attribute "mock_state".
run_screen <- function(fun, provider, ..., prompt = "Is this relevant?", decision = "1", status = 200L,
                       internet = TRUE, quiet = TRUE, env = parent.frame()) {
  state <- local_mock_api(llm_handler(provider, decision = decision, status = status), internet = internet, .env = env)
  call <- function() {
    fun(
      data = mock_dat(2), prompt = prompt, studyid = studyid, title = title,
      abstract = abstract, api_key = "test-key", progress = FALSE, max_tries = 1,
      ...
    )
  }
  res <- if (quiet) suppressMessages(call()) else call()
  if (is.list(res)) attr(res, "mock_state") <- state
  res
}

providers <- list(
  gpt = list(fun = tabscreen_gpt, api = "gpt", args = list(model = "gpt-5-nano")),
  claude = list(fun = tabscreen_claude, api = "claude", args = list(model = "claude-sonnet-4-6")),
  gemini = list(fun = tabscreen_gemini, api = "gemini", args = list()),
  groq = list(fun = tabscreen_groq, api = "chat", args = list()),
  mistral = list(fun = tabscreen_mistral, api = "chat", args = list()),
  ollama = list(fun = tabscreen_ollama, api = "ollama", args = list(model = "llama3"))
)

screen <- function(p, ..., decision = "1", status = 200L, internet = TRUE, env = parent.frame()) {
  args <- utils::modifyList(p$args, list(...))
  do.call(
    run_screen,
    c(list(fun = p$fun, provider = p$api, decision = decision, status = status, internet = internet, env = env), args)
  )
}

