# Screening app. Start it with AIscreenR::run_screening_app().
# The logic is in the package: R/misc.func.hidden.screening_app*.R

app <- asNamespace("AIscreenR") # Gives access to the internal functions

# Buttons on the Screen and Reconcile tabs send one message with the record they
# belong to, so a double click cannot answer a record that has not been seen.
script <- shiny::tags$script(shiny::HTML("
  function screeningAction(input, action, key) {
    var note = document.getElementById('screen_note');
    Shiny.setInputValue(input,
      {action: action, key: key, note: note ? note.value : null},
      {priority: 'event'});
  }
  document.addEventListener('keydown', function(e) {
    if (e.repeat || e.ctrlKey || e.metaKey || e.altKey) return;
    if (/^(INPUT|TEXTAREA|SELECT)$/.test(e.target.tagName)) return;
    var id = {i: 'btn_include', e: 'btn_exclude'}[e.key.toLowerCase()];
    var button = id && document.getElementById(id);
    if (button && button.offsetParent !== null) button.click();
  });
"))

action_button <- function(label, input, action, key, class = "btn-default", id = NULL,
                          disabled = FALSE) {
  shiny::tags$button(
    type = "button", id = id, class = paste("btn", class),
    disabled = if (disabled) NA,
    onclick = sprintf("screeningAction('%s', '%s', '%s')", input, action, key),
    label
  )
}

need_project <- function() shiny::p("Upload a RIS file on the Upload tab first.")

need_both <- function() {
  shiny::p("This opens when both reviewers have screened all records and clicked \"I am finished\".")
}

steps <- c("Setup", "Screen", "AI", "Reconcile", "Compare", "Export")

# Button to the next tab, shown when a step is done
next_button <- function(tab) {
  shiny::p(shiny::actionButton(
    paste0("go_", tab), paste("Next:", tab),
    icon = shiny::icon("arrow-right"), class = "btn-primary"
  ))
}

# Download buttons for the report(). `info` is NULL until the report may be shown.
# `tab` keeps the button IDs unique, because the section is on several tabs.
report_tabs <- c("ai", "reconcile", "export")

report_section <- function(info, tab) {
  if (is.null(info)) return(NULL)

  content <- if (info$disagreements == 0) {
    shiny::p("The AI agrees with all the final decisions, so there is nothing to report.")
  } else if (!app$.quarto_available()) {
    shiny::p("Install Quarto and the R package quarto to create the report.")
  } else {
    shiny::tagList(
      shiny::p(paste0(
        "Records where the AI differs from the final decision: ", info$disagreements,
        ". Creating the report can take half a minute."
      )),
      shiny::p(
        shiny::downloadButton(paste0("report_html_", tab), "Report (HTML)"),
        shiny::downloadButton(paste0("report_docx_", tab), "Report (Word)")
      )
    )
  }

  shiny::tagList(
    shiny::h4("Report of AI disagreements"),
    if (info$without_ai > 0) {
      shiny::div(class = "alert alert-warning", paste(
        "Records without an AI answer, which are not in the report:", info$without_ai
      ))
    },
    content
  )
}

record_text <- function(record) {
  shiny::tagList(
    shiny::h4(record$title),
    shiny::p(if (nzchar(record$abstract)) record$abstract else shiny::em("No abstract"))
  )
}

ui <- shiny::navbarPage(
  "AIscreenR screening",
  id = "tabs",
  header = script,
  shiny::tabPanel(
    "Upload",
    shiny::uiOutput("file_picker"),
    shiny::uiOutput("summary"),
    DT::DTOutput("preview")
  ),
  shiny::tabPanel("Setup", shiny::uiOutput("setup_panel")),
  shiny::tabPanel("Screen", shiny::uiOutput("screen_panel")),
  shiny::tabPanel(
    "AI",
    shiny::passwordInput("api_key", "OpenAI API key (leave empty to use the key already set in R)"),
    shiny::uiOutput("ai_panel")
  ),
  shiny::tabPanel("Reconcile", shiny::uiOutput("reconcile_panel")),
  shiny::tabPanel(
    "Compare",
    shiny::uiOutput("compare_panel"),
    DT::DTOutput("ai_disagreements"),
    shiny::uiOutput("analysis_panel")
  ),
  shiny::tabPanel("Export", shiny::uiOutput("export_panel"))
)

server <- function(input, output, session) {

  path <- getOption("AIscreenR.screening_project")
  if (is.null(path)) stop("Start the app with AIscreenR::run_screening_app().", call. = FALSE)

  project <- shiny::reactiveVal(if (file.exists(path)) app$.load_screening_project(path))

  records <- shiny::reactive({
    shiny::req(project())
    app$.screening_records(project()$refs)
  })

  # Change the project file and show the new version. Returns TRUE if it worked.
  change_project <- function(change) {
    res <- tryCatch(app$.update_screening_project(path, change), error = function(e) e)
    if (inherits(res, "error")) {
      shiny::showNotification(conditionMessage(res), type = "error")
      return(FALSE)
    }
    project(res)
    TRUE
  }

  # A value that only changes when its content changes. Panels with text fields
  # use it, so they are not redrawn (and emptied) when the AI saves in the background.
  stable <- function(value) {
    state <- shiny::reactiveVal(NULL)
    shiny::observe(state(value()), priority = 10)
    state
  }

  lapply(steps, function(tab) {
    shiny::observeEvent(input[[paste0("go_", tab)]], {
      shiny::updateNavbarPage(session, "tabs", selected = tab)
    })
  })

  #.........................................
  # Upload ----
  #.........................................

  uploaded <- shiny::reactiveVal(NULL) # A file that is read, but not yet saved
  problem <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$ris_file, {
    uploaded(NULL)
    problem(NULL)

    res <- shiny::withProgress(
      message = "Reading file...",
      tryCatch(app$.read_screening_ris(input$ris_file$datapath), error = function(e) e)
    )

    if (inherits(res, "error")) {
      problem(conditionMessage(res))
    } else {
      uploaded(c(res, list(name = input$ris_file$name)))
    }
  })

  shiny::observeEvent(input$create, {
    shiny::req(uploaded())
    problem(NULL)

    res <- tryCatch(
      app$.create_screening_project(uploaded()$refs, uploaded()$name, path),
      error = function(e) e
    )

    if (inherits(res, "error")) {
      problem(conditionMessage(res))
    } else {
      project(res)
      uploaded(NULL)
    }
  })

  # The references on screen: from the project if there is one, else from the upload
  shown_refs <- shiny::reactiveVal(NULL)
  shiny::observe(
    shown_refs(if (!is.null(project())) project()$refs else uploaded()$refs)
  )

  has_project <- stable(function() !is.null(project()))

  output$file_picker <- shiny::renderUI({
    if (isFALSE(has_project())) shiny::fileInput("ris_file", "Choose a RIS file", accept = ".ris")
  })

  output$summary <- shiny::renderUI({
    refs <- shown_refs()

    counts <- if (!is.null(refs)) {
      shown <- app$.screening_records(refs)
      name <- if (!is.null(project())) project()$ris_name else uploaded()$name

      shiny::tagList(
        shiny::h4(paste0(nrow(shown), " records in ", name)),
        shiny::p(paste("Records without an abstract:", sum(shown$abstract == ""))),
        shiny::p(paste("Records without a title:", sum(shown$title == "")))
      )
    }

    next_step <- if (!is.null(project())) {
      shiny::tagList(
        shiny::p(
          paste0("Saved in ", path, ". To screen another file, restart the app with a new project file name.")
        ),
        next_button("Setup")
      )
    } else if (!is.null(refs)) {
      shiny::tagList(
        shiny::p(paste("The project will be saved in", path)),
        shiny::actionButton("create", "Create project", class = "btn-primary")
      )
    } else {
      shiny::p(paste("No project yet. It will be saved in", path))
    }

    shiny::tagList(
      if (!is.null(problem())) shiny::div(class = "alert alert-danger", problem()),
      lapply(uploaded()$warnings, function(w) shiny::div(class = "alert alert-warning", w)),
      counts,
      next_step
    )
  })

  output$preview <- DT::renderDT({
    shiny::req(shown_refs())
    DT::datatable(
      app$.screening_preview(shown_refs()),
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 10)
    )
  })

  #.........................................
  # Setup ----
  #.........................................

  ai_running <- shiny::reactiveVal(FALSE)

  setup_state <- stable(function() {
    p <- project()
    if (is.null(p)) return(list(exists = FALSE))
    list(
      exists = TRUE, question = p$question, model = p$model, reasons = p$reasons,
      started = !is.null(p$ai), running = ai_running()
    )
  })

  output$setup_panel <- shiny::renderUI({
    s <- setup_state()
    shiny::req(s)
    if (!s$exists) return(need_project())

    if (s$started || s$running) {
      return(shiny::tagList(
        shiny::p("The AI has started, so the setup is locked."),
        shiny::p(shiny::strong("Question: "), s$question),
        shiny::p(shiny::strong("Model: "), s$model),
        shiny::p(shiny::strong("AI reasons: "), if (s$reasons) "Yes" else "No"),
        if (!s$running) shiny::p(shiny::actionButton("delete_ai", "Delete AI answers and start over")),
        next_button("Screen")
      ))
    }

    shiny::tagList(
      shiny::textAreaInput("question", "Screening question", s$question, width = "100%", rows = 5),
      shiny::selectInput("model", "Model", app$.screening_models(), selected = s$model),
      shiny::checkboxInput("reasons", "Ask the AI for a reason (costs more)", s$reasons),
      shiny::p(shiny::actionButton("save_setup", "Save", class = "btn-primary")),
      if (nzchar(s$question)) next_button("Screen")
    )
  })

  shiny::observeEvent(input$save_setup, {
    shiny::req(!ai_running())
    saved <- change_project(function(p) {
      app$.set_screening_setup(p, input$question, input$model, input$reasons)
    })
    if (saved) shiny::showNotification("Setup saved.")
  })

  shiny::observeEvent(input$delete_ai, {
    shiny::showModal(shiny::modalDialog(
      "Delete all AI answers? The AI screening must then be paid for again.",
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })

  shiny::observeEvent(input$confirm_delete, {
    shiny::req(!ai_running())
    change_project(app$.delete_ai_answers)
    ai_message(NULL)
    shiny::removeModal()
  })

  #.........................................
  # Screen ----
  #.........................................

  reviewer <- shiny::reactiveVal(NULL)
  position <- shiny::reactiveVal(1L)

  # Only this reviewer's own answers are collected here
  screen_state <- stable(function() {
    p <- project()
    if (is.null(p)) return(list(view = "none"))
    if (app$.screening_locked(p)) return(list(view = "locked"))

    r <- reviewer()
    n <- nrow(p$refs)
    if (is.null(r)) {
      done <- vapply(
        app$.screening_reviewers,
        function(x) sum(!is.na(app$.screening_decisions(p, x))),
        integer(1)
      )
      return(list(view = "choose", n = n, done = done, finished = p$finished))
    }

    id <- position()
    answers <- app$.screening_decisions(p, r)
    list(
      view = "record", reviewer = r, id = id, n = n, question = p$question,
      record = shiny::isolate(records())[id, ],
      n_done = sum(!is.na(answers)), decision = answers[id],
      note = app$.screening_notes(p, r)[id],
      finished = r %in% p$finished,
      other_finished = any(setdiff(app$.screening_reviewers, r) %in% p$finished)
    )
  })

  output$screen_panel <- shiny::renderUI({
    s <- screen_state()
    shiny::req(s)

    if (s$view == "none") return(need_project())

    if (s$view == "locked") {
      return(shiny::tagList(
        shiny::p("Both reviewers have finished, so answers are locked."),
        next_button("AI")
      ))
    }

    if (s$view == "choose") {
      return(shiny::tagList(
        shiny::h4("Who is screening?"),
        lapply(app$.screening_reviewers, function(r) {
          shiny::p(
            action_button(paste("Screen as", r), "screen_action", "start", r, "btn-primary"),
            paste0(" ", s$done[[r]], " of ", s$n, " screened"),
            if (r %in% s$finished) ", finished"
          )
        })
      ))
    }

    key <- paste0(s$reviewer, "_", s$id)
    chosen <- function(value, class) if (isTRUE(s$decision == value)) class else "btn-default"

    shiny::tagList(
      shiny::p(
        shiny::strong(s$reviewer),
        paste0(" - record ", s$id, " of ", s$n, " (", s$n_done, " screened)")
      ),
      if (nzchar(s$question)) shiny::div(class = "well", s$question),
      record_text(s$record),
      shiny::tags$label("for" = "screen_note", "Note (optional)"),
      shiny::tags$textarea(id = "screen_note", class = "form-control", rows = 3, s$note),
      shiny::p(
        "Your answer: ",
        shiny::strong(app$.decision_label(s$decision, missing = "not screened yet"))
      ),
      shiny::p(
        action_button("Include (I)", "screen_action", "include", key,
                      chosen(1, "btn-success"), id = "btn_include"),
        action_button("Exclude (E)", "screen_action", "exclude", key,
                      chosen(0, "btn-danger"), id = "btn_exclude")
      ),
      shiny::p(
        action_button("Back", "screen_action", "back", key, disabled = s$id == 1),
        action_button("Next", "screen_action", "next", key,
                      disabled = s$id == s$n || is.na(s$decision)),
        action_button("Hand over", "screen_action", "handover", key, "btn-warning")
      ),
      if (s$finished) {
        shiny::p("You are finished. You can still change answers until the other reviewer is finished too.")
      } else if (s$n_done == s$n) {
        shiny::tagList(
          if (s$other_finished) {
            shiny::p("The other reviewer is finished. Clicking below locks all answers.")
          },
          action_button("I am finished", "screen_action", "finish", key, "btn-primary")
        )
      }
    )
  })

  shiny::observeEvent(input$screen_action, {
    p <- project()
    shiny::req(p)
    action <- input$screen_action$action
    key <- input$screen_action$key
    note <- input$screen_action$note

    if (action == "start") {
      shiny::req(key %in% app$.screening_reviewers)
      position(app$.first_unscreened(p, key))
      reviewer(key)
      return()
    }

    r <- reviewer()
    id <- position()
    n <- nrow(p$refs)
    # A message from a record that is no longer on screen is ignored
    shiny::req(r, identical(key, paste0(r, "_", id)))

    if (action %in% c("include", "exclude")) {
      decision <- as.integer(action == "include")
      if (change_project(function(p) app$.record_decision(p, r, id, decision, note))) {
        position(min(id + 1L, n))
      }
      return()
    }

    change_project(function(p) app$.save_note(p, r, id, note))

    if (action == "back") position(max(id - 1L, 1L))
    if (action == "next" && !is.na(app$.screening_decisions(project(), r)[id])) {
      position(min(id + 1L, n))
    }
    if (action == "finish") change_project(function(p) app$.finish_screening(p, r))
    if (action == "handover") reviewer(NULL)
  })

  #.........................................
  # AI ----
  #.........................................

  ai_job <- NULL
  ai_queue <- integer(0)
  ai_stop <- FALSE
  ai_settings <- NULL
  ai_message <- shiny::reactiveVal(NULL)

  ai_state <- stable(function() {
    p <- project()
    if (is.null(p)) return(list(exists = FALSE))
    list(
      exists = TRUE, n = nrow(p$refs), n_missing = length(app$.ai_missing(p)),
      question = p$question, model = p$model, reasons = p$reasons,
      price = sum(p$ai_price), running = ai_running(), message = ai_message(),
      report = if (!ai_running()) app$.report_info(p)
    )
  })

  ai_cost <- shiny::reactive({
    s <- ai_state()
    shiny::req(s$exists, !s$running)
    app$.ai_cost(shiny::isolate(project())) # Recalculate only when ai_state changes
  })

  output$ai_panel <- shiny::renderUI({
    s <- ai_state()
    shiny::req(s)
    if (!s$exists) return(need_project())
    if (!nzchar(s$question)) return(shiny::p("Save a screening question on the Setup tab first."))

    status <- shiny::tagList(
      shiny::p(paste(s$n - s$n_missing, "of", s$n, "records have an AI answer.")),
      if (s$price > 0) {
        shiny::p(shiny::strong(paste("Total price of the AI screening:", app$.format_price(s$price))))
      }
    )
    message <-if (!is.null(s$message)) shiny::div(class = "alert alert-info", s$message)

    if (s$running) {
      return(shiny::tagList(
        status, message,
        shiny::p("Running. Keep this window open."),
        shiny::actionButton("stop_ai", "Stop after the current chunk")
      ))
    }

    if (s$n_missing == 0) {
      return(shiny::tagList(
        status, message, report_section(s$report, "ai"), next_button("Reconcile")
      ))
    }

    cost <- ai_cost()
    shiny::tagList(
      status, message,
      shiny::p(
        paste0("Approximate cost for the remaining records: $", signif(cost, 2), " (", s$model, ")."),
        if (s$reasons) " AI reasons make it more expensive than this."
      ),
      if (cost > 15) shiny::checkboxInput("accept_cost", "I accept this cost", FALSE),
      shiny::actionButton(
        "start_ai",
        if (s$n_missing < s$n) "Continue/Retry" else "Start AI screening",
        class = "btn-primary"
      ),
      report_section(s$report, "ai")
    )
  })

  start_next_chunk <- function() {
    size <- getOption("AIscreenR.screening_chunk_size", 25)
    ids <- utils::head(ai_queue, size)
    ai_queue <<- setdiff(ai_queue, ids)
    ai_job <<- app$.start_ai_chunk(
      shiny::isolate(records())[ids, ],
      ai_settings$question, ai_settings$model, ai_settings$reasons, ai_settings$api_key
    )
  }

  end_ai_run <- function(message) {
    ai_job <<- NULL
    ai_queue <<- integer(0)
    ai_settings <<- NULL # Forget the API key
    ai_message(message)
    ai_running(FALSE)
    # Also print the total price in the R console
    price <- sum(shiny::isolate(project())$ai_price)
    base::message(message, " Total price of the AI screening: ", app$.format_price(price))
  }

  shiny::observeEvent(input$start_ai, {
    p <- project()
    shiny::req(p, !ai_running(), nzchar(p$question), length(app$.ai_missing(p)) > 0)
    ai_message(NULL)

    if (app$.ai_cost(p) > 15 && !isTRUE(input$accept_cost)) {
      ai_message("Tick \"I accept this cost\" to start.")
      return()
    }

    api_key <- if (isTRUE(nzchar(input$api_key))) {
      input$api_key
    } else {
      tryCatch(app$get_api_key(), error = function(e) "")
    }
    if (!nzchar(api_key)) {
      ai_message("No API key found. Write it in the field above.")
      return()
    }

    ai_queue <<- app$.ai_missing(p)
    ai_stop <<- FALSE
    ai_settings <<- list(
      question = p$question, model = p$model, reasons = p$reasons, api_key = api_key
    )
    started <- tryCatch(start_next_chunk(), error = function(e) e)
    if (inherits(started, "error")) {
      end_ai_run(conditionMessage(started))
    } else {
      ai_running(TRUE)
    }
  })

  shiny::observeEvent(input$stop_ai, {
    ai_stop <<- TRUE
    ai_message("Stopping after the current chunk.")
  })

  # Check twice a second whether the current chunk is done
  shiny::observe({
    shiny::req(ai_running())
    shiny::invalidateLater(500)

    shiny::isolate({
      if (is.null(ai_job) || !future::resolved(ai_job)) return()

      result <- app$.ai_chunk_result(ai_job)
      saved <- change_project(function(p) {
        app$.add_ai_price(app$.add_ai_answers(p, result$answers), result$price)
      })
      n_good <- if (is.null(result$answers)) 0 else nrow(result$answers)

      if (!saved) {
        end_ai_run("The AI answers could not be saved.")
      } else if (n_good == 0) {
        end_ai_run(paste("Stopped, because every request in a chunk failed:", result$error))
      } else if (ai_stop) {
        end_ai_run("Stopped.")
      } else if (length(ai_queue) == 0) {
        n_left <- length(app$.ai_missing(project()))
        end_ai_run(if (n_left == 0) {
          "AI screening is done."
        } else {
          paste(n_left, "records got no answer. Click Continue/Retry.")
        })
      } else {
        started <- tryCatch(start_next_chunk(), error = function(e) e)
        if (inherits(started, "error")) end_ai_run(conditionMessage(started))
      }
    })
  })

  #.........................................
  # Reconcile ----
  #.........................................

  dis_position <- shiny::reactiveVal(1L)

  output$reconcile_panel <- shiny::renderUI({
    p <- project()
    if (is.null(p)) return(need_project())
    if (!app$.screening_locked(p)) return(need_both())

    ids <- app$.human_disagreements(p)
    report <- if (!ai_running()) report_section(app$.report_info(p), "reconcile")
    if (length(ids) == 0) {
      return(shiny::tagList(
        shiny::p("human1 and human2 agree on all records."),
        report,
        next_button("Compare")
      ))
    }

    i <- min(dis_position(), length(ids))
    id <- ids[i]
    final <- app$.final_decisions(p)
    chosen <- function(value, class) if (isTRUE(final[id] == value)) class else "btn-default"
    reviewer_line <- function(r) {
      note <- app$.screening_notes(p, r)[id]
      shiny::p(
        shiny::strong(paste0(r, ": ")),
        app$.decision_label(app$.screening_decisions(p, r)[id]),
        if (nzchar(note)) paste0(" - ", note)
      )
    }

    shiny::tagList(
      shiny::p(paste0(
        "Disagreement ", i, " of ", length(ids), " (", sum(!is.na(final[ids])), " settled)"
      )),
      record_text(records()[id, ]),
      lapply(app$.screening_reviewers, reviewer_line),
      shiny::p("Final decision: ", shiny::strong(app$.decision_label(final[id], "not settled"))),
      shiny::p(
        action_button("Include (I)", "reconcile_action", "include", id,
                      chosen(1, "btn-success"), id = "btn_include"),
        action_button("Exclude (E)", "reconcile_action", "exclude", id,
                      chosen(0, "btn-danger"), id = "btn_exclude")
      ),
      shiny::p(
        action_button("Back", "reconcile_action", "back", id, disabled = i == 1),
        action_button("Next", "reconcile_action", "next", id, disabled = i == length(ids))
      ),
      report,
      if (!anyNA(final[ids])) next_button("Compare")
    )
  })

  shiny::observeEvent(input$reconcile_action, {
    p <- project()
    shiny::req(p, app$.screening_locked(p))
    ids <- app$.human_disagreements(p)
    shiny::req(length(ids) > 0)

    i <- min(dis_position(), length(ids))
    id <- ids[i]
    shiny::req(identical(as.character(input$reconcile_action$key), as.character(id)))
    action <- input$reconcile_action$action

    if (action == "back") dis_position(max(i - 1L, 1L))
    if (action == "next") dis_position(min(i + 1L, length(ids)))
    if (action %in% c("include", "exclude")) {
      decision <- as.integer(action == "include")
      if (change_project(function(p) app$.record_final(p, id, decision))) {
        dis_position(min(i + 1L, length(ids)))
      }
    }
  })

  #.........................................
  # Compare ----
  #.........................................

  analysis_job <- NULL
  analysis_input <- NULL
  analysis_running <- shiny::reactiveVal(FALSE)
  analysis_message <- shiny::reactiveVal(NULL)

  compare_open <- shiny::reactive({
    !is.null(project()) && app$.screening_locked(project())
  })

  output$compare_panel <- shiny::renderUI({
    p <- project()
    if (is.null(p)) return(need_project())
    if (!compare_open()) return(need_both())

    n <- nrow(p$refs)
    unsettled <- sum(is.na(app$.final_decisions(p)))
    performance <- tryCatch(app$.ai_performance(p), error = function(e) e)

    shiny::tagList(
      shiny::p(paste("The AI has answered", n - length(app$.ai_missing(p)), "of", n, "records.")),
      shiny::h4("AI against the final human decision"),
      if (unsettled > 0) {
        shiny::p(paste("Settle the remaining", unsettled, "disagreements first."))
      } else if (is.null(performance)) {
        shiny::p("Run the AI screening first.")
      } else if (inherits(performance, "error")) {
        shiny::div(class = "alert alert-warning", conditionMessage(performance))
      } else {
        shiny::tableOutput("performance")
      },
      shiny::h4("Records where the AI disagrees with the final decision")
    )
  })

  output$performance <- shiny::renderTable({
    shiny::req(compare_open())
    app$.ai_performance(project())
  }, digits = 3)

  output$ai_disagreements <- DT::renderDT({
    shiny::req(compare_open())
    shown <- app$.ai_disagreements(project())
    for (column in c("human1", "human2", "ai", "final")) {
      shown[[column]] <- app$.decision_label(shown[[column]])
    }
    DT::datatable(shown, rownames = FALSE, selection = "none")
  })

  output$analysis_panel <- shiny::renderUI({
    shiny::req(compare_open())
    p <- project()

    shiny::tagList(
      shiny::h4("Solve-or-guess analysis of the original answers"),
      if (!is.null(analysis_message())) {
        shiny::div(class = "alert alert-info", analysis_message())
      },
      if (analysis_running()) {
        shiny::p("Running. This can take several minutes (about 9 for 1,000 records).")
      } else if (is.null(p$ai)) {
        shiny::p("Run the AI screening first.")
      } else {
        shiny::actionButton("run_analysis", "Run analysis")
      },
      if (!is.null(p$analysis)) {
        shiny::tagList(
          shiny::p(paste("Bootstrap repeats: B =", p$analysis$B)),
          shiny::h5("Solving probabilities"), shiny::tableOutput("solving"),
          shiny::h5("AI compared with each reviewer (kappa ratios)"), shiny::tableOutput("ratios"),
          shiny::h5("Agreement between pairs (kappa)"), shiny::tableOutput("kappa")
        )
      },
      next_button("Export")
    )
  })

  analysis_table <- function(name) {
    shiny::renderTable({
      shiny::req(compare_open(), project()$analysis)
      project()$analysis[[name]]
    }, digits = 3)
  }
  output$solving <- analysis_table("solving_probabilities")
  output$ratios <- analysis_table("kappa_ratios")
  output$kappa <- analysis_table("pairwise_kappa")

  shiny::observeEvent(input$run_analysis, {
    shiny::req(compare_open(), !analysis_running(), project()$ai)
    analysis_message(NULL)
    analysis_input <<- app$.analysis_data(project())

    job <- tryCatch(
      app$.start_analysis(analysis_input, getOption("AIscreenR.screening_B", 200)),
      error = function(e) e
    )
    if (inherits(job, "error")) {
      analysis_message(conditionMessage(job))
    } else {
      analysis_job <<- job
      analysis_running(TRUE)
    }
  })

  shiny::observe({
    shiny::req(analysis_running())
    shiny::invalidateLater(500)

    shiny::isolate({
      if (!future::resolved(analysis_job)) return()
      fit <- tryCatch(suppressWarnings(future::value(analysis_job)), error = function(e) e)

      if (inherits(fit, "error")) {
        analysis_message(paste("The analysis failed:", conditionMessage(fit)))
      } else {
        started_with <- analysis_input
        change_project(function(p) {
          if (!identical(app$.analysis_data(p), started_with)) {
            stop("The answers changed during the analysis. Run it again.", call. = FALSE)
          }
          p$analysis <- app$.analysis_summary(fit)
          p
        })
      }
      analysis_job <<- NULL
      analysis_running(FALSE)
    })
  })

  #.........................................
  # Export ----
  #.........................................

  output$export_panel <- shiny::renderUI({
    p <- project()
    if (is.null(p)) return(need_project())
    if (!app$.screening_locked(p)) return(need_both())

    unsettled <- sum(is.na(app$.final_decisions(p)))
    without_ai <- length(app$.ai_missing(p))

    shiny::tagList(
      if (unsettled > 0) {
        shiny::div(class = "alert alert-warning",
                   paste(unsettled, "disagreements are not settled yet."))
      },
      if (without_ai > 0) {
        shiny::div(class = "alert alert-warning", paste(without_ai, "records have no AI answer."))
      },
      shiny::p(
        shiny::downloadButton("download_xlsx", "Download Excel"),
        shiny::downloadButton("download_ris", "Download RIS")
      ),
      if (!ai_running()) report_section(app$.report_info(p), "export")
    )
  })

  export <- function(write, extension) {
    shiny::downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(basename(path)), "_screening.", extension)
      },
      content = function(file) {
        p <- app$.load_screening_project(path)
        shiny::req(app$.screening_locked(p))
        write(p, file)
      }
    )
  }
  output$download_xlsx <- export(app$.write_screening_xlsx, "xlsx")
  output$download_ris <- export(app$.write_screening_ris, "ris")

  report_download <- function(format) {
    shiny::downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(basename(path)), "_AI_disagreements.", format)
      },
      content = function(file) {
        p <- app$.load_screening_project(path)
        info <- app$.report_info(p)
        shiny::req(!ai_running(), info, info$disagreements > 0)
        app$.write_screening_report(p, file, format)
      }
    )
  }
  for (tab in report_tabs) {
    output[[paste0("report_html_", tab)]] <- report_download("html")
    output[[paste0("report_docx_", tab)]] <- report_download("docx")
  }
}

shiny::shinyApp(ui, server)
