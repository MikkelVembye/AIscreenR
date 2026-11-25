# Internal functions for the screening app: comparing with the AI and exporting

# One row per record with every decision
.screening_results <- function(project) {
  records <- .screening_records(project$refs)

  reason <- rep("", nrow(records))
  if ("detailed_description" %in% names(project$ai)) {
    reason[project$ai$studyid] <- as.character(project$ai$detailed_description)
    reason[is.na(reason)] <- ""
  }

  data.frame(
    studyid = records$studyid,
    title = records$title,
    human1 = .screening_decisions(project, "human1"),
    human1_note = .screening_notes(project, "human1"),
    human2 = .screening_decisions(project, "human2"),
    human2_note = .screening_notes(project, "human2"),
    ai = .ai_decisions(project),
    ai_reason = reason,
    final = .final_decisions(project)
  )
}

.decision_label <- function(x, missing = "No answer") {
  out <- ifelse(x == 1, "Include", "Exclude")
  out[is.na(x)] <- missing
  out
}

# AI against the final human decision. NULL until there is something to compare.
.ai_performance <- function(project) {
  final <- .final_decisions(project)
  if (anyNA(final) || is.null(project$ai)) return(NULL)

  answers <- project$ai
  answers$human_code <- final[answers$studyid]
  screen_analyzer(answers)
}

.ai_disagreements <- function(project) {
  res <- .screening_results(project)
  res[which(res$ai != res$final), ]
}

#.........................................
# Disagreement report with report() ----
#.........................................

# NULL until both reviewers have finished, all disagreements are settled and the
# AI has answered. The report shows AI answers, so it must not open earlier.
.report_info <- function(project) {
  if (!.screening_locked(project) || is.null(project$ai) || anyNA(.final_decisions(project))) {
    return(NULL)
  }
  list(
    disagreements = nrow(.ai_disagreements(project)),
    without_ai = length(.ai_missing(project))
  )
}

# The records where the AI differs from the final decision, in the form report() uses
.report_data <- function(project) {
  rows <- .ai_disagreements(project)
  data <- data.frame(
    studyid = rows$studyid,
    title = rows$title,
    abstract = .screening_records(project$refs)$abstract[rows$studyid],
    human_code = rows$final,
    final_decision_gpt_num = rows$ai,
    # The AI's reason if it was asked for, otherwise its decision in words
    decision_gpt = ifelse(nzchar(rows$ai_reason), rows$ai_reason, .decision_label(rows$ai))
  )

  # Extra information that report() shows when it is there
  for (column in intersect(c("author", "year", "journal"), names(project$refs))) {
    data[[column]] <- project$refs[[column]][rows$studyid]
  }
  answers <- project$ai[match(rows$studyid, project$ai$studyid), ]
  for (column in intersect(c("model", "submodel", "run_date"), names(answers))) {
    data[[column]] <- answers[[column]]
  }
  data
}

.quarto_available <- function() {
  requireNamespace("quarto", quietly = TRUE) && !is.null(quarto::quarto_path())
}

# Write the report as one file, "html" or "docx"
.write_screening_report <- function(project, file, format) {
  folder <- tempfile("screening_report")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)

  out <- suppressMessages(report(
    data = .report_data(project),
    studyid = studyid,
    title = title,
    abstract = abstract,
    gpt_answer = decision_gpt,
    human_code = human_code,
    final_decision_gpt_num = final_decision_gpt_num,
    file = "AI_disagreements",
    format = format,
    document_title = "AI screening disagreements",
    directory = folder,
    open = FALSE
  ))

  # report() puts the styles of an HTML file in a separate folder. Render again
  # with everything inside the file, so one file can be downloaded.
  if (format == "html") {
    quarto::quarto_render(
      file.path(folder, "AI_disagreements.qmd"), output_format = "html",
      metadata = list("embed-resources" = TRUE), quiet = TRUE
    )
  }

  if (!file.copy(out, file, overwrite = TRUE)) stop("Could not create the report.", call. = FALSE)
}

# The original answers of all three screeners, before disagreements were settled
.analysis_data <- function(project) {
  .screening_results(project)[, c("studyid", "human1", "human2", "ai")]
}

# solve_or_guess() is slow, so it runs in the background
.start_analysis <- function(data, B) {
  future::future(
    AIscreenR::solve_or_guess(
      data, system_rater_id = "ai", reference_raters = c("human1", "human2"),
      item_id = "studyid", B = B, progress = FALSE
    ),
    globals = list(data = data, B = B),
    packages = "AIscreenR",
    seed = TRUE
  )
}

# The parts of the result that the app shows and saves
.analysis_summary <- function(fit) {
  fit[c("solving_probabilities", "kappa_ratios", "pairwise_kappa", "B")]
}

# Excel cannot hold more than 32,767 characters in a cell
.fit_excel_cells <- function(data) {
  data <- as.data.frame(data)
  text <- vapply(data, is.character, logical(1))
  data[text] <- lapply(data[text], substr, 1, 32767)
  data
}

# Excel file with the sheets
#   answer_data: the AI answers as returned by tabscreen_gpt() (if the AI has run)
#   decisions:   one row per record with the human, AI and final decisions
.write_screening_xlsx <- function(project, file) {
  sheets <- list(decisions = .fit_excel_cells(.screening_results(project)))
  if (!is.null(project$ai)) {
    answers <- project$ai[order(project$ai$studyid), ]
    sheets <- c(list(answer_data = .fit_excel_cells(answers)), sheets)
  }
  writexl::write_xlsx(sheets, file)
}

# The original RIS records with one note line (N1) about the screening added to
# each. save_dataframe_to_ris() leaves out new columns, so the lines are added here.
.write_screening_ris <- function(project, file) {
  res <- .screening_results(project)

  notes <- paste0(
    "N1  - AIscreenR screening: human1=", .decision_label(res$human1),
    "; human2=", .decision_label(res$human2),
    "; ai=", .decision_label(res$ai),
    "; final=", .decision_label(res$final, missing = "Not settled")
  )
  for (reviewer in .screening_reviewers) {
    text <- .norm_space(res[[paste0(reviewer, "_note")]]) # A note must stay on one line
    has_text <- nzchar(text)
    notes[has_text] <- paste0(notes[has_text], "; ", reviewer, " note: ", text[has_text])
  }

  utils::capture.output(save_dataframe_to_ris(project$refs, file))
  lines <- readLines(file, encoding = "UTF-8")
  ends <- which(lines == "ER  - ")
  if (length(ends) != nrow(res)) stop("Could not add the decisions to the RIS file.", call. = FALSE)

  with_notes <- as.list(lines)
  with_notes[ends] <- Map(c, notes, lines[ends])
  writeLines(enc2utf8(unlist(with_notes, use.names = FALSE)), file, useBytes = TRUE)
}
