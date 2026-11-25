# Internal functions for the screening app (inst/app/app.R): reading the RIS
# file, the project file, human screening and settling disagreements.
#
# A project is a list saved as an .rds file. The RIS references in `refs` are
# never edited, so the row number in `refs` is a fixed study ID.

.screening_reviewers <- c("human1", "human2")

# Read a RIS file. Returns the references and any warnings from reading them.
.read_screening_ris <- function(file) {
  warns <- character(0)
  refs <- withCallingHandlers(
    read_ris_to_dataframe(file),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (nrow(refs) == 0) stop("No records found. Is this a RIS file?", call. = FALSE)
  if (all(.screening_records(refs)$title == "")) stop("No titles found in the file.", call. = FALSE)

  list(refs = refs, warnings = warns)
}

# The columns the app works with
.screening_records <- function(refs) {
  text_column <- function(name) {
    if (!name %in% names(refs)) return(rep("", nrow(refs)))
    x <- trimws(as.character(refs[[name]]))
    x[is.na(x)] <- ""
    x
  }

  data.frame(
    studyid = seq_len(nrow(refs)),
    title = text_column("title"),
    abstract = text_column("abstract")
  )
}

# Records with shortened abstracts for the preview table
.screening_preview <- function(refs, max_chars = 150) {
  records <- .screening_records(refs)
  long <- nchar(records$abstract) > max_chars
  records$abstract[long] <- paste0(substr(records$abstract[long], 1, max_chars), "...")
  records
}

#.........................................
# Project file ----
#.........................................

.create_screening_project <- function(refs, ris_name, path) {
  if (file.exists(path)) stop("A project already exists in \"", path, "\".", call. = FALSE)

  project <- structure(
    list(
      refs = refs,
      ris_name = ris_name,
      created = Sys.time(),
      question = "",
      model = "gpt-6-luna",
      reasons = FALSE,
      decisions = data.frame(
        studyid = integer(0), reviewer = character(0), decision = integer(0), note = character(0)
      ),
      finished = character(0), # Reviewers who have marked themselves as finished
      ai = NULL,               # AI answers: rows of the 'answer_data' from tabscreen_gpt()
      ai_price = 0,            # Total price (USD) of the AI screening
      final = data.frame(studyid = integer(0), decision = integer(0)), # Settled disagreements
      analysis = NULL          # Saved solve_or_guess() results
    ),
    class = "screening_project"
  )
  .save_screening_project(project, path)
  project
}

# Write to a temporary file first, so a crash cannot leave a half-written project.
# No compression, because the file is saved after every click.
.save_screening_project <- function(project, path) {
  tmp <- tempfile("screening_", tmpdir = dirname(path), fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(project, tmp, compress = FALSE)
  if (!file.rename(tmp, path)) stop("Could not save the project to \"", path, "\".", call. = FALSE)
  invisible(path)
}

.load_screening_project <- function(path) {
  project <- tryCatch(suppressWarnings(readRDS(path)), error = function(e) NULL)
  if (!inherits(project, "screening_project")) {
    stop("\"", path, "\" is not a screening project file.", call. = FALSE)
  }
  project
}

# All changes go through here: load the newest file, change it, save it. In this
# way, an old browser tab cannot overwrite newer work.
.update_screening_project <- function(path, change) {
  project <- change(.load_screening_project(path))
  .save_screening_project(project, path)
  project
}

#.........................................
# Human screening ----
#.........................................

# Answers are locked when both reviewers have marked themselves as finished
.screening_locked <- function(project) {
  all(.screening_reviewers %in% project$finished)
}

# One value per record, NA where the reviewer has not answered
.screening_decisions <- function(project, reviewer) {
  out <- rep(NA_integer_, nrow(project$refs))
  own <- project$decisions[project$decisions$reviewer == reviewer, ]
  out[own$studyid] <- own$decision
  out
}

.screening_notes <- function(project, reviewer) {
  out <- rep("", nrow(project$refs))
  own <- project$decisions[project$decisions$reviewer == reviewer, ]
  out[own$studyid] <- own$note
  out
}

.first_unscreened <- function(project, reviewer) {
  missing <- which(is.na(.screening_decisions(project, reviewer)))
  if (length(missing) == 0) 1L else missing[1]
}

# `note = NULL` keeps the note that is already saved
.record_decision <- function(project, reviewer, studyid, decision, note = NULL) {
  if (.screening_locked(project)) {
    stop("Both reviewers have finished, so answers are locked.", call. = FALSE)
  }
  stopifnot(
    reviewer %in% .screening_reviewers,
    studyid %in% seq_len(nrow(project$refs)),
    decision %in% c(0, 1)
  )

  if (is.null(note)) note <- .screening_notes(project, reviewer)[studyid]

  d <- project$decisions
  d <- d[!(d$studyid == studyid & d$reviewer == reviewer), ]
  project$decisions <- rbind(
    d,
    data.frame(
      studyid = as.integer(studyid), reviewer = reviewer,
      decision = as.integer(decision), note = note
    )
  )
  project
}

# Save a changed note for a record that already has a decision
.save_note <- function(project, reviewer, studyid, note) {
  decision <- .screening_decisions(project, reviewer)[studyid]
  if (is.null(note) || is.na(decision) || .screening_locked(project) ||
      identical(note, .screening_notes(project, reviewer)[studyid])) {
    return(project)
  }
  .record_decision(project, reviewer, studyid, decision, note)
}

.finish_screening <- function(project, reviewer) {
  stopifnot(reviewer %in% .screening_reviewers)
  if (anyNA(.screening_decisions(project, reviewer))) {
    stop("Screen all records before finishing.", call. = FALSE)
  }
  project$finished <- union(project$finished, reviewer)
  project
}

#.........................................
# Settling disagreements ----
#.........................................

# Study IDs where human1 and human2 disagree
.human_disagreements <- function(project) {
  which(.screening_decisions(project, "human1") != .screening_decisions(project, "human2"))
}

.record_final <- function(project, studyid, decision) {
  if (!.screening_locked(project)) stop("Both reviewers must finish first.", call. = FALSE)
  stopifnot(studyid %in% .human_disagreements(project), decision %in% c(0, 1))

  f <- project$final[project$final$studyid != studyid, ]
  project$final <- rbind(f, data.frame(studyid = as.integer(studyid), decision = as.integer(decision)))
  project
}

# The agreed decision, or the settled one. NA where nothing is decided yet.
.final_decisions <- function(project) {
  h1 <- .screening_decisions(project, "human1")
  h2 <- .screening_decisions(project, "human2")

  out <- rep(NA_integer_, length(h1))
  agree <- which(h1 == h2)
  out[agree] <- h1[agree]

  settled <- project$final[project$final$studyid %in% .human_disagreements(project), ]
  out[settled$studyid] <- settled$decision
  out
}
