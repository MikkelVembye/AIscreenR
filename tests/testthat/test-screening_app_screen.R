# Tests for human screening and settling disagreements in the screening app

test_that(".record_decision() adds and replaces answers and keeps the note", {
  path <- local_screening_project(2)
  p <- .load_screening_project(path)

  p <- .record_decision(p, "human1", 1, 1, "first note")
  p <- .record_decision(p, "human1", 1, 0) # No note given: the old one is kept

  expect_equal(.screening_decisions(p, "human1"), c(0L, NA))
  expect_equal(.screening_notes(p, "human1"), c("first note", ""))
  expect_equal(.screening_decisions(p, "human2"), c(NA_integer_, NA_integer_))
  expect_equal(.first_unscreened(p, "human1"), 2)
  expect_equal(.save_note(p, "human1", 2, "no decision yet"), p)
})

test_that("a reviewer must screen everything before finishing", {
  path <- local_screening_project(2)
  p <- .record_decision(.load_screening_project(path), "human1", 1, 1)

  expect_error(.finish_screening(p, "human1"), "Screen all records")
})

test_that("answers are locked when both have finished, and only then", {
  path <- local_screening_project(2)
  p <- .load_screening_project(path)
  for (i in 1:2) p <- .record_decision(p, "human1", i, 1)
  p <- .finish_screening(p, "human1")

  expect_false(.screening_locked(p))
  p <- .record_decision(p, "human1", 1, 0) # Still allowed
  expect_error(.record_final(p, 1, 1), "Both reviewers must finish")

  for (i in 1:2) p <- .record_decision(p, "human2", i, 1)
  p <- .finish_screening(p, "human2")

  expect_true(.screening_locked(p))
  expect_error(.record_decision(p, "human1", 1, 1), "locked")
})

test_that("final decisions come from agreement or from settling", {
  path <- local_screening_project(4)
  p <- screen_all(path, human1 = c(1, 0, 1, 0), human2 = c(1, 0, 0, 1))

  expect_equal(.human_disagreements(p), c(3, 4))
  expect_equal(.final_decisions(p), c(1L, 0L, NA, NA))

  p <- .record_final(p, 3, 1)
  p <- .record_final(p, 3, 0) # Changing a settled decision replaces it

  expect_equal(.final_decisions(p), c(1L, 0L, 0L, NA))
  expect_equal(nrow(p$final), 1)
  expect_error(.record_final(p, 1, 0)) # Not a disagreement
})

test_that("human2 never sees the answers or notes of human1", {
  skip_if_no_app()
  path <- local_screening_project(2)

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    click(session, "include", "human1_1", note = "SECRET one")
    click(session, "exclude", "human1_2", note = "SECRET two")
    click(session, "back", "human1_2", note = "SECRET two")
    expect_match(output$screen_panel$html, "SECRET one") # human1 sees their own note

    click(session, "handover", "human1_1", note = "SECRET one")
    expect_match(output$screen_panel$html, "Who is screening")
    expect_no_match(output$screen_panel$html, "SECRET")

    click(session, "start", "human2")
    expect_match(output$screen_panel$html, "record 1 of 2 \\(0 screened\\)")
    expect_match(output$screen_panel$html, "not screened yet")
    expect_no_match(output$screen_panel$html, "SECRET")
  })
})

test_that("answers are saved after every click and survive a restart", {
  skip_if_no_app()
  path <- local_screening_project(3)

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    click(session, "include", "human1_1", note = "keep")

    saved <- .load_screening_project(path)
    expect_equal(.screening_decisions(saved, "human1"), c(1L, NA, NA))
    expect_equal(.screening_notes(saved, "human1")[1], "keep")
    expect_equal(position(), 2)
  })

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    expect_equal(position(), 2) # Continues at the first record without an answer
  })
})

test_that("a double click cannot answer a record that has not been seen", {
  skip_if_no_app()
  path <- local_screening_project(3)

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    click(session, "include", "human1_1")
    click(session, "include", "human1_1") # Second click on the old button

    expect_equal(.screening_decisions(project(), "human1"), c(1L, NA, NA))
    expect_equal(position(), 2)
  })
})

test_that("Next needs an answer, and a changed note is saved when moving", {
  skip_if_no_app()
  path <- local_screening_project(2)

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    click(session, "next", "human1_1")
    expect_equal(position(), 1)

    click(session, "include", "human1_1", note = "old")
    click(session, "back", "human1_2", note = "")
    click(session, "next", "human1_1", note = "new")

    expect_equal(position(), 2)
    expect_equal(.screening_notes(project(), "human1")[1], "new")
  })
})

test_that("a background save does not redraw the Screen tab and wipe a typed note", {
  skip_if_no_app()
  path <- local_screening_project(2)
  renders <- 0
  label <- .decision_label
  # The Screen tab calls .decision_label() once every time it is drawn
  testthat::local_mocked_bindings(.decision_label = function(x, missing = "No answer") {
    renders <<- renders + 1
    label(x, missing)
  })

  shiny::testServer(app_dir(), {
    click(session, "start", "human1")
    invisible(output$screen_panel)
    before <- renders

    change_project(function(p) .add_ai_answers(p, fake_ai_answers(c(1, 0))))
    session$flushReact()
    invisible(output$screen_panel)

    expect_gt(before, 0)
    expect_equal(renders, before)
  })
})

test_that("both reviewers finishing locks screening and opens the other tabs", {
  skip_if_no_app()
  path <- local_screening_project(1)

  shiny::testServer(app_dir(), {
    expect_match(output$reconcile_panel$html, "both reviewers")
    expect_match(output$compare_panel$html, "both reviewers")
    expect_match(output$export_panel$html, "both reviewers")

    for (r in c("human1", "human2")) {
      key <- paste0(r, "_1")
      click(session, "start", r)
      click(session, "include", key)
      expect_match(output$screen_panel$html, "I am finished")
      click(session, "finish", key)
      if (r == "human1") click(session, "handover", key)
    }

    expect_match(output$screen_panel$html, "answers are locked")
    expect_match(output$reconcile_panel$html, "agree on all records")
    expect_match(output$export_panel$html, "Download Excel")
  })
})

test_that("disagreements are settled on the Reconcile tab without showing the AI", {
  skip_if_no_app()
  path <- local_screening_project(3)
  screen_all(path, human1 = c(1, 1, 0), human2 = c(0, 1, 1))
  .update_screening_project(path, function(p) .add_ai_answers(p, fake_ai_answers(c(1, 1, 1))))

  shiny::testServer(app_dir(), {
    expect_match(output$reconcile_panel$html, "Disagreement 1 of 2 \\(0 settled\\)")
    expect_no_match(output$reconcile_panel$html, "\\bAI\\b")

    click(session, "include", 1, input = "reconcile_action")
    click(session, "include", 1, input = "reconcile_action") # Double click is ignored
    expect_match(output$reconcile_panel$html, "Disagreement 2 of 2 \\(1 settled\\)")

    click(session, "exclude", 3, input = "reconcile_action")

    expect_equal(.final_decisions(project()), c(1L, 1L, 0L))
  })
})
