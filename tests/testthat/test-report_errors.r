library(testthat)
library(AIscreenR)
library(withr)

# Use a small subset of the disagreements data for testing
test_data <- disagreements[1, , drop = FALSE]

test_that("report() function generates a report correctly", {

  # Use a temporary directory to avoid creating files in the project
  withr::with_tempdir({
    report_file <- "test_report.qmd"
    
    # Run the report function
    report_obj <- report(
      data = test_data,
      studyid = studyid,
      title = title,
      abstract = abstract,
      gpt_answer = longest_answer,
      human_code = human_code,
      final_decision_gpt_num = final_decision_gpt_num,
      file = report_file,
      format = "html",
      document_title = "Test Report",
      document_subtitle = "Disagreement between humans and GPT",
      open = FALSE,
      directory = "."
    )
    
    # Check that the output files are created
    expect_true(file.exists(report_file))
    expect_true(file.exists("test_report.html"))
    
    # Check the returned object
    expect_equal(basename(report_obj), "test_report.html")
    
    # Check some content of the generated .qmd file
    # Read the whole file into a single string to make the test robust
    qmd_lines <- readLines(report_file)
    expect_true(any(grepl("title: \" Test Report \"", qmd_lines, fixed = TRUE)))
    expect_true(any(grepl("subtitle: \" Disagreement between humans and GPT \"", qmd_lines, fixed = TRUE)))
  })
})

test_that("report() error structure.", {
  withr::with_tempdir({
    # Test for missing arguments
    expect_error(
      report(data = test_data, title = title),
      "Argument 'studyid' is missing"
    )
    
    expect_error(
      report(studyid = studyid, title = title),
      "Argument 'data' is missing"
    )
    
    # Test for invalid format
    expect_error(
      report(
        data = test_data, studyid = studyid, title = title, abstract = abstract,
        gpt_answer = longest_answer, human_code = human_code,
        final_decision_gpt_num = final_decision_gpt_num,
        file = "test.qmd", document_title = "t", format = "invalid_format"
      ),
      "Invalid format"
    )
    
    # Test for non-existent directory
    expect_error(
      report(
        data = test_data, studyid = studyid, title = title, abstract = abstract,
        gpt_answer = longest_answer, human_code = human_code,
        final_decision_gpt_num = final_decision_gpt_num,
        file = "test.qmd", document_title = "t", format = 'html', directory = "a_folder_that_does_not_exist"
      ),
      "The specified directory does not exist"
    )
  })
})