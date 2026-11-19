# report() warns when no gpt_answer is given

    Code
      suppressMessages(make_report())
    Condition
      Warning in `report()`:
      Argument 'gpt_answer' is missing. The report will include only numeric GPT answers from 'final_decision_gpt_num'.

# report() accepts full paths and appends the qmd extension

    Code
      suppressMessages(report(data = report_dat(), studyid = studyid, title = title,
      abstract = abstract, human_code = human_code, final_decision_gpt_num = final_decision_gpt_num,
      file = file.path(tmp, "full.qmd"), format = "html", document_title = "Doc",
      open = FALSE, directory = withr::local_tempdir()))
    Condition
      Warning in `report()`:
      Argument 'file' includes a directory. Using that directory and ignoring 'directory'.
      Warning in `report()`:
      Argument 'gpt_answer' is missing. The report will include only numeric GPT answers from 'final_decision_gpt_num'.

