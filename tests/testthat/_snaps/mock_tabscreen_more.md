# tabscreen_claude() validates its remaining arguments

    Code
      res <- tryCatch(f(reasoning_effort = "bogus"), error = function(e)
        conditionMessage(e))
    Condition
      Warning:
      reasoning_effort must be either 'low', 'medium', 'high', 'xhigh' or 'max'. Setting reasoning_effort to NULL.

