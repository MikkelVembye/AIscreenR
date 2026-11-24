# tabscreen_gpt supports custom models, multiple models and deprecated args

    Code
      res <- screen(p, model = "my-ft-model", fine_tuned = TRUE)
    Condition
      Warning:
      The `fine_tuned` argument of `tabscreen_gpt()` is deprecated as of AIscreenR 0.2.1.
      i Please use the `custom_model` argument instead.

