# Changelog

## AIscreenR 0.3.2

CRAN release: 2026-04-20

## AIscreenR 0.3.1.9008

### Minor improvements

- Updating the default inclusion threshold and documentation hereof when
  conducting replicate screenings to be aligned with the finding from
  Vembye et al. (2025).
- Updating the handling of coding missing abstracts in the vignette now
  when using
  [`read_ris_to_dataframe()`](https://mikkelvembye.github.io/AIscreenR/reference/read_ris_to_dataframe.md).
- Better error messages for unknown GPT models when using newer or
  fine-tuned models.

### Bug fixes

- Fixed bug in
  [`report()`](https://mikkelvembye.github.io/AIscreenR/reference/report.md)
  when rendering large amounts of text.
- Fixed bug in
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  when using multiple reps and gpt-5 models.
- Fixed bug in
  [`screen_analyzer()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_analyzer.md)
  when working with multiple prompts, models, and reps.
- Correcting path in generating-disagreement-reports article.
- Set max_tries in
  [`rate_limits_per_minute()`](https://mikkelvembye.github.io/AIscreenR/reference/rate_limits_per_minute.md)
  to avoid message from httr2.

### Further documentation

- Add installation guide to ollama article.
- Include an example of fine-tuning a model and using it to fine-tuning
  article.

## AIscreenR 0.3.1

CRAN release: 2026-04-13

- Updating documentation of
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)

## AIscreenR 0.3.0

CRAN release: 2026-04-13

### New features

- Adding
  [`tabscreen_groq()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_groq.md)
  function to screen titles and abstracts using Groq AI.
- Adding
  [`tabscreen_ollama()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_ollama.md)
  function to screen titles and abstracts using local ollama models.
- Adding functions to read and write RIS files:
  [`read_ris_to_dataframe()`](https://mikkelvembye.github.io/AIscreenR/reference/read_ris_to_dataframe.md)
  and
  [`save_dataframe_to_ris()`](https://mikkelvembye.github.io/AIscreenR/reference/save_dataframe_to_ris.md).
- Adding function to generate disagreement reports:
  `generate_disagreement_report()`.
- Making new refinements to the tabscreen\_\* functions. Making it
  possible to steer the model’s (over)inclusion behavior via the
  `overinclusive = TRUE` argument in tabscreen\_\* functions.

### Further documentation

- Adding articles for fine-tuning OpenAI models, generating disagreement
  reports, generating fine-tuning data and reading/writing RIS files.
- Adding article for comparing performance of reasoning models
  (including gpt-5 models) with gpt-4o-mini.

### Minor improvements

- Updated prize data, including all up-to-date models

## AIscreenR 0.2.0

CRAN release: 2025-08-18

- Adding Thomas Olsen as co-author.

### New features

- Adding
  [`create_fine_tune_data()`](https://mikkelvembye.github.io/AIscreenR/reference/create_fine_tune_data.md)
  and `write_fine_tune_data()` to generate data for fine tuning OpenAI’s
  models.

### Minor improvements

- Minor change in the setup of the vignette.
- Updated prize data, including all up-to-date models.

## AIscreenR 0.1.1

CRAN release: 2024-11-26

- A typo in the vignette has been corrected.
- The vignette now draws on functions from synthesisr instead of
  revtools to handle RIS files.
- [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  now treats the study ID variable as a factor to keep original order of
  the dataset with titles and abstracts.

## AIscreenR 0.1.0

CRAN release: 2024-11-08

- This is the first release of AIscreenR.
