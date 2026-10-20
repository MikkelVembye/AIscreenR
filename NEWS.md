# AIscreenR 0.5.2

## Major improvements
* All gpt-4 and gpt-3.5 models (e.g., `gpt-4`, `gpt-4o`, `gpt-4o-mini`, `gpt-4.1`) are deprecated and have been removed from `model_prizes`. `tabscreen_gpt()` and `tabscreen_gpt.tools()` now stop if one of these models is used, also when `custom_model = TRUE`.
* The `top_p` argument has been removed from `tabscreen_gpt()`, `tabscreen_gpt.tools()`, and `approximate_price_gpt()`, and the `top_p` variable is no longer part of their output. The argument is unchanged for the other providers (Groq, Mistral, Gemini, and Ollama).
* The default model is now `"gpt-6-luna"` in `tabscreen_gpt()`, `tabscreen_gpt.tools()`, `approximate_price_gpt()`, and `rate_limits_per_minute()`.
* `reasoning_effort` and `verbosity` are now added to the request body for all gpt-5 and newer models.
* Refactoring the price calculations. The reasoning token multipliers have been re-measured per model and reasoning effort, and prices are now looked up with a join on `model_prizes`.

## Minor improvements
* `screen_analyzer()` now also works for data without a `top_p` variable.


# AIscreenR 0.5.1

## New features
* Adding the `confidence` argument to all tabscreen functions. When `confidence = TRUE`, the model also reports how confident it is in each decision (0-100), returned in the new `confidence` variable. When `reps > 1`, the aggregated data also contains `mean_confidence`. The new `gpt_confidence` argument in `report()` includes the confidence in the report.

# AIscreenR 0.5.0

## New features
* Adding `solve_or_guess()` and `rank_one_diagnostic()` to evaluate screening performance without requiring a gold-standard label set. `solve_or_guess()` estimates each rater's (system or human) solving probability from patterns of agreement alone, and `rank_one_diagnostic()` checks whether that model's core assumption holds for a given fit.

## Bug fixes
* `read_ris_to_dataframe()` now repairs invalid UTF-8 characters to avoid data loss.

# AIscreenR 0.4.0

## New features
* Adding `tabscreen_mistral()` and `get_api_key_mistral()` functions to screen titles and abstracts using Mistral's API models.
* Adding `tabscreen_gemini()` and `get_api_key_gemini()` function to screen titles and abstracts using Gemini's API models.
* Adding `tabscreen_claude()` and `get_api_key_anthropic()` function to screen titles and abstracts using Anthropics's API models.

## Major improvements
* Migrating from chat/completions endpoint to responses for all OpenAI functions. This includes:
    * `tabscreen_gpt()`
    * `screen_errors.gpt()`
    * `rate_limits_per_minute()`

## Minor improvements
* Updated the prize function with up-to-date prizes for the GPT-5 models. 
* Adding the n_screened and n_missing variables to the key results in `screen_analyzer()`.


# AIscreenR 0.3.2

## Minor improvements
* Updating the default inclusion threshold and documentation hereof when conducting replicate screenings to be aligned with the finding from Vembye et al. (2025).
* Updating the handling of coding missing abstracts in the vignette now when using `read_ris_to_dataframe()`.
* Better error messages for unknown GPT models when using newer or fine-tuned models.


## Bug fixes
* Fixed bug in `report()` when rendering large amounts of text.
* Fixed bug in `tabscreen_gpt()` when using multiple reps and gpt-5 models.
* Fixed bug in `screen_analyzer()` when working with multiple prompts, models, and reps.
* Correcting path in generating-disagreement-reports article.
* Set max_tries in `rate_limits_per_minute()` to avoid message from httr2.

## Further documentation
* Add installation guide to ollama article.
* Include an example of fine-tuning a model and using it to fine-tuning article.

# AIscreenR 0.3.1

* Updating documentation of `tabscreen_gpt()`


# AIscreenR 0.3.0

## New features
* Adding `tabscreen_groq()` function to screen titles and abstracts using Groq AI.
* Adding `tabscreen_ollama()` function to screen titles and abstracts using local ollama models.
* Adding functions to read and write RIS files: `read_ris_to_dataframe()` and `save_dataframe_to_ris()`.
* Adding function to generate disagreement reports: `generate_disagreement_report()`.
* Making new refinements to the tabscreen_* functions. Making it possible to steer the model's (over)inclusion behavior via the `overinclusive = TRUE` argument in tabscreen_* functions. 

## Further documentation
* Adding articles for fine-tuning OpenAI models, generating disagreement reports, generating fine-tuning data and reading/writing RIS files.
* Adding article for comparing performance of reasoning models (including gpt-5 models) with gpt-4o-mini.

## Minor improvements
* Updated prize data, including all up-to-date models


# AIscreenR 0.2.0

* Adding Thomas Olsen as co-author.

## New features
* Adding `create_fine_tune_data()` and `write_fine_tune_data()` to generate data for fine tuning OpenAI's models.

## Minor improvements
* Minor change in the setup of the vignette.
* Updated prize data, including all up-to-date models.


# AIscreenR 0.1.1

* A typo in the vignette has been corrected.
* The vignette now draws on functions from synthesisr instead of revtools to handle RIS files.
* `tabscreen_gpt()` now treats the study ID variable as a factor to keep original order of the dataset with titles and abstracts.

# AIscreenR 0.1.0

* This is the first release of AIscreenR.
