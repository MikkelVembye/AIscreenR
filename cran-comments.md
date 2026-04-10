## Submission

This is a minor version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. In this submission, we have 

1) Adding tabscreen_groq() function to screen titles and abstracts using Groq AI.
2) Adding tabscreen_ollama() function to screen titles and abstracts using local ollama models.
3) Adding functions to read and write RIS files: read_ris_to_dataframe() and save_dataframe_to_ris().
4) Adding function to generate disagreement reports: generate_disagreement_report().
5) Adding articles for fine-tuning OpenAI models, generating disagreement reports, generating fine-tuning data and reading/writing RIS files.
6) Adding article for comparing performance of reasoning models (including gpt-5 models) with gpt-4o-mini.
7) Making it possible to steer the model's (over)inclusion behavior via the overinclusive argument in tabscreen_* functions. 
8) Updated prize data, including all up-to-date models


## Test environments

* local Windows 10 Enterprise, R 4.5.2
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)


## R CMD check results

As for previous versions, there were no ERRORs and WARNINGs.

There was 1 NOTE:

 * On win-builder release, devel, and oldrelease:
  Found the following (possibly) invalid URLs:
  URL: https://openai.com/api/pricing/
    From: man/model_prizes.Rd
    Status: 403
    Message: Forbidden
  URL: https://openai.com/blog/function-calling-and-other-api-updates
    From: man/tabscreen_gpt.original.Rd
    Status: 403
    Message: Forbidden
  URL: https://openai.com/pricing
    From: man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/
    From: DESCRIPTION
          man/AIscreenR-package.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/account/api-keys
    From: man/get_api_key.Rd
          man/set_api_key.Rd
          man/tabscreen_gpt.original.Rd
          inst/doc/Using-GPT-API-Models-For-Screening.html
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/api-reference/chat/create
    From: man/screen_errors.Rd
          man/screen_errors.chatgpt.Rd
          man/screen_errors.gpt.Rd
          man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/api-reference/chat/create#chat-create-tool_choice
    From: man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/api-reference/chat/create#chat-create-tools
    From: man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p
    From: man/approximate_price_gpt.Rd
          man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/guides/rate-limits/overview
    From: man/rate_limits_per_minute.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/guides/rate-limits/what-are-the-rate-limits-for-our-api
    From: man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/docs/models/model-endpoint-compatibility
    From: man/approximate_price_gpt.Rd
          man/rate_limits_per_minute.Rd
          man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
    Status: 403
    Message: Forbidden
  URL: https://platform.openai.com/finetune/
    From: man/save_fine_tune_data.Rd
    Status: 403
    Message: Forbidden

These are all valid URLs important for guiding the package user.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
