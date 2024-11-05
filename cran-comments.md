## Submission

This is the first version of AIscreenR. The package provides functions for conducting title and abstract
screening in systematic reviews by using AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models.

## Test environments

* local Windows 10 Enterprise, R 4.4.0
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)


## R CMD check results

There were no ERRORs and WARNINGs.

There was 1 NOTE:

 * On win-builder release, devel, and oldrelease:
  Found the following (possibly) invalid URLs:
  URL: https://auth0.openai.com/u/login/identifier?state=hKFo2SBqQjNHSlc1ejIyREpUb01hdDF2OHEzQy12NnJwWlFUN6Fur3VuaXZlcnNhbC1sb2dpbqN0aWTZIEJSOWJaamdKLWswNGlfWDQ2NER1OXJmVUNpVmVzVjZfo2NpZNkgRFJpdnNubTJNdTQyVDNLT3BxZHR3QjNOWXZpSFl6d0Q
    From: inst/doc/Using-GPT-API-Models-For-Screening.html
    Status: 403
    Message: Forbidden
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

These are all valid URLs important for guiding the package user.
