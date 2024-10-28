## Submission

This is the first version of AIscreenR. The package provides functions for conducting title and abstract
screening in systematic reviews by using AI tools such as OpenAI's GPT API models.

## Test environments

* local Windows 10 Enterprise, R 4.2.0
* local Windows 11 Pro, R 4.1.2
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)
* r-hub:
  * Windows Server 2022, R-release, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  * Fedora Linux, R-devel, clang, gfortran
  * Debian Linux, R-devel, GCC

## R CMD check results

There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

 * On win-builder release, devel, and oldrelease:
  Possibly misspelled words in DESCRIPTION:
  GPT (12:83)
  
This is not misspelling

 * On R CMD Check: Found the following (possibly) invalid URLs:
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
             man/rate_limits_per_minute.Rd
             man/set_api_key.Rd
             man/tabscreen_gpt.original.Rd
             man/tabscreen_gpt.tools.Rd
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
     
   
The flagged URLs can be accessed.
