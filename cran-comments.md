## Submission

This is a minor version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. The most substantial changes in this version is that we have: 

1) Adding `tabscreen_mistral()` and `get_api_key_mistral()` functions to screen titles and abstracts using Mistral's API models.
2) Adding `tabscreen_gemini()` and `get_api_key_gemini()` function to screen titles and abstracts using Gemini's API models.
3) Adding `tabscreen_claude()` and `get_api_key_anthropic()` function to screen titles and abstracts using Anthropics's API models.
4) Migrating from chat/completions endpoint to responses for all OpenAI functions in order to use OpenAI's GPT-5.2 and above.


## Test environments

* local Windows 10 Enterprise R 4.6.0 
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)


## R CMD check results

As for previous versions, there were no ERRORs and WARNINGs.

There was 1 NOTE:

* On win-builder oldrelease:

  Found the following (possibly) invalid URLs:
  URL: https://psycnet.apa.org/record/2026-37236-001
    From: man/sample_references.Rd
          man/tabscreen_claude.Rd
          man/tabscreen_gemini.Rd
          man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
          man/tabscreen_gpt.tools_responses.Rd
          man/tabscreen_groq.Rd
          man/tabscreen_mistral.Rd
          man/tabscreen_ollama.Rd
          inst/doc/Using-GPT-API-Models-For-Screening.html
    Status: 403
    Message: Forbidden

This is the correct URL

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
