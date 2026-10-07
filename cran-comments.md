## Submission

This is a minor version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. The most substantial change in this version is that we have:

1) Added `solve_or_guess()` and `rank_one_diagnostic()` to evaluate screening performance without requiring a gold-standard label set.
2) Fixed `read_ris_to_dataframe()` so it repairs invalid UTF-8 characters to avoid data loss.


## Test environments

* local Windows 11 Enterprise, R 4.6.0
* ubuntu (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)


## R CMD check results

As for previous versions, there were no ERRORs and WARNINGs.

There was 1 NOTE:

* On win-builder oldrelease:

  Found the following (possibly) invalid URLs:
  URL: https://psycnet.apa.org/record/2026-37236-001
    From: man/tabscreen_claude.Rd
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
  This is the correct URL.


## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
