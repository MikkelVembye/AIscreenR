## Submission

This is a patch version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. The most substantial changes in this version is that we have: 

1) Fixed bug in `report()` when rendering large amounts of text.
2) Fixed bug in `tabscreen_gpt()` when using multiple reps and gpt-5 models.
3) Fixed bug in `screen_analyzer()` when working with multiple prompts, models, and reps.
4) Set max_tries in `rate_limits_per_minute()` to avoid message from httr2.
5) Updating the default inclusion threshold and documentation hereof when conducting replicate screenings to be aligned with the finding from Vembye et al. (2025).
6) Updating the handling of coding missing abstracts in the vignette now when using `read_ris_to_dataframe()`.
7) Better error messages for unknown GPT models when using newer or fine-tuned models.

## Test environments

* local Windows 10 Enterprise R 4.5.2 and R 4.5.3 
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
  URL: https://psycnet.apa.org/record/2026-37236-001
    From: man/sample_references.Rd
          man/tabscreen_gpt.original.Rd
          man/tabscreen_gpt.tools.Rd
          man/tabscreen_groq.Rd
          man/tabscreen_ollama.Rd
          inst/doc/Using-GPT-API-Models-For-Screening.html
    Status: 404
    Message: Not Found

This is the correct URL

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
