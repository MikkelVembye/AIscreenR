## Submission

This is a patch version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. The most substantial change in this version is that we have:

1) Made all GPT functions work with GPT-6 models. The current 0.5.0 CRAN version does not work with the newest models


## Test environments

* local Windows 11 Enterprise, R 4.6.1 and R 4.6.0
* ubuntu 24.04 LTS (on Github), R devel, release, oldrelease
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
    From: inst/doc/Using-GPT-API-Models-For-Screening.html
    Status: 403
    Message: Forbidden
  This is the correct URL.


## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
