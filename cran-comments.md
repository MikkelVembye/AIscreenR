## Submission

This is a patch version update of AIscreenR. The package provides functions for conducting title and abstract screening in systematic reviews with AI models, such as OpenAI's GPT (Generative Pre-trained Transformer) API (Application Programming Interface) models. In this submission, we have 

1) Updating documentation of main function in the package, i.e., tabscreen_gpt()


## Test environments

* local Windows 10 Enterprise R 4.5.2 
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* mac-builder (release)


## R CMD check results

As for previous versions, there were no ERRORs and WARNINGs.

There was 2 NOTEs:

* On win-builder release and oldrelease:

  Author field differs from that derived from Authors@R
  Author:    'Mikkel H. Vembye [aut, cre] (ORCID: <https://orcid.org/0000-0001-9071-0724>), Thomas Olsen [aut]'
  Authors@R: 'Mikkel H. Vembye [aut, cre] (<https://orcid.org/0000-0001-9071-0724>), Thomas Olsen [aut]'

We have not experienced this issue in previous releases, and we are uncertain how to resolve this discrepancy and whether it is consequential for the package building. 

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
