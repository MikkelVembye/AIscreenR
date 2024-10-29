# Locally
devtools::spell_check()                       # fix any spelling errors
devtools::check(remote = TRUE, manual = TRUE)

# Check the package under various architectures, using CRAN build configurations

devtools::check_mac_release() # Tested Succeeded




# After all checks complete
devtools::release()                # final checklist, build and submit package

# After package is accepted to CRAN
# - Add a Github release
# - Update version number in DESCRIPTION: To 0.1.0.9999?
# - Add section for next version in NEWS: 0.1.0?
