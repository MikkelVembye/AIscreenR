.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Welcome to the AIscreenR. Be aware ",
  "that we do not take\nany responsibility for the cost of using this software.\n\n",
  "If you experience any breakdowns, check: https://status.openai.com/")
  )

  suppressMessages(conflicted::conflicts_prefer(testthat::is_null))

}
