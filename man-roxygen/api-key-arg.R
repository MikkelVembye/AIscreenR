#' @param api_key Numerical value with your personal API key. Default setting draws
#'  on the [get_api_key()] to retrieve the API key from the R environment, so that the key is not
#'  compromised. The API key can be added to the R environment via [set_api_key()]
#'  or by using [usethis::edit_r_environ()]. In the `.Renviron` file, write `CHATGPT_KEY=INSERT_YOUR_KEY_HERE`.
#'  After entering the API key, close and save the `.Renviron` file and restart `RStudio` (ctrl + shift + F10).
#'  Alternatively, one can use [httr2::secret_make_key()], [httr2::secret_encrypt()], and
#'  [httr2::secret_decrypt()] to scramble and decrypt the API key.
