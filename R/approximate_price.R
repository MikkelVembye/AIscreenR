
#' @title Approximate price estimation for title and abstract screening using OpenAI's GPT API models
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' This function supports the approximation of the price of the total title and abstract
#' screening with OpenAI's GPT API models. The function only provide approximately accurate price
#' estimates for screenings. When detailed descriptions are used,
#' this will increase the completion tokens with an unknown amount.
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models, including gpt-4 models. Default = `"gpt-4o-mini"`.
#'   Find available model at
#'   \url{https://platform.openai.com/docs/models/model-endpoint-compatibility}.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to ChatGPT. This can be useful to test consistency
#'   between answers. Default is `1` but when using 3.5 models, we recommend setting this
#'   value to `10`.
#' @param top_p 'An alternative to sampling with temperature, called nucleus sampling,
#'   where the model considers the results of the tokens with top_p probability mass.
#'   So 0.1 means only the tokens comprising the top 10% probability mass are considered.
#'   We generally recommend altering this or temperature but not both.' (OPEN-AI). Default is 1.
#'   Find documentation at
#' \url{https://platform.openai.com/docs/api-reference/chat/create#chat/create-top_p}.
#'
#' @param token_word_ratio The multiplier used to approximate the number of tokens per word.
#'   Default is `1.6` which we empirically have found to be the average number of tokens per word.
#'
#' @return An object of class \code{"gpt_price"}. The object is a list containing the following
#' components:
#' \item{price}{numerical value indicating the total approximate price (in USD) of the screening across all gpt-models expected to be used for the screening.}
#' \item{price_data}{dataset with prices across all gpt models expected to be used for screening.}
#'
#' @importFrom stats df
#' @import dplyr
#' @export
#'
#' @examples
#' prompt <- "This is a prompt"
#'
#' app_price <- approximate_price_gpt(
#'   data = filges2015_dat[1:2,],
#'   prompt = prompt,
#'   studyid = studyid,
#'   title = title,
#'   abstract = abstract,
#'   model = c("gpt-4o-mini", "gpt-4"),
#'   reps = c(10, 1)
#' )
#'
#' app_price
#' app_price$price_dollar
#' app_price$price_data


approximate_price_gpt <-
  function(
    data,
    prompt,
    studyid,
    title,
    abstract,
    model = "gpt-4o-mini",
    reps = 1,
    top_p = 1,
    token_word_ratio = 1.6
  ){

    if (length(reps) > 1 && length(model) != length(reps)){
      stop("model and reps must be of the same length.")
    }


    # Ensures only viable models are used
    if (any(!is.element(model, model_prizes$model))) stop("Unknown gpt model(s) used - check model name(s).")


    if (n_distinct(prompt) != length(prompt)) stop("Do not add same prompt twice.")

    ###############################################
    # Data manipulation
    ###############################################


    if (missing(studyid)){

      dat <-
        data |>
        dplyr::mutate(
          studyid = 1:nrow(data)
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})


    } else {

      dat <-
        data |>
        dplyr::mutate(
          studyid = {{ studyid }}
        ) |>
        dplyr::relocate(studyid, .before = {{ title }})

    }


    mp_reps <- if (length(reps) > 1) 1 else length(model)

    model_length <- length(model)
    prompt_length <- length(prompt)
    studyid_length <- dplyr::n_distinct(dat$studyid)

    question_dat <-
      dat |>
      dplyr::mutate(
        dplyr::across(c({{ title }}, {{ abstract }}), ~ dplyr::if_else(
          is.na(.x) | .x == "" | .x == " ", "No information", .x, missing = "No information")
        )
      ) |>
      dplyr::slice(rep(1:nrow(dat), prompt_length)) |>
      dplyr::mutate(
        promptid = rep(paste("Prompt", 1:prompt_length), each = studyid_length),
        prompt = rep(prompt, each = studyid_length)
      ) |>
      dplyr::slice(rep(1:dplyr::n(), each = model_length)) |>
      dplyr::mutate(
        model = rep(model, studyid_length*prompt_length),
        iterations = rep(reps, studyid_length*prompt_length*mp_reps),
        #req_per_min = rep(rpm, studyid_length*dplyr::n_distinct(prompt)*mp_rpm),
        question_raw = paste0(
          prompt,
          " Now, evaluate the following title and abstract for",
          " Study ", studyid, ":",
          " -Title: ", {{ title }},
          " -Abstract: ", {{ abstract }}
        ),
        question = stringr::str_replace_all(question_raw, "\n\n", " "),
        question = stringr::str_remove_all(question, "\n")
      ) |>
      dplyr::select(-question_raw) |>
      dplyr::slice(rep(1:dplyr::n(), each = length(top_p))) |>
      mutate(
        topp = rep(top_p, studyid_length*prompt_length*model_length)
      )


    price_dat <- .price_gpt(question_dat)
    price <- sum(price_dat$total_price_dollar, na.rm = TRUE)

    res <- list(price_data = price_dat, price_dollar = price)
    class(res) <- c("gpt_price", class(res))

    res

}



