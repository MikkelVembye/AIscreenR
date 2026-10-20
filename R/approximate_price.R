
#' @title Approximate price estimation for title and abstract screening using OpenAI's GPT API models
#'
#' @description
#' `r lifecycle::badge("experimental")`<br>
#' <br>
#' This function supports the approximation of the price of title and abstract
#' screenings when using OpenAI's GPT API models. The function only provide approximately accurate price
#' estimates. When detailed descriptions are used,
#' this will increase the completion tokens with an unknown amount.
#'
#' @template common-arg
#' @param model Character string with the name of the completion model. Can take
#'   multiple models. Default = `"gpt-6-luna"`. Be aware that gpt-4 and gpt-3.5 models
#'   are deprecated and can no longer be used.
#'   Find available model at
#'   \url{https://developers.openai.com/api/docs/models}.
#' @param reps Numerical value indicating the number of times the same
#'   question should be sent to the GPT server. This can be useful to test consistency
#'   between answers. Default is `1` but when using cheaper models, we recommend setting this
#'   value to `10`.
#' @param token_word_ratio The multiplier used to approximate the number of tokens per word.
#'   Default is `1.6` which we empirically have found to be the average number of tokens per word.
#' @param reasoning_effort Character string indicating the level of reasoning effort required for the task. Default is `"medium"`.
#'  Can take the values `"low"`, `"medium"`, and `"high"`. Only applicable for gpt models.
#'  Used to approximate the reasoning tokens. The approximation is based on measured reasoning token use with the default verbosity (`"low"`).
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
#'   model = c("gpt-6-luna", "gpt-5-nano"),
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
    model = "gpt-6-luna",
    reps = 1,
    token_word_ratio = 1.6,
    reasoning_effort = "medium"
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
          studyid = seq_len(nrow(data))
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
      dplyr::slice(rep(seq_len(nrow(dat)), prompt_length)) |>
      dplyr::mutate(
        promptid = rep(paste("Prompt", 1:prompt_length), each = studyid_length),
        prompt = rep(prompt, each = studyid_length)
      ) |>
      dplyr::slice(rep(seq_len(dplyr::n()), each = model_length)) |>
      dplyr::mutate(
        model = rep(model, studyid_length*prompt_length),
        iterations = rep(reps, studyid_length*prompt_length*mp_reps),
        reasoning_effort = reasoning_effort,
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
      dplyr::select(-question_raw)


    price_dat <- .price_gpt(question_dat)
    price <- sum(price_dat$total_price_dollar, na.rm = TRUE)

    res <- list(price_data = price_dat, price_dollar = price)
    class(res) <- c("gpt_price", class(res))

    res

}