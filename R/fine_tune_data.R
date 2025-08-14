#' @title Function to generate dataset to be used for fine-tuning models
#'
#' @description
#'  This function creates the initial data that can be used to fine tune models from OpenAI.
#'
#' @template common-arg
#'
#' @return A dataset of class `'ft_data'`.
#'
#' @note The dataset contains at least the following variables:
#' \tabular{lll}{
#'  \bold{studyid} \tab \code{integer/character/factor} \tab indicating the study ID of the reference. \cr
#'  \bold{title} \tab \code{character} \tab indicating the title of the reference. \cr
#'  \bold{abstract} \tab \code{character} \tab indicating the abstract of the reference. \cr
#'  \bold{question} \tab \code{character} \tab indicating the final question sent to OpenAI's GPT API models for training. \cr
#' }
#'
#' @seealso [save_fine_tune_data()]
#'
#' @examples
#' # Extract 5 irrelevant and relevant records, respectively.
#' dat <- filges2015_dat[c(1:5, 261:265),]
#'
#' prompt <- "Is this study about functional family therapy?"
#'
#' dat <-
#'   create_fine_tune_data(
#'     data = dat,
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract
#'    )
#'
#' dat
#'
#' @export

create_fine_tune_data <- function(data, prompt, studyid, title, abstract){

  study_id <- if (missing(studyid)) 1:nrow(data) else data |> pull({{ studyid }})

  dat <-
    data |>
    dplyr::mutate(
      studyid = study_id,
      studyid = factor(studyid, levels = unique(studyid)),

      question_raw = paste0(
        prompt,
        " Now, evaluate the following title and abstract for",
        " Study ", studyid, ":",
        " -Title: ", {{ title }},
        " -Abstract: ", {{ abstract }}
      ),
      # removing line shift symbols and creating the main question
      question = stringr::str_replace_all(question_raw, "\n\n", " "),
      question = stringr::str_remove_all(question, "\n")

    ) |>
    dplyr::relocate(c(studyid, {{ title }}, {{ abstract }}), .before = question) |>
    dplyr::select(-question_raw)

  class(dat) <- c("ft_data", class(dat))

  dat

}

#' @title Function to write/save fine tune dataset in required jsonl format
#'
#' @description
#'  This function creates `jsonl` training data that can be used to fine tune models from OpenAI.
#'  To generate a fine tuned model, this writing data can be uploaded to
#'  \url{https://platform.openai.com/finetune/}.
#'
#' @param data Dataset with questions strings that should be used for training.
#'  The data must be of class `'ft_data'`, containing two variables named question and true_answer.
#' @param role_and_subject Descriptions of the role of the GPT model and the subject under review, respectively.
#' @param file A character string naming the file to write to. If not specified the
#'  written file name and format will be `"fine_tune_data.jsonl"`.
#' @param true_answer Optional name of the variable containing the true answers/decisions
#'  used for training. Only relevant, if the the dataset contains a variable with
#'  the name true_answer.
#' @param roles String variable defining the various role the model should take.
#'  Default is `roles = c("system", "user", "assistant")`.
#'
#' @return A `jsonl` dataset to the set working directory.
#'
#' @seealso [create_fine_tune_data()]
#'
#'
#' @examples
#' \dontrun{
#' # Extract 5 irrelevant and relevant records, respectively.
#' library(dplyr)
#'
#' dat <- filges2015_dat[c(1:5, 261:265),]
#'
#' prompt <- "Is this study about functional family therapy?"
#'
#' ft_dat <-
#'   generate_ft_data(
#'     data = dat,
#'     prompt = prompt,
#'     studyid = studyid,
#'     title = title,
#'     abstract = abstract
#'     ) |>
#'     mutate(true_answer = if_else(human_code == 1, "Include", "Exclude"))
#'
#' role_subject <- paste0(
#'   "Act as a systematic reviewer that is screening study titles and ",
#'   "abstracts for your systematic reviews regarding the the effects ",
#'   "of family-based interventions on drug abuse reduction for young ",
#'   "people in treatment for non-opioid drug use."
#' )
#'
#' # Saving data in jsonl format (required format by OpenAI)
#' save_fine_tune_data(
#'   data = ft_dat,
#'   role_and_subject = role_subject,
#'   file = "fine_tune_data.jsonl"
#' )
#'}
#'
#' @export

save_fine_tune_data <-
  function(
    data,
    role_and_subject,
    file,
    true_answer,
    roles = c("system", "user", "assistant")
  ) {

    # Stop messages

    if (!inherits(data, "ft_data")) stop("The data must be of class 'ft_data'.")

    if (nrow(data) < 10) {
      stop(
        paste0(
          "The dataset must contain at least 10 rows. For fine tuning to work, ",
          "it requires at least 10 examples."
          )
        )
    }

    if (missing(true_answer) && any(!"true_answer" %in% names(data))) {
      stop(
        paste0(
          "You must either add a true_answer variable with the true inclusion decision ",
          "or specify the name of variable with the true inclusion decison via the ",
          "true_answer argument."))
    }

    file_name <- if (!missing(file)) file else "fine_tune_data.jsonl"
    if (!stringr::str_detect(file_name, "jsonl")) stop("For the data to work, it must be saved in jsonl format")

    if (!missing(true_answer)){
      data$true_answer <- data |> dplyr::pull({{ true_answer }})
    }

    dat_list <- purrr::map(
      1:nrow(data), .f = .create_message,
      ft_data = data,
      role_subjet = role_and_subject,
      role = roles
    )

    json_data <-
      dat_list |>
      purrr::map(jsonlite::toJSON, auto_unbox = TRUE) |>
      unlist() |>
      paste(collapse = "\n")

    write(json_data, file = file_name)

  }
