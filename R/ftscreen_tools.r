# Functions and tools for ftscreen

supplementary_check_description <- paste0(
  "Check if the text mentions or references supplementary materials, appendices, ",
  "supplementary information, additional materials, supporting information, ",
  "extra files, or similar content. ",
  "Respond with 'yes' if such references are found, 'no' if not found."
)

tools_supplementary <- list(
  list(
    type = "function",
    "function" = list(
      name = "supplementary_check",
      description = supplementary_check_description,
      parameters = list(
        type = "object",
        strict = TRUE,
        properties = list(
          supplementary = list(
            type = "string",
            enum = list("yes", "no"),
            description = "Whether the text contains references to supplementary materials or appendices"
          )
        ),
        required = list("supplementary")
      )
    )
  )
)

inclusion_decision_description <- paste0(
  "If the study should be included for further review, write '1'.",
  "If the study should be excluded, write '0'.",
  "If there is not enough information to make a clear decision, write '1.1'."
)

tools_simple <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        strict = TRUE,
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "The inclusion decision: '1' for include, '0' for exclude, '1.1' for unclear."
          )
        ),
        required = list("decision_gpt")
      )
    )
  )
)

detailed_description_description <- paste0(
  "If the study should be included for further reviewing, give a detailed description of your inclusion decision. ",
  "If the study should be excluded from the review, give a detailed description of your exclusion decision. ",
  "If there is not enough information to make a clear decision, give a detailed description of why you cannot reach a decision. ",
  "If there is no information in the text, write 'No information'"
)

tools_detailed <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        strict  = TRUE,
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "The inclusion decision: '1' for include, '0' for exclude, '1.1' for unclear."
          ),
          detailed_description = list(
            type = "string",
            description = detailed_description_description
          )
        ),
        required = list("decision_gpt", "detailed_description")
      )
    )
  )
)