#----------------------------------------------------------------
#
#  Functions
#
#----------------------------------------------------------------

# Body functions to tools and tool_choice

inclusion_decision_description <- paste0(
  "If the study should be included for further review, write '1'.",
  "If the study should be excluded, write '0'.",
  "If there is not enough information to make a clear decision, write '1.1'.",
  "If there is no or only a little information in the title and abstract also write '1.1'",
  "When providing the response only provide the numerical decision."
)


tools_simple <- list(
  # Function 1
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            items = list(
              type = "integer",
              description = "An integer of either 1, 0, or 1.1"
            ),
            description = "List the inclusion decision"
          )
        ),
        required = list("decision_gpt"),
        additionalProperties = FALSE
      )
    )
  )
)


detailed_description_description <- paste0(
  "If the study should be included for further reviewing, give a detailed description of your inclusion decision. ",
  "If the study should be excluded from the review, give a detailed description of your exclusion decision. ",
  "If there is not enough information to make a clear decision, give a detailed description of why you can reach a decision. ",
  "If there is no information in the title and abstract, write 'No information'"
)

# Combines both simple and detailed descriptions

tools_detailed <- list(
  # Function 1
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            items = list(
              type = "integer",
              description = "An integer of either 1, 0, or 1.1"
            ),
            description = "List the inclusion decision"
          ),
          detailed_description = list(
            type = "string",
            items = list(
              type = "string",
              description = detailed_description_description
            ),
            description = "List the detailed description of your inclusion decision"
          )
        ),
        required = list("decision_gpt", "detailed_description"),
        additionalProperties = FALSE
      )
    )
  )
)


#----------------------------------------------------------------
# GROQ function calling
#----------------------------------------------------------------


tools_simple_groq <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "List the inclusion decision as '1', '0', or '1.1'",
            enum = list("1", "0", "1.1")
          )
        ),
        required = list("decision_gpt"),
        additionalProperties = FALSE
      )
    )
  )
)

# Combines both simple and detailed descriptions

tools_detailed_groq <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision",
      description = inclusion_decision_description,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "List the inclusion decision as '1', '0', or '1.1'",
            enum = list("1", "0", "1.1")
          ),
          detailed_description = list(
            type = "string",
            description = "List the detailed description of your inclusion decision"
          )
        ),
        required = list("decision_gpt", "detailed_description"),
        additionalProperties = FALSE
      )
    )
  )
)