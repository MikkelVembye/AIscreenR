#----------------------------------------------------------------
#
#  Functions
#
#----------------------------------------------------------------

#----------------------------------------------------------------
# Overinclusive GPT function calls
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
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
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
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          detailed_description = list(
            type = "string",
            description = detailed_description_description
          )
        ),
        required = list("decision_gpt", "detailed_description"),
        additionalProperties = FALSE
      )
    )
  )
)

#----------------------------------------------------------------
# Binary GPT function calls
#----------------------------------------------------------------

# Body functions to tools and tool_choice

inclusion_decision_description_binary <- paste0(
  "If the study should be included for further review, write '1'.",
  "If the study should be excluded, write '0'.",
  "When providing the response only provide the numerical decision."
)


tools_simple_binary <- list(
  # Function 1
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple_binary",
      description = inclusion_decision_description_binary,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          )
        ),
        required = list("decision_gpt"),
        additionalProperties = FALSE
      )
    )
  )
)


detailed_description_description_binary <- paste0(
  "If the study should be included for further reviewing, give a detailed description of your inclusion decision. ",
  "If the study should be excluded from the review, give a detailed description of your exclusion decision. "
)

# Combines both simple and detailed descriptions

tools_detailed_binary <- list(
  # Function 1
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_binary",
      description = inclusion_decision_description_binary,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          detailed_description = list(
            type = "string",
            description = detailed_description_description_binary
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
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
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
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          detailed_description = list(
            type = "string",
            description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
          )
        ),
        required = list("decision_gpt", "detailed_description"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_simple_binary_groq <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple_binary",
      description = inclusion_decision_description_binary,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          )
        ),
        required = list("decision_gpt"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_detailed_binary_groq <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_binary",
      description = inclusion_decision_description_binary,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          detailed_description = list(
            type = "string",
            description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
          )
        ),
        required = list("decision_gpt", "detailed_description"),
        additionalProperties = FALSE
      )
    )
  )
)

#----------------------------------------------------------------
# Gemini function calling
#----------------------------------------------------------------

tools_simple_gemini <- list(
  list(
    name = "inclusion_decision_simple",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        )
      ),
      required = list("decision_gpt")
    )
  )
)

tools_detailed_gemini <- list(
  list(
    name = "inclusion_decision",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
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

tools_simple_binary_gemini <- list(
  list(
    name = "inclusion_decision_simple_binary",
    description = inclusion_decision_description_binary,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        )
      ),
      required = list("decision_gpt")
    )
  )
)

tools_detailed_binary_gemini <- list(
  list(
    name = "inclusion_decision_binary",
    description = inclusion_decision_description_binary,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        detailed_description = list(
          type = "string",
          description = detailed_description_description_binary
        )
      ),
      required = list("decision_gpt", "detailed_description")
    )
  )
)

#----------------------------------------------------------------
# Anthropic-specific function calls (for Claude)
#----------------------------------------------------------------

# Anthropic uses input_schema format
tools_simple_claude <- list(
  list(
    name = "inclusion_decision_simple",
    description = inclusion_decision_description,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        )
      ),
      required = list("decision_gpt")
    )
  )
)

tools_detailed_claude <- list(
  list(
    name = "inclusion_decision",
    description = inclusion_decision_description,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
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

tools_simple_binary_claude <- list(
  list(
    name = "inclusion_decision_simple_binary",
    description = inclusion_decision_description_binary,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        )
      ),
      required = list("decision_gpt")
    )
  )
)

tools_detailed_binary_claude <- list(
  list(
    name = "inclusion_decision_binary",
    description = inclusion_decision_description_binary,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        detailed_description = list(
          type = "string",
          description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
        )
      ),
      required = list("decision_gpt", "detailed_description")
    )
  )
)


#----------------------------------------------------------------
# Function calls including a confidence score
#----------------------------------------------------------------

confidence_description <- paste0(
  "How confident are you in your decision? ",
  "Give a number from 0 (not confident at all) to 100 (completely confident)."
)

#----------------------------------------------------------------
# Overinclusive GPT function calls with confidence
#----------------------------------------------------------------

tools_simple_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple",
      description = inclusion_decision_description,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_detailed_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision",
      description = inclusion_decision_description,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          detailed_description = list(
            type = "string",
            description = detailed_description_description
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "detailed_description", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

#----------------------------------------------------------------
# Binary GPT function calls with confidence
#----------------------------------------------------------------

tools_simple_binary_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple_binary",
      description = inclusion_decision_description_binary,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_detailed_binary_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_binary",
      description = inclusion_decision_description_binary,
      strict = TRUE,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          detailed_description = list(
            type = "string",
            description = detailed_description_description_binary
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "detailed_description", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

#----------------------------------------------------------------
# GROQ function calling with confidence
#----------------------------------------------------------------

tools_simple_groq_conf <- list(
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
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_detailed_groq_conf <- list(
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
            description = "1=Include, 0=Exclude, 1.1=Uncertain",
            enum = list("1", "0", "1.1")
          ),
          detailed_description = list(
            type = "string",
            description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "detailed_description", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_simple_binary_groq_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_simple_binary",
      description = inclusion_decision_description_binary,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

tools_detailed_binary_groq_conf <- list(
  list(
    type = "function",
    "function" = list(
      name = "inclusion_decision_binary",
      description = inclusion_decision_description_binary,
      parameters = list(
        type = "object",
        properties = list(
          decision_gpt = list(
            type = "string",
            description = "1=Include, 0=Exclude",
            enum = list("1", "0")
          ),
          detailed_description = list(
            type = "string",
            description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
          ),
          confidence = list(
            type = "number",
            description = confidence_description
          )
        ),
        required = list("decision_gpt", "detailed_description", "confidence"),
        additionalProperties = FALSE
      )
    )
  )
)

#----------------------------------------------------------------
# Gemini function calling with confidence
#----------------------------------------------------------------

tools_simple_gemini_conf <- list(
  list(
    name = "inclusion_decision_simple",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "confidence")
    )
  )
)

tools_detailed_gemini_conf <- list(
  list(
    name = "inclusion_decision",
    description = inclusion_decision_description,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        ),
        detailed_description = list(
          type = "string",
          description = detailed_description_description
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "detailed_description", "confidence")
    )
  )
)

tools_simple_binary_gemini_conf <- list(
  list(
    name = "inclusion_decision_simple_binary",
    description = inclusion_decision_description_binary,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "confidence")
    )
  )
)

tools_detailed_binary_gemini_conf <- list(
  list(
    name = "inclusion_decision_binary",
    description = inclusion_decision_description_binary,
    parameters = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        detailed_description = list(
          type = "string",
          description = detailed_description_description_binary
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "detailed_description", "confidence")
    )
  )
)

#----------------------------------------------------------------
# Anthropic-specific function calls (for Claude) with confidence
#----------------------------------------------------------------

tools_simple_claude_conf <- list(
  list(
    name = "inclusion_decision_simple",
    description = inclusion_decision_description,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "confidence")
    )
  )
)

tools_detailed_claude_conf <- list(
  list(
    name = "inclusion_decision",
    description = inclusion_decision_description,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude, 1.1=Uncertain",
          enum = list("1", "0", "1.1")
        ),
        detailed_description = list(
          type = "string",
          description = detailed_description_description
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "detailed_description", "confidence")
    )
  )
)

tools_simple_binary_claude_conf <- list(
  list(
    name = "inclusion_decision_simple_binary",
    description = inclusion_decision_description_binary,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "confidence")
    )
  )
)

tools_detailed_binary_claude_conf <- list(
  list(
    name = "inclusion_decision_binary",
    description = inclusion_decision_description_binary,
    input_schema = list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "1=Include, 0=Exclude",
          enum = list("1", "0")
        ),
        detailed_description = list(
          type = "string",
          description = "List the detailed description of your inclusion decision. IMPORTANT: This must match the logic of your decision_gpt exactly."
        ),
        confidence = list(
          type = "number",
          description = confidence_description
        )
      ),
      required = list("decision_gpt", "detailed_description", "confidence")
    )
  )
)
