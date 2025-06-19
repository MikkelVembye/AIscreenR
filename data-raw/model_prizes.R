# Models Prizes October 28 2024

mio <- 1000000

model_prizes <-
  data.frame(
    model = c(
      # GPT 3.5 models
      "gpt-3.5-turbo",
      "gpt-3.5-turbo-0125",
      "gpt-3.5-turbo-16k-0613",

      # GPT 4 models
      "gpt-4-32k",
      "gpt-4",
      "gpt-4-0613",
      "gpt-4-turbo-2024-04-09",
      "gpt-4-turbo",

      # GPT-4o models
      "gpt-4o",
      "gpt-4o-2024-08-06",
      "gpt-4o-2024-11-20",
      "gpt-4o-2024-05-13",

      # GPT-4o-mini models
      "gpt-4o-mini",
      "gpt-4o-mini-2024-07-18",

      # GPT-4.1 models
      "gpt-4.1",
      "gpt-4.1-2025-04-14",

      # GPT-4.1-mini models
      "gpt-4.1-mini",
      "gpt-4.1-mini-2025-04-14",

      # GPT-4.1-nano models
      "gpt-4.1-nano",
      "gpt-4.1-nano-2025-04-14",

      # GPT-4.5-preview models
      "gpt-4.5-preview",
      "gpt-4.5-preview-2025-02-27"
    ),

    price_in_per_token = c(
      # GPT 3.5 models
      0.5/mio, # gpt-3.5-turbo (refers currently to gpt-3.5-turbo-0125)
      0.5/mio, # gpt-3.5-turbo-0125
      3/mio,   # gpt-3.5-turbo-16k-0613

      # GPT 4 models
      60/mio,  # gpt-4-32k
      30/mio,  # gpt-4
      30/mio,  # gpt-4-0613
      10/mio,  # gpt-4-turbo-2024-04-09
      10/mio,  # gpt-4-turbo

      # GPT-4o models
      2.5/mio, # gpt-4o
      2.5/mio, # gpt-4o-2024-08-06
      2.5/mio, # gpt-4o-2024-11-20
      5/mio,   # gpt-4o-2024-05-13

      # GPT-4o-mini models
      0.15/mio, # gpt-4o-mini
      0.15/mio, # gpt-4o-mini-2024-07-18

      # GPT-4.1 models
      2/mio,   # gpt-4.1
      2/mio,   # gpt-4.1-2025-04-14

      # GPT-4.1-mini models
      0.4/mio, # gpt-4.1-mini
      0.4/mio, # gpt-4.1-mini-2025-04-14

      # GPT-4.1-nano models
      0.1/mio, # gpt-4.1-nano
      0.1/mio, # gpt-4.1-nano-2025-04-14

      # GPT-4.5-preview models
      75/mio,  # gpt-4.5-preview
      75/mio   # gpt-4.5-preview-2025-02-27
    ),

    price_out_per_token = c(
      # GPT 3.5 models
      1.5/mio, # gpt-3.5-turbo (refers currently to gpt-3.5-turbo-0125)
      1.5/mio, # gpt-3.5-turbo-0125
      4/mio,   # gpt-3.5-turbo-16k-0613

      # GPT 4 models
      120/mio, # gpt-4-32k
      60/mio,  # gpt-4
      60/mio,  # gpt-4-0613
      30/mio,  # gpt-4-turbo-2024-04-09
      30/mio,  # gpt-4-turbo

      # GPT-4o models
      10/mio,  # gpt-4o
      10/mio,  # gpt-4o-2024-08-06
      10/mio,  # gpt-4o-2024-11-20
      15/mio,  # gpt-4o-2024-05-13

      # GPT-4o-mini models
      0.6/mio, # gpt-4o-mini
      0.6/mio, # gpt-4o-mini-2024-07-18

      # GPT-4.1 models
      8/mio,   # gpt-4.1
      8/mio,   # gpt-4.1-2025-04-14

      # GPT-4.1-mini models
      1.6/mio, # gpt-4.1-mini
      1.6/mio, # gpt-4.1-mini-2025-04-14

      # GPT-4.1-nano models
      0.4/mio, # gpt-4.1-nano
      0.4/mio, # gpt-4.1-nano-2025-04-14

      # GPT-4.5-preview models
      150/mio, # gpt-4.5-preview
      150/mio  # gpt-4.5-preview-2025-02-27
    )
  )

use_data(model_prizes, overwrite = TRUE)
