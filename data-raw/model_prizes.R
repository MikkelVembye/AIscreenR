# Models Prizes Augusts 27 2024

mio <- 1000000

model_prizes <-
  data.frame(
    model = c(
      # GPT 3.5 models
      "gpt-3.5-turbo",
      "gpt-3.5-turbo-0301",
      "gpt-3.5-turbo-16k-0613",
      "gpt-3.5-turbo-0613",
      "gpt-3.5-turbo-1106",
      "gpt-3.5-turbo-0125",

      # GPT 4 models
      "gpt-4-32k",
      "gpt-4",
      "gpt-4-turbo-2024-04-09",
      "gpt-4-turbo",

      # GPT-4o models
      "gpt-4o",
      "gpt-4o-2024-08-06",
      "gpt-4o-2024-05-13",

      # GPT-4o-mini models
      "gpt-4o-mini",
      "gpt-4o-mini-2024-07-18"

    ),

    price_in_per_token = c(
      # GPT 3.5 models
      0.5/mio, # gpt-3.5-turbo (refers currently to gpt-3.5-turbo-0125)
      1.5/mio, # gpt-3.5-turbo-0301
      3/mio,   # gpt-3.5-turbo-16k-0613
      1.5/mio, # gpt-3.5-turbo-0613
      1/mio,   # gpt-3.5-turbo-1106
      0.5/mio, # gpt-3.5-turbo-0125

      # GPT 4 models
      60/mio,  # gpt-4-32k
      30/mio,  # gpt-4
      10/mio,  # gpt-4-turbo-2024-04-09
      10/mio,  # gpt-4-turbo

      # GPT-4o models
      5/mio,   # gpt-4o
      2.5/mio, # gpt-4o-2024-08-06
      5/mio,    # gpt-4o-2024-05-13

      # GPT-4o-mini models
      0.15/mio, # gpt-4o-mini
      0.15/mio  # gpt-4o-mini-2024-07-18

    ),

    price_out_per_token = c(
      # GPT 3.5 models
      1.5/mio, # gpt-3.5-turbo (refers currently to gpt-3.5-turbo-0125)
      2/mio,   # gpt-3.5-turbo-0301
      4/mio,   # gpt-3.5-turbo-16k-0613
      2/mio,   # gpt-3.5-turbo-0613
      2/mio,   # gpt-3.5-turbo-1106
      1.5/mio, # gpt-3.5-turbo-0125

      # GPT 4 models
      120/mio, # gpt-4-32k
      60/mio,  # gpt-4
      30/mio,  # gpt-4-turbo-2024-04-09
      30/mio,  # gpt-4-turbo

      # GPT-4o models
      15/mio,  # gpt-4o
      10/mio,  # gpt-4o-2024-08-06
      15/mio,  # gpt-4o-2024-05-13

      # GPT-4o-mini models
      0.6/mio, # gpt-4o-mini
      0.6/mio  # gpt-4o-mini-2024-07-18

    )

  )

use_data(model_prizes, overwrite = TRUE)
