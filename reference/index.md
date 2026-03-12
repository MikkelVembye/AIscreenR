# Package index

## Screen with GPT API models

R functions to automate title and abstract screeening with GPT API
models

- [`tabscreen_gpt.tools()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  [`tabscreen_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.tools.md)
  **\[stable\]** : Title and abstract screening with GPT API models
  using function calls via the tools argument
- [`tabscreen_gpt.original()`](https://mikkelvembye.github.io/AIscreenR/reference/tabscreen_gpt.original.md)
  **\[deprecated\]** : Title and abstract screening with GPT API models
  using function calls via the original function call arguments

## Screen failed requests

R functions to re-screen failied title and abstract requests

- [`screen_errors()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.md)
  **\[experimental\]** : Generic function to re-screen failed title and
  abstract requests.
- [`screen_errors(`*`<gpt>`*`)`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.gpt.md)
  **\[experimental\]** : Re-screen failed requests.
- [`screen_errors(`*`<chatgpt>`*`)`](https://mikkelvembye.github.io/AIscreenR/reference/screen_errors.chatgpt.md)
  **\[deprecated\]** : Re-screen failed requests.

## Analyze screening performance

R functions to analyze agreement between humans and GPT

- [`screen_analyzer()`](https://mikkelvembye.github.io/AIscreenR/reference/screen_analyzer.md)
  **\[stable\]** : Analyze performance between the human and AI
  screening.

## Fine tuning

R functions generate data that can be used for fine tuning of models

- [`create_fine_tune_data()`](https://mikkelvembye.github.io/AIscreenR/reference/create_fine_tune_data.md)
  : Function to generate dataset to be used for fine-tuning models
- [`save_fine_tune_data()`](https://mikkelvembye.github.io/AIscreenR/reference/save_fine_tune_data.md)
  : Function to write/save fine tune dataset in required jsonl format

## API management

R functions to ease and automate API management

- [`set_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/set_api_key.md)
  : Creating a temporary R environment API key variable
- [`get_api_key()`](https://mikkelvembye.github.io/AIscreenR/reference/get_api_key.md)
  : Get API key from R environment variable.

## Rate limits

R functions to find api rate limits

- [`rate_limits_per_minute()`](https://mikkelvembye.github.io/AIscreenR/reference/rate_limits_per_minute.md)
  **\[stable\]** : Find updated rate limits for API models

## Datasets

Datasets in the package

- [`filges2015_dat`](https://mikkelvembye.github.io/AIscreenR/reference/filges2015_dat.md)
  : RIS file data from Functional Family Therapy (FFT) systematic review
- [`model_prizes`](https://mikkelvembye.github.io/AIscreenR/reference/model_prizes.md)
  : Model prize data (last updated August 14, 2025)

## Coercion

- [`is_chatgpt()`](https://mikkelvembye.github.io/AIscreenR/reference/is_chatgpt.md)
  **\[deprecated\]** :

  Test if the object is a `'chatgpt'` object

- [`is_chatgpt_tbl()`](https://mikkelvembye.github.io/AIscreenR/reference/is_chatgpt_tbl.md)
  **\[deprecated\]** :

  Test if the object is a `'chatgpt_tbl'` object

- [`is_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/is_gpt.md)
  :

  Test if the object is a `'gpt'` object

- [`is_gpt_tbl()`](https://mikkelvembye.github.io/AIscreenR/reference/is_gpt_tbl.md)
  :

  Test if the object is a `'gpt_tbl'` object

- [`is_gpt_agg_tbl()`](https://mikkelvembye.github.io/AIscreenR/reference/is_gpt_agg_tbl.md)
  :

  Test if the object is a `'gpt_agg_tbl'` object

## Print methods

- [`print(`*`<gpt>`*`)`](https://mikkelvembye.github.io/AIscreenR/reference/print.gpt.md)
  :

  Print methods for `'gpt'` objects

- [`print(`*`<chatgpt>`*`)`](https://mikkelvembye.github.io/AIscreenR/reference/print.chatgpt.md)
  :

  Print methods for `'chatgpt'` objects

- [`print(`*`<gpt_price>`*`)`](https://mikkelvembye.github.io/AIscreenR/reference/print.gpt_price.md)
  :

  Print methods for `'gpt_price'` objects

## Pricing

- [`approximate_price_gpt()`](https://mikkelvembye.github.io/AIscreenR/reference/approximate_price_gpt.md)
  **\[experimental\]** : Approximate price estimation for title and
  abstract screening using OpenAI's GPT API models

## Sample references

Sample references to construct test dataset

- [`sample_references()`](https://mikkelvembye.github.io/AIscreenR/reference/sample_references.md)
  : Random sample references
