# AIscreenR 0.3.1.9000

* Updating data handling of report(), removing bug
* Correcting path in generating-disagreement-reports article
* Updating the handling of coding missing abstracts in the vignette now when using read_ris_to_dataframe()
* Updating the default inclusion threshold then conducting replicate screenings. 

# AIscreenR 0.3.1

* Updating documentation of tabscreen_gpt()


# AIscreenR 0.3.0

* Adding tabscreen_groq() function to screen titles and abstracts using Groq AI.
* Adding tabscreen_ollama() function to screen titles and abstracts using local ollama models.
* Adding functions to read and write RIS files: read_ris_to_dataframe() and save_dataframe_to_ris().
* Adding function to generate disagreement reports: generate_disagreement_report().
* Adding articles for fine-tuning OpenAI models, generating disagreement reports, generating fine-tuning data and reading/writing RIS files.
* Adding article for comparing performance of reasoning models (including gpt-5 models) with gpt-4o-mini.
* Making new refinements to the tabscreen_* functions. Making it possible to steer the model's (over)inclusion behavior via the overinclusive argument in tabscreen_* functions. 
* Updated prize data, including all up-to-date models

# AIscreenR 0.2.0

* Minor change in the setup of the vignette
* Updated prize data, including all up-to-date models
* Adding create_fine_tune_data() and write_fine_tune_data() to generate data for fine tuning OpenAI's models
* Adding Thomas Olsen as co-author

# AIscreenR 0.1.1

* A typo in the vignette has been corrected.
* The vignette now draws on functions from synthesisr instead of revtools to handle RIS files.
* tabscreen_gpt() now treats the study ID variable as a factor to keep original order of the dataset with titles and abstracts.

# AIscreenR 0.1.0

* This is the first release of AIscreenR.
