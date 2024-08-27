#'The following code shows an example of how to approach a screening using hierarchical prompting. This means using multiple prompts in a specific organized order, where all references are evaluated by the first prompt and the references included by the first prompt are then evaluated by the second prompt etc.
#'
#'The example below is based on already screened references for an ongoing review.
#'
#' ```{r}
#' # Loading the included references
#' #tf_incl <- revtools::read_bibliography("ExportedRis_included_ROB.ris") |>
#' #  suppressWarnings() |>
#' #  as_tibble() |>
#' #  select(author, eppi_id, title, abstract) |>
#' #  mutate(
#' #    human_code = 1
#' #  )
#'
#' # Loading the excluded references
#' #tf_excl <- revtools::read_bibliography("ExportedRis_100_excluded_refs.ris") |>
#' #  suppressWarnings() |>
#' #  as_tibble() |>
#' #  select(author, eppi_id, title, abstract) |> # Using only relevant variables
#' #  mutate(
#' #    human_code = 0
#' #  )
#'
#' # Combining the two datasets
#' #tf_dat <-
#' #  bind_rows(tf_excl, tf_incl) |>
#' #  mutate(studyid = 1:n()) |>
#' #  relocate(studyid, .before = title)
#'
#'
#' # Filtering the data so that studies having "meta-anaysis", "review", "meta", "Meta" or "systematic review in the title are excluded.
#' # And only studies that have an abstract are included in the data.
#' #tf_filtered <-
#' #  tf_dat |>
#' #  filter(!str_detect(title, "review|meta-analysis|meta|Meta|systematic review"), !is.na(abstract))
#' # References without an abstract can be saved in another dataset so they can be screened by human coders on title & abstract level or go directly to the 2nd stage screening for further review in full text format.
#'
#' ```
#' For this review, six prompts have been developed and these are saved as .docx files that need to be loaded and saved as a vector for the following screening.
#'
#' ```{r}
#' # Creating an empty list to store the prompts
#' #prompts <- list()
#'
#' # Looping over the prompt numbers
#' #for (i in 1:6) {
#'
#' # Construct the file name
#' #  prompt_file <- paste0("Prompt 12.", i, ".docx")
#'
#' # Read and process the file, then store the result in the list
#' #  prompts[[i]] <- readtext::readtext(prompt_file)$text |>
#' #    stringr::str_remove_all("\n")
#' #}
#'
#' # Converting the list to a vector
#' #prompts <- unlist(prompts)
#' ```
#'
#'
#' ```{r eval = FALSE}
#' # Example of a hierarchical screening using the six developed prompts for TF
#' #data <- tf_filtered
#' #var_names <- names(data)
#' #n_prompts <- length(prompts)
#' #i <- 1
#' #nrow_left <- 5
#'
#' #x <- list()
#' #y <- list()
#'
#' #while (i <= n_prompts & nrow_left > 0) {
#'
#' #  obj <- tabscreen_gpt(
#' #    data = data,
#' #    prompt = prompts[i],
#' #    studyid = studyid,
#' #    title = title,
#' #    abstract = abstract,
#' #functions = incl_function_custom,
#' #function_call_name = list(name = "inclusion_decision"),
#' #    model = "gpt-4",
#' #    reps = 1,
#' #    messages = FALSE
#' #  )
#'
#' #  y[[i]] <- obj
#' #  x[[i]] <- obj$answer_data_sum
#'
#' #  data <-
#' #    obj$answer_data_sum |>
#' #    filter(final_decision_gpt_num == 1) |>
#' #    select(all_of(var_names)) |>
#' #    as_tibble()
#'
#' #  final_answer_dat <-
#' #    obj$answer_data_sum |>
#' #    filter(final_decision_gpt_num == 1)
#'
#' #  i <- i + 1
#' #  nrow_left <- nrow(data)
#'
#' #}
#'
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $2.2379.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $0.7889.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $0.7007.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $0.6886.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $0.7925.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#' #> * Be aware that using gpt-4 models cost at least 10 times more than gpt-3.5 models.
#' #> * The approximate price of the current (simple) screening will be around $0.5516.
#' #> Progress: ──────────────────────────────────────────────────────────────────────────────────────────── 100%
#'
#' ```
#'
#' The results from the screening can be viewed in two ways:
#'
#'   ```{r}
#' #include <- x[[length(x)]] |> filter(final_decision_gpt_num == 1)
#' #include
#'
#' #final_answer_dat
#'
#' ## Optional
#' # Saving the dataset in a separate datafile
#' #saveRDS(final_answer_dat, "final_answer_dat.rds")
#'
#' # Loading the dataset
#' #final_dat <- readRDS("final_answer_dat.rds")
#'
#' ```
#'
#' If you use EPPI-reviewer as your software tool for your systematic review, then it can be useful to isolate the eppi-id's for the included references so they can be identified through EPPI-reviewer's 'Search & Classify' function, specifically searching for the references 'With these internal ID's'.
#'
#' ```{r}
#' # Converting the dataset to a wide-format and keeping only the eppi-id's for the included references to load them into EPPI
#' #selected_final_dat <- final_dat %>%
#' #  dplyr::select(eppi_id) %>%
#' #  t()
#'
#' #write.csv(selected_final_dat)
#' ```
#'
#' ```{r}
#' # Add codes
#' ```
#'
