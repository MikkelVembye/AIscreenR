# --- File Processing Helper Functions ---

# Read protocol file content
.read_protocol_file <- function(protocol_file_path) {
  if (!file.exists(protocol_file_path)) stop(paste("Error: Protocol file does not exist:", protocol_file_path))
  
  protocol_ext <- tolower(tools::file_ext(protocol_file_path))
  supported_protocol_extensions <- c("txt", "md", "pdf", "docx")
  if (!protocol_ext %in% supported_protocol_extensions) stop(paste0("Error: Unsupported protocol file type '.", protocol_ext, "'. Supported extensions are: .", paste(supported_protocol_extensions, collapse = ", .")))
  
  tryCatch({
    protocol_content <- switch(protocol_ext,
      "txt" = ,
      "md" = paste(readLines(protocol_file_path, warn = FALSE), collapse = "\n"),
      "pdf" = {
        if (!requireNamespace("pdftools", quietly = TRUE)) stop("Package 'pdftools' required to read PDF protocols. Please install it.", call. = FALSE)
        paste(pdftools::pdf_text(protocol_file_path), collapse = "\n")
      },
      "docx" = {
        if (!requireNamespace("officer", quietly = TRUE)) stop("Package 'officer' required to read DOCX protocols. Please install it.", call. = FALSE)
        doc <- officer::read_docx(protocol_file_path)
        paste(officer::docx_summary(doc)$text[officer::docx_summary(doc)$content_type == "paragraph"], collapse = "\n")
      }
    )
    if (nchar(trimws(protocol_content)) == 0) stop("Protocol file appears to be empty or unreadable.")
    return(protocol_content)
  }, error = function(e) {
    stop(paste("Error reading protocol file:", e$message))
  })
}

# Combine files within a single directory
.combine_files_in_dir <- function(dir_path, temp_files_created) {
  files_in_dir <- list.files(dir_path, full.names = TRUE, recursive = FALSE)
  files_in_dir <- files_in_dir[!sapply(files_in_dir, dir.exists)]
  
  pdfs <- files_in_dir[tolower(tools::file_ext(files_in_dir)) == "pdf"]
  if (length(pdfs) > 1) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      warning("Package 'pdftools' is required to combine PDFs. Processing them individually.", call. = FALSE)
      return(list(files = files_in_dir, temp_files = temp_files_created))
    }
    combined_pdf_path <- tempfile(pattern = paste0("combined_", basename(dir_path)), fileext = ".pdf")
    tryCatch({
      pdftools::pdf_combine(pdfs, output = combined_pdf_path)
      other_files <- files_in_dir[!files_in_dir %in% pdfs]
      return(list(files = c(combined_pdf_path, other_files), temp_files = c(temp_files_created, combined_pdf_path)))
    }, error = function(e) {
      warning(paste("Failed to combine PDFs in", dir_path, ". Processing them individually. Error:", e$message))
      return(list(files = files_in_dir, temp_files = temp_files_created))
    })
  }
  list(files = files_in_dir, temp_files = temp_files_created)
}

# Process file paths and directories
.process_file_paths <- function(file_path) {
  processed_file_paths <- c()
  temp_files_created <- c()
  
  for (p in file_path) {
    if (dir.exists(p)) {
      contents <- list.files(p, full.names = TRUE, recursive = FALSE)
      files_in_p <- contents[!sapply(contents, dir.exists)]
      processed_file_paths <- c(processed_file_paths, files_in_p)
      
      sub_dirs <- contents[sapply(contents, dir.exists)]
      for (sub_dir in sub_dirs) {
        result <- .combine_files_in_dir(sub_dir, temp_files_created)
        processed_file_paths <- c(processed_file_paths, result$files)
        temp_files_created <- result$temp_files
      }
    } else {
      processed_file_paths <- c(processed_file_paths, p)
    }
  }
  list(file_paths = unique(processed_file_paths), temp_files = temp_files_created)
}

# Clean up temporary files
.cleanup_temp_files <- function(temp_files_created) {
  if (length(temp_files_created) > 0) unlink(temp_files_created, force = TRUE)
}

# --- Argument Validation ---
.validate_ftscreen_args <- function(args) {
  possible_models <- c("gpt-4o-mini", "gpt-4.1", "gpt-4.1-mini", "gpt-4.1-nano", "gpt-4o")
  if (any(!args$model %in% possible_models)) stop("Error: Invalid 'model' provided. Possible models are: ", paste(possible_models, collapse = ", "))
  if (any(args$model == "gpt-4o") && args$reps > 1 && !args$force) stop("Error: Using 'gpt-4o' with reps > 1 requires setting 'force = TRUE'.")
  if (args$reps > 10 && !args$force) stop("* Are you sure you want to use ", args$reps, " iterations? If so, set 'force = TRUE'")
  
  allowed_extensions <- c("c", "cpp", "cs", "css", "doc", "docx", "go", "html", "java", "js", "json", "md", "pdf", "php", "pptx", "py", "rb", "sh", "tex", "ts", "txt")
  for (f_path in args$file_path) {
    if (!file.exists(f_path)) stop(paste("Error: File does not exist:", f_path))
    ext <- tolower(tools::file_ext(f_path))
    if (!ext %in% allowed_extensions) stop(paste0("Error: Unsupported file type for '", basename(f_path), "'. Extension '.", ext, "' is not supported."))
  }
}

# --- Result Aggregation ---
.aggregate_ft_res <- function(data, incl_cutoff_u, incl_cutoff_l) {
  sum_dat <- data |>
    dplyr::summarise(
      incl_p = mean(decision_binary == 1, na.rm = TRUE),
      final_decision_gpt = dplyr::case_when(
        incl_p < incl_cutoff_u & incl_p >= incl_cutoff_l ~ "Check",
        incl_p >= incl_cutoff_u ~ "Include",
        incl_p < incl_cutoff_l ~ "Exclude",
        TRUE ~ NA_character_
      ),
      final_decision_gpt_num = dplyr::case_when(
        incl_p >= incl_cutoff_l ~ 1,
        incl_p < incl_cutoff_l ~ 0,
        TRUE ~ NA_real_
      ),
      reps = dplyr::n(),
      n_mis_answers = sum(is.na(decision_binary)),
      .by = c(title, model, promptid, prompt)
    )

  if ("supplementary" %in% names(data)) {
    supplementary_final <- data |>
      dplyr::summarise(supplementary = if (any(supplementary == "yes", na.rm = TRUE)) "yes" else "no", .by = c(title, model, promptid))
    sum_dat <- dplyr::left_join(sum_dat, supplementary_final, by = c("title", "model", "promptid"))
  }

  if ("detailed_description" %in% names(data)) {
    long_answer_dat_sum <- data |>
      dplyr::mutate(
        incl_p = mean(decision_binary == 1, na.rm = TRUE),
        final_decision_gpt_num = dplyr::case_when(incl_p >= incl_cutoff_l ~ 1, incl_p < incl_cutoff_l ~ 0, TRUE ~ NA_real_),
        n_words_answer = stringr::str_count(detailed_description, '\\w+'),
        .by = c(title, model, promptid)
      ) |>
      dplyr::filter(decision_binary == final_decision_gpt_num) |>
      dplyr::arrange(title, model, promptid, desc(n_words_answer)) |>
      dplyr::summarise(longest_answer = detailed_description[1], .by = c(title, model, promptid))
    sum_dat <- dplyr::left_join(sum_dat, long_answer_dat_sum, by = c("title", "model", "promptid"))
  }
  
  tibble::new_tibble(sum_dat, class = "gpt_agg_tbl")
}

# --- Prompt Creation ---
.create_protocol_prompt <- function(protocol_content) {
  paste0("You are conducting a systematic literature review using a comprehensive protocol. Please analyze this document following this structured approach:\n\nSTEP 1: First, use the supplementary_check function to determine if this text contains references to supplementary materials, appendices, or additional information.\n\nSTEP 2: PROTOCOL-BASED EVALUATION\n- Carefully review the complete protocol provided below\n- Systematically evaluate the study against EACH criterion in the protocol\n\nPROTOCOL FOR EVALUATION:\n", protocol_content, "\n\nSTEP 3: INCLUSION DECISION\n- Use the appropriate inclusion decision function based on your protocol evaluation.\n- A study should be INCLUDED if it meets ALL the inclusion criteria specified in the protocol.\n- A study should be EXCLUDED if it fails to meet any inclusion criteria OR meets any exclusion criteria.")
}
.create_direct_prompt <- function(prompt) {
  paste0("Please analyze this document systematically:\n\nSTEP 1: First, use the supplementary_check function to determine if this text contains references to supplementary materials, appendices, or additional information.\n\nSTEP 2: Then, carefully read and analyze the document content to answer this question: ", prompt, "\n\nSTEP 3: Use the appropriate inclusion decision function to provide your final determination.\n\nRemember: If the answer to the prompt is yes, the study should be included in further studies.")
}

# --- Result DataFrame Creation ---
.create_result_df <- function(result_list, decision_description, status_message) {
  data.frame(
    studyid = result_list$studyid,
    title = result_list$title,
    promptid = result_list$promptid,
    prompt = result_list$prompt,
    model = result_list$model,
    iterations = result_list$iterations,
    question = result_list$prompt,
    top_p = result_list$top_p,
    decision_gpt = result_list$decision_gpt %||% NA_character_,
    detailed_description = if(decision_description && !is.null(result_list$detailed_description)) result_list$detailed_description else NA_character_,
    supplementary = result_list$supplementary %||% NA_character_,
    decision_binary = result_list$decision_binary %||% NA_real_,
    run_date = as.character(result_list$run_date),
    status_message = status_message,
    run_time = result_list$run_time,
    prompt_tokens = result_list$prompt_tokens,
    completion_tokens = result_list$completion_tokens,
    stringsAsFactors = FALSE
  )
}