#' Read an RIS file into a data frame
#'
#' Parses an RIS file into a data.frame, preserving the order of tags as they
#' first appear in the file. Repeated tags within a record are collapsed into a
#' single semicolon-separated string.
#'
#' @param file_path character. Path to the RIS file to read.
#'
#' @return A data.frame with one row per record and one column per encountered
#'   RIS tag (e.g., TY, AU, TI). Columns are ordered by first appearance of the
#'   tag in the file. Repeated tag values are collapsed with "; ".
#'
#' @examples
#' \dontrun{
#' df <- read_ris_to_dataframe("data-raw/raw data/apa_psycinfo_test_data.ris")
#' }
#'
#' @export
read_ris_to_dataframe <- function(file_path) {
  lines <- readLines(file_path, encoding = "UTF-8")
  
  records <- list()
  current_record <- list()
  field_order <- character(0)
  last_field <- NULL
  
  for (raw_line in lines) {
    # Preserve leading spaces (needed to detect continuation lines), remove trailing whitespace/newlines
    line <- sub("[\r\n]+$", "", raw_line)
    line <- sub("\\s+$", "", line)
    
    # Skip empty lines
    if (line == "") next
    
    # Check if line starts a new record
    if (startsWith(line, "TY  - ")) {
      # If we have a current record, save it
      if (length(current_record) > 0) {
        records <- append(records, list(current_record), after = length(records))
      }
      # Start new record
      current_record <- list()
      val <- sub("^TY  - ", "", line)
      val <- gsub("\\s+", " ", trimws(val))
      current_record$TY <- val
      last_field <- "TY"
      # Add TY to field order if not already present
      if (!"TY" %in% field_order) {
        field_order <- c(field_order, "TY")
      }
    }
    # Check for end of record
    else if (grepl("^ER  -\\s*$", line)) {
      # Save current record
      if (length(current_record) > 0) {
        records <- append(records, list(current_record), after = length(records))
      }
      current_record <- list()
      last_field <- NULL
    }
    # Process regular field lines like "AB  - value"
    else if (grepl("^[A-Z0-9]{2}  - ", line)) {
      field <- substr(line, 1, 2)
      value <- sub("^[A-Z0-9]{2}  - ", "", line)
      value <- gsub("\\s+", " ", trimws(value))
      
      # Add field to order if not already present
      if (!field %in% field_order) {
        field_order <- c(field_order, field)
      }
      
      # Handle multiple values for same field
      if (field %in% names(current_record)) {
        if (is.list(current_record[[field]])) {
          current_record[[field]] <- append(current_record[[field]], value)
        } else {
          current_record[[field]] <- list(current_record[[field]], value)
        }
      } else {
        current_record[[field]] <- value
      }
      last_field <- field
    }
    # Continuation lines start with two spaces in RIS exports; append to last field value
    else if (grepl("^\\s{2}", raw_line)) {
      # Remove the two leading spaces and trailing whitespace
      cont <- sub("^\\s{2}", "", raw_line)
      cont <- sub("\\s+$", "", cont)
      cont <- gsub("\\s+", " ", trimws(cont))
      if (!is.null(last_field) && last_field %in% names(current_record)) {
        curval <- current_record[[last_field]]
        if (is.list(curval)) {
          # Append to the last element of the list, normalizing spaces
          n <- length(curval)
          curval[[n]] <- paste(gsub("\\s+", " ", trimws(curval[[n]])), cont, sep = " ")
          curval[[n]] <- gsub("\\s+", " ", trimws(curval[[n]]))
          current_record[[last_field]] <- curval
        } else {
          newval <- paste(gsub("\\s+", " ", trimws(curval)), cont, sep = " ")
          current_record[[last_field]] <- gsub("\\s+", " ", trimws(newval))
        }
      }
    }
    # Treat unknown non-empty lines as continuation of last field
    else {
      if (!is.null(last_field) && last_field %in% names(current_record)) {
        cont <- gsub("\\s+", " ", trimws(line))
        curval <- current_record[[last_field]]
        if (is.list(curval)) {
          n <- length(curval)
          curval[[n]] <- paste(gsub("\\s+", " ", trimws(curval[[n]])), cont, sep = " ")
          curval[[n]] <- gsub("\\s+", " ", trimws(curval[[n]]))
          current_record[[last_field]] <- curval
        } else {
          newval <- paste(gsub("\\s+", " ", trimws(curval)), cont, sep = " ")
          current_record[[last_field]] <- gsub("\\s+", " ", trimws(newval))
        }
      }
    }
  }
  
  # If file didn't end with ER, save the last record
  if (length(current_record) > 0) {
    records <- append(records, list(current_record), after = length(records))
  }
  
  # Convert to dataframe
  if (length(records) == 0) {
    return(data.frame())
  }
  
  # Create dataframe using the discovered field order
  df_list <- list()
  for (field in field_order) {
    df_list[[field]] <- character(length(records))
  }
  
  # Fill dataframe
  for (i in seq_along(records)) {
    record <- records[[i]]
    for (field in names(record)) {
      value <- record[[field]]
      # If multiple values, collapse with semicolon
      if (is.list(value)) {
        vals <- gsub("\\s+", " ", trimws(unlist(value)))
        df_list[[field]][i] <- paste(vals, collapse = "; ")
      } else {
        df_list[[field]][i] <- gsub("\\s+", " ", trimws(value))
      }
    }
  }
  
  # Convert to dataframe with fields in the order they appeared in the file
  df <- data.frame(df_list, stringsAsFactors = FALSE)
  
  return(df)
}

#' Write a data frame to a RIS file
#'
#' Writes a data.frame to a RIS file, one record per row. If a field value
#' contains semicolons, it is split and written as multiple tag lines. The `TY`
#' field is written first for each record, followed by all other
#' fields. Records are terminated with `ER  - `.
#'
#' @param df data.frame. The data to write.
#' @param file_path character. Path to the output RIS file.
#'
#' @return A character string indicating the file path where the RIS file was saved.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(TY = "JOUR", TI = "An example", AU = "Doe, J.; Roe, R.", stringsAsFactors = FALSE)
#' save_dataframe_to_ris(df, "path/to/output.ris")
#' }
#'
#' @export
save_dataframe_to_ris <- function(df, file_path) {
  # Open file connection
  con <- file(file_path, "w", encoding = "UTF-8")

  # Tags that should be written as multiple lines when semicolon-separated
  multi_value_tags <- c("AU", "A1", "KW")

  for (i in 1:nrow(df)) {
    row <- df[i, ]

    # Write TY field first
    if ("TY" %in% names(row) && !is.na(row$TY) && row$TY != "") {
      writeLines(paste0("TY  - ", as.character(row$TY)), con)
    }

    # Write each field
    for (col_name in names(row)) {
      # Skip TY as it's already written, and skip empty values
      if (col_name == "TY") next
      value <- row[[col_name]]

      # Coerce factors, lists etc. to character and collapse multiple elements
      if (is.factor(value)) value <- as.character(value)
      if (is.list(value) || length(value) > 1) {
        value <- paste(unlist(value), collapse = "; ")
      }
      value <- as.character(value)

      # Skip empty values
      if (is.na(value) || value == "" || is.null(value)) next

      # Normalize whitespace
      value <- gsub("\\s+", " ", trimws(value))

      # If this tag is a multi-value tag, split on semicolons and write multiple lines.
      # For free-text fields (e.g., AB) we do NOT split, so they remain a single tag line.
      if (grepl(";", value) && col_name %in% multi_value_tags) {
        values <- strsplit(value, ";\\s*")[[1]]
        for (val in values) {
          val <- trimws(val)
          if (val != "") {
            writeLines(paste0(col_name, "  - ", val), con)
          }
        }
      } else {
        writeLines(paste0(col_name, "  - ", value), con)
      }
    }

    # Write end of record
    writeLines("ER  - ", con)
    writeLines("", con)  # Empty line between records
  }

  # Close file connection
  close(con)

  cat("RIS file saved to:", file_path, "\n")
}