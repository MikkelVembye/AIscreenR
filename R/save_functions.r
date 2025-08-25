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
  
  for (line in lines) {
    line <- trimws(line)
    
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
      current_record$TY <- sub("^TY  - ", "", line)
      # Add TY to field order if not already present
      if (!"TY" %in% field_order) {
        field_order <- c(field_order, "TY")
      }
    }
    # Check for end of record (allow optional trailing spaces after '-')
    else if (grepl("^ER\\s{2}-\\s*$", line)) {
      # Save current record
      if (length(current_record) > 0) {
        records <- append(records, list(current_record), after = length(records))
      }
      current_record <- list()
    }
    # Process other fields
    else if (grepl("^[A-Z0-9]{2}  - ", line)) {
      field <- substr(line, 1, 2)
      value <- sub("^[A-Z0-9]{2}  - ", "", line)
      
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
        df_list[[field]][i] <- paste(unlist(value), collapse = "; ")
      } else {
        df_list[[field]][i] <- value
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
  
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    
    # Write TY field first
    if ("TY" %in% names(row) && !is.na(row$TY) && row$TY != "") {
      writeLines(paste0("TY  - ", row$TY), con)
    }
    
    # Write each field
    for (col_name in names(row)) {
      # Skip TY as it's already written, and skip empty values
      if (col_name == "TY") next
      value <- row[[col_name]]
      
      # Skip empty values
      if (is.na(value) || value == "" || is.null(value)) next
      
      # Handle multiple values (split by semicolon) for any field
      if (grepl(";", value)) {
        values <- strsplit(value, "; ?")[[1]]
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