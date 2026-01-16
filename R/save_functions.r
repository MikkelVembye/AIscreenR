#' Read an RIS file into a data frame
#'
#' Parses an RIS file into a data.frame, preserving the order of tags as they
#' first appear in the file. Repeated tags within a record are collapsed into a
#' single semicolon-separated string.
#'
#' @param file_path character. Path to the RIS file to read.
#'
#' @return A data.frame with one row per record and one column per encountered
#'   RIS tag, using descriptive column names. Columns are ordered by first appearance of the
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
  norm_space <- function(x) gsub("\\s+", " ", trimws(x))

  preallocate_size <- max(1L, length(lines) %/% 5L + 1L) # Estimate number of records (avg 5 lines per record)
  records <- vector("list", preallocate_size) # List of records
  record_order <- vector("list", preallocate_size) # List of tag order per record
  rec_idx <- 0L 
  current_record <- list()
  current_order <- character(0)
  field_order <- character(0)
  last_field <- NULL
  after_end_record <- FALSE # Track if we just processed an ER tag

  # Parse the RIS file line by line
  # Each record typically starts with "TY  - " but can start with other tags like "DB  - "
  # Records end with "ER  - "
  for (raw_line in lines) {
    line <- sub("[\r\n]+$", "", raw_line)
    if (trimws(line) == "") next

    if (grepl("^ER  -\\s*$", line)) { # End of record
      if (length(current_record) > 0) {
        rec_idx <- rec_idx + 1L
        records[[rec_idx]] <- current_record
        record_order[[rec_idx]] <- current_order
      }
      current_record <- list()
      current_order <- character(0)
      last_field <- NULL
      after_end_record <- TRUE # Mark that we just saw an end of record
    } else if (grepl("^[A-Z0-9]{2}  - ", line)) { # If the line starts with a two-letter tag followed by "  - ", it's a field
      field <- substr(line, 1, 2)
      value <- norm_space(sub("^[A-Z0-9]{2}  - ", "", line))
      
      # Start a new record if: this is the first field ever, OR we just saw ER and this is the first field of next record
      if ((rec_idx == 0L && length(current_record) == 0) || after_end_record) {
        if (length(current_record) > 0) { # Save any pending record
          rec_idx <- rec_idx + 1L
          records[[rec_idx]] <- current_record
          record_order[[rec_idx]] <- current_order
        }
        current_record <- list()
        current_order <- character(0)
        after_end_record <- FALSE
      }
      
      if (!field %in% field_order) field_order <- c(field_order, field)
      current_order <- c(current_order, field) # Track order of fields in this record
      if (field %in% names(current_record)) { # If field already exists, make it a list to hold multiple values
        current_record[[field]] <- if (is.list(current_record[[field]])) { 
          append(current_record[[field]], value)
        } else {
          list(current_record[[field]], value)
        }
      } else { # Else, just add the field
        current_record[[field]] <- value
      }
      last_field <- field
      # Handle continuation lines (RIS values that span multiple lines)
    } else if (!is.null(last_field) && last_field %in% names(current_record)) {
      cont <- norm_space(if (grepl("^\\s{2}", line)) sub("^\\s{2}", "", line) else line)
      curval <- current_record[[last_field]]
      if (is.list(curval)) { # If multiple values, append to the last one
        n <- length(curval)
        curval[[n]] <- norm_space(paste(curval[[n]], cont))
        current_record[[last_field]] <- curval
      } else {
        current_record[[last_field]] <- norm_space(paste(curval, cont)) 
      }
    }
  }
  # Add the last record if file doesn't end with ER
  if (length(current_record) > 0) {
    rec_idx <- rec_idx + 1L
    records[[rec_idx]] <- current_record
    record_order[[rec_idx]] <- current_order
  }
  # Trim preallocated lists to actual size
  if (rec_idx == 0L) return(data.frame())
  records <- records[seq_len(rec_idx)]
  record_order <- record_order[seq_len(rec_idx)]

  # Build data frame
  df_list <- vector("list", length(field_order))
  names(df_list) <- field_order
  raw_values <- vector("list", length(field_order))
  names(raw_values) <- field_order
  
  # Initialize columns
  for (field in field_order) {
    df_list[[field]] <- character(rec_idx)
    raw_values[[field]] <- vector("list", rec_idx)
  }

  # Fill data frame
  for (i in seq_len(rec_idx)) {
    for (field in names(records[[i]])) {
      value <- records[[i]][[field]]
      if (is.list(value)) {
        vals <- norm_space(unlist(value))
        df_list[[field]][i] <- paste(vals, collapse = "; ")
        raw_values[[field]][[i]] <- vals
      } else {
        df_list[[field]][i] <- norm_space(value)
        raw_values[[field]][[i]] <- df_list[[field]][i]
      }
    }
  }

  df <- data.frame(df_list, stringsAsFactors = FALSE)
  attr(df, "ris_raw_values") <- raw_values # Store raw values
  attr(df, "ris_field_order") <- field_order # Store field order
  attr(df, "ris_record_order") <- record_order # Store record order
  .map_ris_tags(df)
}

#' Write a data frame to a RIS file
#'
#' Writes a data.frame to a RIS file, one record per row. If the data frame was created
#' by \code{read_ris_to_dataframe()}, the original RIS tag order and tags are preserved where possible.
#' Otherwise, a standard RIS format is used.
#'
#' If a field value contains semicolons, it is split and written as multiple tag lines. The \code{TY}
#' (source type) field is written first for each record, followed by all other fields. Records are
#' terminated with \code{ER  - }.
#'
#' @param df data.frame. The data to write.
#' @param file_path character. Path to the output RIS file.
#'
#' @return A character string indicating the file path where the RIS file was saved.
#'
#' @examples
#' \dontrun{
#' df <- read_ris_to_dataframe("data-raw/raw data/apa_psycinfo_test_data.ris")
#' save_dataframe_to_ris(df, "path/to/output.ris")
#' }
#'
#' @export
save_dataframe_to_ris <- function(df, file_path) {
  tag_meta <- attr(df, "ris_tag_used", exact = TRUE) # Metadata about which original tag was used for each column
  raw_meta <- attr(df, "ris_raw_values", exact = TRUE) # Raw values per tag
  record_order <- attr(df, "ris_record_order", exact = TRUE) # Original record order
  has_meta <- is.list(tag_meta) && length(tag_meta) > 0 # Check if metadata exists
  has_raw <- is.list(raw_meta) && length(raw_meta) > 0 # Check if raw values exist
  has_record_order <- is.list(record_order) && length(record_order) == nrow(df) # Check if record order exists
  reverse_tag_map <- .get_reverse_ris_tag_map() # Get reverse mapping of descriptive names to RIS tags
  norm_space <- function(x) gsub("\\s+", " ", trimws(x))

  # Function to resolve the correct RIS tag for a given column and row
  resolve_ris_tag <- function(col_name, row_idx) {
    if (has_meta && !is.null(tag_meta[[col_name]]) && # Check if metadata exists for this column
        length(tag_meta[[col_name]]) >= row_idx && 
        !is.na(tag_meta[[col_name]][[row_idx]]) && 
        nzchar(tag_meta[[col_name]][[row_idx]])) {
      return(tag_meta[[col_name]][[row_idx]]) # Use the original tag used for this column and row
    }
    if (grepl("^[A-Z0-9]{2,4}$", col_name)) return(col_name) # If column name is already a valid RIS tag, return it
    base_name <- sub("[0-9]+$", "", col_name)
    if (base_name %in% names(reverse_tag_map)) return(reverse_tag_map[[base_name]]) # If base name maps to a RIS tag, return it
    col_name
  }

  if (!has_meta) df <- .reverse_map_ris_tags(df) # Reverse map if no metadata

  # Attempt to preserve original formatting if possible
  if (has_raw && has_record_order) {
    raw_tags <- names(raw_meta) # Get all RIS tags from raw metadata
    raw_df_list <- vector("list", length(raw_tags))
    names(raw_df_list) <- raw_tags
    for (tag in raw_tags) raw_df_list[[tag]] <- character(nrow(df))

    # Fill raw_df_list with normalized raw values
    for (i in seq_len(nrow(df))) {
      for (tag in raw_tags) {
        vals <- unlist(raw_meta[[tag]][[i]])
        vals <- vals[!is.na(vals) & vals != ""]
        if (length(vals) > 0) {
          raw_df_list[[tag]][i] <- norm_space(paste(norm_space(vals), collapse = "; ")) # Collapse multiple values with "; "
        }
      }
    }

    # Reconstruct data frame from raw values and compare with original
    mapped_raw_df <- .map_ris_tags(data.frame(raw_df_list, stringsAsFactors = FALSE)) 
    common_cols <- intersect(names(mapped_raw_df), names(df)) # Find common columns between mapped raw and original df
    identical_on_common <- all(sapply(common_cols, function(col) { # Check if values are identical for common columns
      # Compare normalized values for each common column
      a <- as.character(mapped_raw_df[[col]])
      b <- as.character(df[[col]])
      a[is.na(a)] <- ""
      b[is.na(b)] <- ""
      identical(norm_space(a), norm_space(b))
    }))

    if (identical_on_common) { # If identical, write using original raw formatting
      con <- file(file_path, "w", encoding = "UTF-8")
      on.exit(close(con), add = TRUE)

      for (i in seq_len(nrow(df))) {
        tag_pos <- list()
        for (tag in record_order[[i]]) {
          if (tag == "ER" || is.null(raw_meta[[tag]])) next
          vals <- unlist(raw_meta[[tag]][[i]])
          vals <- vals[!is.na(vals) & vals != ""]
          if (length(vals) == 0) next
          pos <- if (is.null(tag_pos[[tag]])) 1L else tag_pos[[tag]]
          if (pos <= length(vals)) {
            writeLines(paste0(tag, "  - ", norm_space(vals[[pos]])), con)
            tag_pos[[tag]] <- pos + 1L
          }
        }
        writeLines("ER  - ", con)
        writeLines("", con)
      }
      cat("RIS file saved to:", file_path, "\n")
      return(invisible(file_path))
    }
  }
  # Fallback: write using normalized values from df
  con <- file(file_path, "w", encoding = "UTF-8")
  multi_value_tags <- c("AU","A1","KW","M1","T1","TI","SN","JF","JO","JA","Y1","PY",
                        "ID","AN","UR","DO","DP","DB","AD","PB","VL","IS","SP","EP","U1","U2","CN")
  free_text_tags <- c("AB","N1","NT","N2","NO","U1")
  ty_col <- if ("TY" %in% names(df)) "TY" else if ("source_type" %in% names(df)) "source_type" else NULL

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]

    if (!is.null(ty_col)) {
      ty_val <- as.character(row[[ty_col]])
      if (!is.na(ty_val) && ty_val != "") {
        writeLines(paste0(resolve_ris_tag(ty_col, i), "  - ", norm_space(ty_val)), con)
      }
    }

    for (col_name in names(row)) {
      if (!is.null(ty_col) && col_name == ty_col) next
      value <- row[[col_name]]
      if (is.factor(value) || is.list(value) || length(value) > 1) {
        value <- paste(unlist(value), collapse = "; ")
      }
      value <- as.character(value)
      if (is.na(value) || value == "") next
      value <- norm_space(value)
      tag_to_write <- resolve_ris_tag(col_name, i)

      # Attempt to write from raw values if they match
      wrote_from_raw <- FALSE
      if (has_raw && !is.null(raw_meta[[tag_to_write]])) {
        raw_vals <- unlist(raw_meta[[tag_to_write]][[i]])
        raw_vals <- raw_vals[!is.na(raw_vals) & raw_vals != ""]
        if (length(raw_vals) > 0 && identical(norm_space(value), norm_space(paste(norm_space(raw_vals), collapse = "; ")))) {
          for (rv in norm_space(raw_vals)) {
            if (rv != "") writeLines(paste0(tag_to_write, "  - ", rv), con)
          }
          wrote_from_raw <- TRUE
        }
      }

      # Fallback: write normalized value
      if (!wrote_from_raw) {
        if (grepl(";", value) && tag_to_write %in% multi_value_tags && !(tag_to_write %in% free_text_tags)) {
          for (val in strsplit(value, ";\\s*")[[1]]) {
            val <- trimws(val)
            if (val != "") writeLines(paste0(tag_to_write, "  - ", val), con)
          }
        } else {
          writeLines(paste0(tag_to_write, "  - ", value), con)
        }
      }
    }
    writeLines("ER  - ", con)
    writeLines("", con)
  }
  close(con)
  cat("RIS file saved to:", file_path, "\n")
}


# Helper function to map RIS tags to descriptive names
.map_ris_tags <- function(df) {
  # Preserve read-time metadata.
  raw_values <- attr(df, "ris_raw_values", exact = TRUE)
  field_order <- attr(df, "ris_field_order", exact = TRUE)
  record_order <- attr(df, "ris_record_order", exact = TRUE)

  tag_map <- c(
    DB = "database",
    FN = "file_name",
    N = "file_name",
    DA = "date_generated",
    DT = "document_type",
    M3 = "document_type",
    TY = "source_type",
    LT = "publication_type",
    PT = "publication_type",
    LA = "language",
    PST = "status",
    SA = "street_address",
    STAT = "status",
    A1 = "author",
    A2 = "author",
    A3 = "author",
    A4 = "author",
    A5 = "author",
    AU = "author",
    AF = "author_full",
    FAU = "author_full",
    BA = "author_book",
    BF = "author_book_full",
    CA = "author_group",
    GP = "author_group",
    CN = "author_corporate",
    CNx = "author_corporate",
    Z2 = "author_otherlang",
    IR = "investigator",
    IV = "investigator",
    FIR = "investigator_full",
    A2x = "editor",
    BE = "editor",
    ED = "editor",
    EDx = "editor",
    FED = "editor_full",
    EM = "email",
    OI = "orcid_id",
    AD = "address",
    ADx = "address",
    C1 = "address",
    M1 = "address",
    CY = "address",
    IRAD = "address",
    RP = "reprint_address",
    PS = "personal_name_as_subject",
    FPS = "personal_name_as_subject_full",
    PSx = "personal_name_as_subject",
    RI = "researcher_id",
    AUID = "author_id",
    PY = "year",
    Y1 = "year",
    EY = "year_early_access",
    CRDT = "date_created",
    RC = "date_created",
    LR = "date_revised",
    DCOM = "date_completed",
    EDAT = "date_added",
    EA = "date_early_access",
    DEP = "date_published_elec",
    DP = "database_provider",
    PD = "date_published",
    PHST = "publication_history_status",
    T1 = "title",
    TI = "title",
    BTI = "title_book",
    FT = "title_foreign",
    Z1 = "title_otherlang",
    TT = "title_transliterated",
    JA = "journal_ja",
    JF = "journal_jf",
    JO = "journal_jo",
    JT = "journal_jt",
    T3 = "journal_t3",
    TA = "journal_abbreviated",
    SO = "source",
    T2 = "source",
    J2 = "source_abbreviated",
    J9 = "source_abbreviated",
    JI = "source_abbreviated",
    Z3 = "source_otherlang",
    SI = "secondary_source_id",
    C3 = "custom3",
    VTI = "volume_title",
    SE = "book_series_title",
    BS = "book_series_subtitle",
    ED = "edition",
    EN = "edition",
    CTI = "collection_title",
    VI = "volume",
    VL = "volume",
    IP = "issue",
    IS = "issue",
    SI = "special_issue",
    AR = "article_number",
    C7 = "custom7",
    SU = "supplement",
    PG = "pages",
    BP = "start_page",
    SP = "start_page",
    EP = "end_page",
    PG = "n_pages",
    PS = "n_pages",
    P2 = "n_chapters",
    AB = "abstract",
    N2 = "abstract",
    A2x = "abstract_other",
    OAB = "abstract_other",
    Z4 = "abstract_otherlang",
    DE = "keywords",
    KW = "keywords",
    MI = "keywords",
    ID = "record_id",
    MH = "mesh_terms",
    MHDA = "mesh_date",
    VR = "version",
    DI = "doi",
    DO = "doi",
    L3 = "doi",
    D2 = "doi_book",
    BN = "isbn",
    ISBN = "isbn",
    ISx = "issn",
    SN = "issn",
    EI = "eissn",
    CN = "call_number",
    AID = "article_id",
    ID = "article_id",
    MID = "manuscript_id",
    C2 = "pubmed_id",
    PM = "pubmed_id",
    PMID = "pubmed_id",
    PMC = "pubmed_central_identitfier",
    PMCR = "pubmed_central_release",
    JC = "nlm_id",
    JID = "nlm_id",
    OID = "other_id",
    RN = "registry_number",
    UT = "accession_number",
    AN = "accession_nr",
    GA = "document_delivery_id",
    UR = "url",
    AW = "url",
    SF = "space_flight_mission",
    SFM = "space_flight_mission",
    PE = "published_elec",
    CT = "conference_name",
    HO = "conference_host",
    CL = "conference_location",
    CYx = "conference_location",
    CY = "conference_date",
    Y2 = "conference_date",
    A4x = "conference_sponsor",
    SPx = "conference_sponsor",
    MA = "meeting_abstract",
    PB = "publisher",
    PU = "publisher",
    PA = "place_published",
    PI = "place_published",
    PL = "place_published",
    PP = "place_published",
    AE = "patent_assignee",
    PN = "patent_number",
    CI = "copyright_info",
    COI = "conflict_of_interest",
    OCI = "copyright_info_other",
    GN = "gene_name",
    GS = "gene_symbol",
    N1 = "notes1",
    OA = "open_access",
    PUBM = "publishing_model",
    OWN = "owner",
    OB = "record_owner",
    DP = "data_provider",
    FU = "funding_agency",
    FX = "funding_text",
    GR = "grant_number",
    GI = "grant_information",
    LID = "location_id",
    OT = "term_other",
    OTO = "term_owner_other",
    SB = "subset",
    NM = "substance_name",
    WC = "wos_categories",
    SC = "research_areas",
    CH = "chemicals",
    DS = "diseases",
    PR = "parts",
    OR = "systematics",
    MQ = "methods",
    ME = "medium",
    GE = "geographic_data",
    TM = "geologic_data",
    SD = "sequence_data",
    TAx = "taxonomic_data",
    ST = "supertaxa",
    TN = "taxa_notes",
    BD = "concepts",
    CC = "concepts",
    MC = "concepts",
    TC = "n_cited",
    Z9 = "n_cited_allwos",
    Z8 = "n_cited_csc",
    ZB = "n_cited_biosis",
    HC = "esi_highly_cited",
    HP = "esi_hot_paper",
    U2 = "custom2",
    NR = "references_n",
    RF = "references_n",
    CR = "cited_references",
    SS = "citation_subset",
    NT = "notes",
    NO = "comments",
    ER = "end_record",
    EF = "end_file"
  )

  mapped_names <- sapply(seq_along(names(df)), function(i) {
    original_name <- names(df)[i]
    if (original_name == "U1") {
      non_empty <- df[[i]][!is.na(df[[i]]) & df[[i]] != ""]
      # If all values in the U1 column are numeric, map to eppi_id, else custom_info
      return(if (length(non_empty) > 0 && all(grepl("^[0-9]+$", non_empty))) "eppi_id" else "custom_info")
    }
    if (original_name %in% names(tag_map)) tag_map[[original_name]] else original_name
  })
  
  result_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  tag_used <- list() # To track which original tag was used for each column
  
  # Process each unique mapped name
  for (base_name in unique(mapped_names)) {
    matching_indices <- which(mapped_names == base_name)
    
    # If only one matching column, copy it directly
    if (length(matching_indices) == 1) {
      result_df[[base_name]] <- df[[matching_indices]]
      tag_used[[base_name]] <- rep(names(df)[matching_indices], nrow(df))
      # If multiple matching columns, handle repeated fields
    } else {
      result_df[[base_name]] <- character(nrow(df))
      used_col_per_row <- integer(nrow(df))
      base_tag_per_row <- character(nrow(df))
      
      for (row_idx in seq_len(nrow(df))) {
        for (col_idx in matching_indices) {
          val <- df[row_idx, col_idx]
          if (!is.na(val) && val != "") {
            result_df[row_idx, base_name] <- val
            used_col_per_row[row_idx] <- col_idx
            base_tag_per_row[row_idx] <- names(df)[col_idx]
            break
          }
        }
      }
      tag_used[[base_name]] <- base_tag_per_row # Track which column was used for base name
      
      counter <- 2 # Start counter for repeated fields
      # Handle additional columns for repeated fields
      for (col_idx in matching_indices) {
        col_name <- paste0(base_name, counter)
        result_df[[col_name]] <- character(nrow(df))
        col_tag_vec <- character(nrow(df))
        added_any <- FALSE
        
        # Fill in values for this repeated field
        for (row_idx in seq_len(nrow(df))) {
          if (used_col_per_row[row_idx] == col_idx) next
          val <- df[row_idx, col_idx]
          base_val <- result_df[row_idx, base_name]
          if (!is.na(val) && val != "" && base_val != "" && val != base_val) {
            result_df[row_idx, col_name] <- val
            col_tag_vec[row_idx] <- names(df)[col_idx]
            added_any <- TRUE
          }
        }
        
        # If any values were added, keep this column; else remove it
        if (added_any) {
          tag_used[[col_name]] <- col_tag_vec
          counter <- counter + 1
        } else {
          result_df[[col_name]] <- NULL
        }
      }
    }
  }

  attr(result_df, "ris_tag_used") <- tag_used
  if (is.list(raw_values)) attr(result_df, "ris_raw_values") <- raw_values
  if (!is.null(field_order)) attr(result_df, "ris_field_order") <- field_order
  if (is.list(record_order)) attr(result_df, "ris_record_order") <- record_order
  result_df
}

# Helper function: reverse mapping from descriptive names to RIS tags
.get_reverse_ris_tag_map <- function() {
  # Using the most common/standard tag for each descriptive name
  c(
    database = "DB",
    file_name = "FN",
    date_generated = "DA",
    document_type = "M3",
    source_type = "TY",
    publication_type = "PT",
    language = "LA",
    status = "PST",
    street_address = "SA",
    author = "AU",
    author_full = "AF",
    author_book = "BA",
    author_book_full = "BF",
    author_group = "CA",
    author_corporate = "CN",
    author_otherlang = "Z2",
    investigator = "IR",
    investigator_full = "FIR",
    editor = "ED",
    editor_full = "FED",
    email = "EM",
    orcid_id = "OI",
    address = "AD",
    reprint_address = "RP",
    personal_name_as_subject = "PS",
    personal_name_as_subject_full = "FPS",
    researcher_id = "RI",
    author_id = "AUID",
    year = "PY",
    year_early_access = "EY",
    date_created = "CRDT",
    date_revised = "LR",
    date_completed = "DCOM",
    date_added = "EDAT",
    date_early_access = "EA",
    date_published_elec = "DEP",
    database_provider = "DP",
    date_published = "PD",
    publication_history_status = "PHST",
    title = "TI",
    title_book = "BTI",
    title_foreign = "FT",
    title_otherlang = "Z1",
    title_transliterated = "TT",
    journal = "JO",
    journal_ja = "JA",
    journal_jf = "JF",
    journal_jo = "JO",
    journal_jt = "JT",
    journal_t3 = "T3",
    journal_abbreviated = "TA",
    source = "T2",
    source_abbreviated = "J2",
    source_otherlang = "Z3",
    secondary_source_id = "SI",
    custom3 = "C3",
    volume_title = "VTI",
    book_series_title = "SE",
    book_series_subtitle = "BS",
    edition = "EN",
    collection_title = "CTI",
    volume = "VL",
    issue = "IS",
    special_issue = "SI",
    article_number = "AR",
    custom7 = "C7",
    supplement = "SU",
    pages = "PG",
    start_page = "SP",
    end_page = "EP",
    n_pages = "PG",
    n_chapters = "P2",
    abstract = "AB",
    abstract_other = "OAB",
    abstract_otherlang = "Z4",
    keywords = "KW",
    record_id = "ID",
    mesh_terms = "MH",
    mesh_date = "MHDA",
    version = "VR",
    doi = "DO",
    doi_book = "D2",
    isbn = "ISBN",
    issn = "SN",
    eissn = "EI",
    call_number = "CN",
    article_id = "AID",
    manuscript_id = "MID",
    pubmed_id = "PMID",
    pubmed_central_identitfier = "PMC",
    pubmed_central_release = "PMCR",
    nlm_id = "JID",
    other_id = "OID",
    registry_number = "RN",
    accession_number = "UT",
    accession_nr = "AN",
    document_delivery_id = "GA",
    url = "UR",
    space_flight_mission = "SFM",
    published_elec = "PE",
    conference_name = "CT",
    conference_host = "HO",
    conference_location = "CL",
    conference_date = "Y2",
    conference_sponsor = "SP",
    meeting_abstract = "MA",
    publisher = "PB",
    place_published = "PP",
    patent_assignee = "AE",
    patent_number = "PN",
    copyright_info = "CI",
    conflict_of_interest = "COI",
    copyright_info_other = "OCI",
    gene_name = "GN",
    gene_symbol = "GS",
    notes1 = "N1",
    open_access = "OA",
    publishing_model = "PUBM",
    owner = "OWN",
    record_owner = "OB",
    data_provider = "DP",
    funding_agency = "FU",
    funding_text = "FX",
    grant_number = "GR",
    grant_information = "GI",
    location_id = "LID",
    term_other = "OT",
    term_owner_other = "OTO",
    subset = "SB",
    substance_name = "NM",
    wos_categories = "WC",
    research_areas = "SC",
    chemicals = "CH",
    diseases = "DS",
    parts = "PR",
    systematics = "OR",
    methods = "MQ",
    medium = "ME",
    geographic_data = "GE",
    geologic_data = "TM",
    sequence_data = "SD",
    taxonomic_data = "TA",
    supertaxa = "ST",
    taxa_notes = "TN",
    concepts = "CC",
    n_cited = "TC",
    n_cited_allwos = "Z9",
    n_cited_csc = "Z8",
    n_cited_biosis = "ZB",
    esi_highly_cited = "HC",
    esi_hot_paper = "HP",
    custom2 = "U2",
    references_n = "NR",
    cited_references = "CR",
    citation_subset = "SS",
    notes = "NT",
    comments = "NO",
    end_record = "ER",
    end_file = "EF",
    eppi_id = "U1",
    custom_info = "U1"
  )
}

.reverse_map_ris_tags <- function(df) {
  reverse_tag_map <- .get_reverse_ris_tag_map()
  result_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  # Process each column in the input data frame and map back to RIS tags
  for (col_name in names(df)) {
    base_name <- sub("[0-9]+$", "", col_name)
    ris_tag <- if (base_name %in% names(reverse_tag_map)) {
      reverse_tag_map[[base_name]]
    } else if (col_name %in% names(reverse_tag_map)) {
      reverse_tag_map[[col_name]]
    } else {
      col_name
    }
    result_df[[ris_tag]] <- df[[col_name]]
  }
  result_df
}