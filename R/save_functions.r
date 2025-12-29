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
  norm_space <- function(x) gsub("\\s+", " ", trimws(x)) # Normalize whitespace

  records <- vector("list", max(1L, length(lines) %/% 5L + 1L))  # Preallocate records list
  rec_idx <- 0L # Initialize record index

  current_record <- list() # Initialize current record
  field_order <- character(0) # Initialize field order
  last_field <- NULL # Initialize last field

  for (raw_line in lines) { # Loop through each line
    line_nl <- sub("[\r\n]+$", "", raw_line) # Remove newline characters
    if (trimws(line_nl) == "") next # Skip empty lines
    line_preserve <- line_nl # Preserve line for processing

    if (startsWith(line_preserve, "TY  - ")) { # Check for record type
      if (length(current_record) > 0) { # If current record exists
        rec_idx <- rec_idx + 1L # Increment record index
        records[[rec_idx]] <- current_record # Store current record
      }
      current_record <- list() # Reset current record
      val <- norm_space(sub("^TY  - ", "", line_preserve)) # Extract and normalize value
      current_record$TY <- val # Store type value
      last_field <- "TY" # Update last field
      if (!"TY" %in% field_order) field_order <- c(field_order, "TY") # Update field order
    } else if (grepl("^ER  -\\s*$", line_preserve)) { # Check for end of record
      if (length(current_record) > 0) { # If current record exists
        rec_idx <- rec_idx + 1L # Increment record index
        records[[rec_idx]] <- current_record # Store current record
      }
      current_record <- list() # Reset current record
      last_field <- NULL # Reset last field
    } else if (grepl("^[A-Z0-9]{2}  - ", line_preserve)) { # Check for field line
      field <- substr(line_preserve, 1, 2) # Extract field code
      value <- norm_space(sub("^[A-Z0-9]{2}  - ", "", line_preserve)) # Extract and normalize value
      if (!field %in% field_order) field_order <- c(field_order, field) # Update field order
      if (field %in% names(current_record)) { # If field already exists in current record
        if (is.list(current_record[[field]])) { # If field is a list
          current_record[[field]] <- append(current_record[[field]], value) # Append value
        } else {
          current_record[[field]] <- list(current_record[[field]], value) # Convert to list and append
        }
      } else {
        current_record[[field]] <- value # Store value in current record
      }
      last_field <- field # Update last field
    } else if (grepl("^\\s{2}", line_nl)) { # Check for continuation line
      cont <- norm_space(sub("^\\s{2}", "", line_nl)) # Extract continuation value
      if (!is.null(last_field) && last_field %in% names(current_record)) { # If last field exists
        curval <- current_record[[last_field]] # Get current value
        if (is.list(curval)) { # If current value is a list
          n <- length(curval) # Get length of list
          curval[[n]] <- norm_space(paste(curval[[n]], cont, sep = " ")) # Append continuation
          current_record[[last_field]] <- curval # Update current record
        } else {
          current_record[[last_field]] <- norm_space(paste(curval, cont, sep = " ")) # Append continuation
        }
      }
    } else { # Handle other lines
      if (!is.null(last_field) && last_field %in% names(current_record)) { # If last field exists
        cont <- norm_space(line_nl) # Normalize line
        curval <- current_record[[last_field]] # Get current value
        if (is.list(curval)) { # If current value is a list
          n <- length(curval) # Get length of list
          curval[[n]] <- norm_space(paste(curval[[n]], cont, sep = " ")) # Append continuation
          current_record[[last_field]] <- curval # Update current record
        } else {
          current_record[[last_field]] <- norm_space(paste(curval, cont, sep = " ")) # Append continuation
        }
      }
    }
  }

  if (length(current_record) > 0) { # If current record exists
    rec_idx <- rec_idx + 1L # Increment record index
    records[[rec_idx]] <- current_record # Store current record
  }

  if (rec_idx == 0L) return(data.frame()) # Return empty data frame if no records
  records <- records[seq_len(rec_idx)] # Trim records list

  df_list <- vector("list", length(field_order))
  names(df_list) <- field_order # Initialize data frame list
  for (field in field_order) df_list[[field]] <- character(rec_idx) # Preallocate character vectors

  for (i in seq_len(rec_idx)) { # Loop through records
    record <- records[[i]] # Get current record
    for (field in names(record)) { # Loop through fields in record
      value <- record[[field]] # Get field value
      if (is.list(value)) { # If value is a list
        vals <- norm_space(unlist(value)) # Unlist and normalize
        df_list[[field]][i] <- paste(vals, collapse = "; ") # Collapse values
      } else {
        df_list[[field]][i] <- norm_space(value) # Normalize value
      }
    }
  }

  df <- data.frame(df_list, stringsAsFactors = FALSE) # Create data frame
  df <- .map_ris_tags(df) # Map RIS tags
  df
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
#' df <- data.frame(TY = "JOUR", TI = "An example", AU = "Doe, J.; Roe, R.")
#' save_dataframe_to_ris(df, "path/to/output.ris")
#' }
#'
#' @export
save_dataframe_to_ris <- function(df, file_path) {
  df <- .reverse_map_ris_tags(df)
  con <- file(file_path, "w", encoding = "UTF-8")

  multi_value_tags <- c(
    "AU","A1","KW","M1","T1","TI","SN","JF","JO","JA","Y1","PY",
    "ID","AN","UR","DO","DP","DB","AD","PB","VL","IS","SP","EP",
    "U1","U2","CN"
  ) # Tags that can have multiple values
  free_text_tags <- c("AB","N1","NT","N2","NO") # Tags that may contain semicolons in free text

  for (i in seq_len(nrow(df))) { # Loop through each record
    row <- df[i, ] # Get current row

    if ("TY" %in% names(row) && !is.na(row$TY) && row$TY != "") { # Write TY first
      writeLines(paste0("TY  - ", as.character(row$TY)), con)
    }

    for (col_name in names(row)) { # Loop through each column
      if (col_name == "TY") next # Skip TY as it's already written
      value <- row[[col_name]] # Get column value

      if (is.factor(value)) value <- as.character(value) # Convert factor to character
      if (is.list(value) || length(value) > 1) { # Handle list or multiple values
        value <- paste(unlist(value), collapse = "; ") # Collapse to single string
      }
      value <- as.character(value) # Ensure value is character
      if (is.na(value) || value == "" || is.null(value)) next # Skip empty values
      value <- gsub("\\s+", " ", trimws(value)) # Normalize whitespace

      if (grepl(";", value) && col_name %in% multi_value_tags && !(col_name %in% free_text_tags)) { # Split multi-value tags
        values <- strsplit(value, ";\\s*")[[1]]
        for (val in values) { # Loop through split values
          val <- trimws(val) # Trim whitespace
          if (val != "") { # Write non-empty values
            writeLines(paste0(col_name, "  - ", val), con)
          }
        }
      } else { # Write single value
        writeLines(paste0(col_name, "  - ", value), con)
      }
    }

    writeLines("ER  - ", con) # End of record
    writeLines("", con)
  }

  close(con)
  cat("RIS file saved to:", file_path, "\n")
}


# Helper function to map RIS tags to descriptive names
.map_ris_tags <- function(df) {
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

  # Handle U1 conditionally based on content
  mapped_names <- sapply(seq_along(names(df)), function(i) {
    original_name <- names(df)[i]
    
    # Special handling for U1
    if (original_name == "U1") {
      # Check if all non-empty values are numeric
      u1_values <- df[[i]]
      non_empty <- u1_values[!is.na(u1_values) & u1_values != ""]
      
      if (length(non_empty) > 0) {
        # Check if all non-empty values contain only digits
        all_numeric <- all(grepl("^[0-9]+$", non_empty))
        
        if (all_numeric) {
          return("eppi_id")
        } else {
          return("custom_info")
        }
      } else {
        return("custom_info")  # Default if no data
      }
    }
    
    # Regular mapping for other tags
    if (original_name %in% names(tag_map)) {
      return(tag_map[[original_name]])
    } else {
      return(original_name)
    }
  })
  
  unique_mapped <- unique(mapped_names)
  result_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  for (base_name in unique_mapped) {
    matching_indices <- which(mapped_names == base_name)
    
    if (length(matching_indices) == 1) {
      # No duplicates, just use the mapped name
      result_df[[base_name]] <- df[[matching_indices]]
    } else {
      # Multiple columns map to this name
      # For each row, collect all unique non-empty values across matching columns
      result_df[[base_name]] <- character(nrow(df))
      
      # Track which column was used for base value per row
      used_col_per_row <- integer(nrow(df))
      
      for (row_idx in seq_len(nrow(df))) {
        # Find first non-empty value for base column
        for (col_idx in matching_indices) {
          val <- df[row_idx, col_idx]
          if (!is.na(val) && val != "") {
            result_df[row_idx, base_name] <- val
            used_col_per_row[row_idx] <- col_idx
            break
          }
        }
      }
      
      # Create numbered columns for remaining unique values
      if (length(matching_indices) > 1) {
        counter <- 2
        for (col_idx in matching_indices) {
          col_name <- paste0(base_name, counter)
          result_df[[col_name]] <- character(nrow(df))
          added_any <- FALSE
          
          for (row_idx in seq_len(nrow(df))) {
            # Skip if this column was used for the base value in this row
            if (used_col_per_row[row_idx] == col_idx) next
            
            val <- df[row_idx, col_idx]
            base_val <- result_df[row_idx, base_name]
            
            # Only add if: has content, base is filled, and value differs from base
            if (!is.na(val) && val != "" && base_val != "" && val != base_val) {
              result_df[row_idx, col_name] <- val
              added_any <- TRUE
            }
          }
          
          # Only increment counter if we added any values
          if (added_any) {
            counter <- counter + 1
          } else {
            # Remove the column if it's empty
            result_df[[col_name]] <- NULL
          }
        }
      }
    }
  }
  
  return(result_df)
}

# Helper function to reverse map descriptive names to RIS tags
.reverse_map_ris_tags <- function(df) {
  # Create reverse mapping from descriptive names to RIS tags
  # Using the most common/standard tag for each descriptive name
  reverse_tag_map <- c(
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
  
  result_df <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  for (col_name in names(df)) {
    # Check if this is a numbered variant (e.g., author2, editor3)
    base_name <- sub("[0-9]+$", "", col_name)
    
    # Handle numbered columns - map back to original RIS tag
    if (base_name != col_name && base_name %in% names(reverse_tag_map)) {
      ris_tag <- reverse_tag_map[[base_name]]
      result_df[[ris_tag]] <- df[[col_name]]
    } else if (col_name %in% names(reverse_tag_map)) {
      # Regular mapping
      ris_tag <- reverse_tag_map[[col_name]]
      result_df[[ris_tag]] <- df[[col_name]]
    } else {
      # Column name not in map, keep as is
      result_df[[col_name]] <- df[[col_name]]
    }
  }
  
  return(result_df)
}