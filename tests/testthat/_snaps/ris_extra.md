# read_ris_to_dataframe() warns about bytes that cannot be recovered

    Code
      df <- read_ris_to_dataframe(write_ris(bytes = bytes))
    Condition
      Warning:
      1 line(s) had byte(s) that could not be decoded by any candidate encoding (Windows-1252/Latin-1) and were dropped: line(s) 2.

