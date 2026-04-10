# Read an RIS file into a data frame

Parses an RIS file into a data.frame, preserving the order of tags as
they first appear in the file. Repeated tags within a record are
collapsed into a single semicolon-separated string.

## Usage

``` r
read_ris_to_dataframe(file_path)
```

## Arguments

- file_path:

  character. Path to the RIS file to read.

## Value

A data.frame with one row per record and one column per encountered RIS
tag, using descriptive column names. Columns are ordered by first
appearance of the tag in the file. Repeated tag values are collapsed
with "; ".

## Examples

``` r
if (FALSE) { # \dontrun{
df <- read_ris_to_dataframe("data-raw/raw data/apa_psycinfo_test_data.ris")
} # }
```
