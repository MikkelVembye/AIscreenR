# Write a data frame to a RIS file

Writes a data.frame to a RIS file, one record per row. If the data frame
was created by
[`read_ris_to_dataframe()`](https://mikkelvembye.github.io/AIscreenR/reference/read_ris_to_dataframe.md),
the original RIS tag order and tags are preserved where possible.
Otherwise, a standard RIS format is used.

## Usage

``` r
save_dataframe_to_ris(df, file_path)
```

## Arguments

- df:

  data.frame. The data to write.

- file_path:

  character. Path to the output RIS file.

## Value

A character string indicating the file path where the RIS file was
saved.

## Details

If a field value contains semicolons, it is split and written as
multiple tag lines. The `TY` (source type) field is written first for
each record, followed by all other fields. Records are terminated with
`ER - `.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- read_ris_to_dataframe("data-raw/raw data/apa_psycinfo_test_data.ris")
save_dataframe_to_ris(df, "path/to/output.ris")
} # }
```
