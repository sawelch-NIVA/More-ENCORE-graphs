# Convert the raw HUGIN text exports to parquet (run once per new export).
# The txt files have a header with one fewer column than the data rows (the
# first column is an unnamed row index), so we supply column names explicitly
# and drop that index.
#
# Usage (from project root): source("R/convert_to_parquet.R")

suppressPackageStartupMessages({
  library(readr)
  library(arrow)
})

convert_txt_to_parquet <- function(txt_path, parquet_path) {
  header <- strsplit(read_lines(txt_path, n_max = 1), "\t")[[1]]
  df <- read_tsv(
    txt_path,
    skip = 1,
    col_names = c("row_index", header),
    col_types = cols(
      Inst_Group = col_character(),
      Inst_Substance = col_character(),
      .default = col_guess()
    ),
    progress = FALSE
  )
  stopifnot(!anyNA(df$row_index), nrow(df) > 0)
  df$row_index <- NULL
  write_parquet(df, parquet_path)
  message("wrote ", parquet_path, " (", nrow(df), " rows)")
  invisible(parquet_path)
}
