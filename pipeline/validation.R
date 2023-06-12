# The purpose of this file is to validate numeric data and metadata files.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

# should be valid as input AND output for reductions
# note: anything of the form "combined_miRNA" should satisfy this
is_num_data <- function(num_data)
{
  # must be a matrix (enforce one type)
  if (!("matrix" %in% class(num_data)))
    return(FALSE)

  # prevents NA, NaN, Inf, -Inf, non-numerics
  if (!all_fin(num_data))
    return(FALSE)

  # needs at least one row and one column
  if (nrow(num_data) < 1 || ncol(num_data) < 1)
    return(FALSE)

  # row names should be absent (tidyverse convention)
  if (!is.null(rownames(cand_table)))
    return(FALSE)

  # column names are required (tidyverse convention)
  if (!is.character(colnames(num_data)))
    return(FALSE)

  TRUE
}

# determines if a data.frame is valid as metadata
valid_metadata <- function(cand_df)
{
  if (!all.equal(class(data.frame()), class(cand_df)))
    return(FALSE)

  row_n <- nrow(cand_df)
  col_n <- ncol(cand_df)

  # must have at least one row and one column
  if (row_n < 1 || col_n < 1)
    return(FALSE)

  if (sum(is.na(cand_df)) > 0) # NAs not allowed
    return(FALSE)

  if (sum(is.nan(as.matrix(cand_df))) > 0) # NaNs not allowed
    return(FALSE)

  if (sum(cand_df == "") > 0) # empty strings not allowed
    return(FALSE)

  TRUE
}
