# The purpose of this file is to validate numeric data and metadata files.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

# determines if a table is valid for dimensionality reduction
# note: anything of the form "combined_miRNA" should satisfy this
valid_table <- function(cand_table)
{
  if (!all.equal(class(matrix()), class(cand_table)))
    return(FALSE)

  row_n <- nrow(cand_table)
  col_n <- ncol(cand_table)

  # refuse to have less than 4 rows initially, since 3 points define a plane
  # refuse to have less than 4 columns initially, since then you can immediately plot on 3D
  # and plot on 2D for second-round reductions.
  if (row_n < 4 || col_n < 4)
    return(FALSE)

  if (sum(is.na(cand_table)) > 0) # NAs not allowed
    return(FALSE)

  if (sum(is.nan(cand_table)) > 0) # NaNs not allowed
    return(FALSE)

  # row names should be absent
  if (!is.null(rownames(cand_table)))
    return(FALSE)

  # column names are required
  if (length(colnames(cand_table)) != col_n)
    return(FALSE)

  for (j in seq_len(col_n))
    if (!is.numeric(cand_table[,j]))
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
