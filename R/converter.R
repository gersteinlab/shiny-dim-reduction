# The goal of this script is to store functions related
# to converting from raw data to combined data and metadata.
# Actual converter files should source this file.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("workflows.R")
source_sdr("find_replace.R")
source_sdr("storage.R")
source_sdr("authentication.R")

# ------------
# DEPENDENCIES
# ------------

# Lists the dependencies that ought to be generated for the app.
# these are ordered from most to least necessary for app function.
dependencies <- function()
{
  print_clean("DEPENDENCY 01 (REQUIRED): categories_full.rds")
  print_clean("A list of groups (ex: cCREs, Expression, Proteomics), where each
group is a list of categories (ex: H3K27ac, H3K9me3, Methylation) and the value
of each category is the number of features prior to dimensionality reduction.
All categories must be unique, even if in different groups.")
  print_clean("")

  print_clean("DEPENDENCY 02 (OPTIONAL): amazon_keys.rds")
  print_clean("A vector for AWS - access ID, access secret, and bucket.")
  print_clean("")

  print_clean("DEPENDENCY 03 (OPTIONAL): order_total.rds")
  print_clean("A list of data frames, one for each category.
Each data frame has samples as rows and columns as metadata features.")
  print_clean("")

  print_clean("DEPENDENCY 04 (OPTIONAL): decorations.rds")
  print_clean("A list of decorations, where each decoration contains (i) a
vector of applicable categories, (ii) the row subsets, and (iii) the column subsets.
Each subset contains (a) a reference character vector and (b) indices that constitute subsets.")
  print_clean("")

  print_clean("DEPENDENCY 05 (OPTIONAL): app_title.rds")
  print_clean("The title of the application.")
  print_clean("")

  print_clean("DEPENDENCY 06 (OPTIONAL): app_citations.rds")
  print_clean("The data-related citations for this application.")
  print_clean("")

  print_clean("DEPENDENCY 07 (OPTIONAL): user_credentials.rds")
  print_clean("A list of user credentials, where usernames are
names(user_credentials) and passwords are unlist(user_credentials).")
  print_clean("")

  print_clean("DEPENDENCY 08 (OPTIONAL): custom_color_scales.rds")
  print_clean("A list of custom color scales, where each scale is a list such that
the labels are names(scale) and the colors are unlist(scale).")
  print_clean("")
}

# ---------
# FUNCTIONS
# ---------

# reads tsv text
read_tsv_text <- function(filename)
{
  strsplit(readLines(filename), split='\t', fixed=TRUE)
}

# converts a matrix to a numeric matrix,
# removing columns with no valid entries
convert_to_num <- function(data){
  data <- apply(data, 1:2, as.numeric)
  data[is.na(data)] <- 0
  data[is.nan(data)] <- 0
  data[,colSums(data) > 0]
}

# converts the first row of a matrix to column names
r1_to_cols <- function(data){
  if (length(data) < 1 || nrow(data) < 1)
  {
    print("Warning: < 1 row")
    return(data)
  }
  colnames(data) <- data[1,]
  data[-1,,drop=FALSE]
}

# a very permissive tryCatch that ignores all errors and warnings
try_catch_ignore <- function(expr)
{
  tryCatch(
    expr,
    warning = function(e){NULL},
    error = function(e){NULL},
    finally = {NULL}
  )
}

# a function that attempts a mass download,
# returning all indices that the download failed at
# url_vec: a vector of URLs
# loc_vec: a vector of corresponding locations to write to
# chunk_size: the number of concurrent downloads to be tried at a time
mass_download <- function(url_vec, loc_vec, chunk_size = 100)
{
  # input validation
  len <- length(url_vec)

  if (len != length(loc_vec))
    stop("Length of URL vector does not equal length of location vector.")

  if (len < 1)
    return(numeric())

  failed_indices <- numeric()

  # separate into chunks
  chunk_indices <- c(seq(1, len, chunk_size), len+1)
  num_chunks <- length(chunk_indices)-1

  start <- my_timer()

  for (i in seq_len(num_chunks))
  {
    sprintf_clean("Downloading chunk: %s/%s", i, num_chunks)
    chunk <- chunk_indices[i]:(chunk_indices[i+1]-1)
    download.file(url_vec[chunk], loc_vec[chunk], method = "libcurl")

    for (j in chunk)
      if (!file.exists(loc_vec[j]))
        failed_indices <- c(failed_indices, j)

    num_items_done <- max(i*chunk_size, len)
    sprintf_clean("Number of items failed: %s", length(failed_indices)-1)
    sprintf_clean("Seconds per item: %s", round(my_timer(start)/num_items_done, digits=4))
  }

  failed_indices
}

# gets the fraction of values of x that are not in 'unacceptable'
frac_acceptable <- function(x, unacceptable=list(NA, NaN, NULL, "", "Unknown")){
  1 - sum(x %in% unacceptable)/length(x)
}

# a version of dplyr::bind_rows that works extremely quickly
# when binding many small matrices together
chunk_bind_rows <- function(data, num_chunks=1, bind_fun = dplyr::bind_rows)
{
  len <- length(data)

  if (len < 2)
    return(data)

  if (is.null(names(data)))
    names(data) <- 1:len

  if (num_chunks < 2)
    return(bind_fun(data))

  chunk_size <- ceiling(len/num_chunks)

  result <- my_empty_list(1:num_chunks)

  for (i in 1:num_chunks)
  {
    min_ind <- (i-1)*chunk_size+1
    max_ind <- min(len, i*chunk_size)
    result[[i]] <- data.frame(bind_fun(data[min_ind:max_ind]))
  }

  result
}

# does chunk_bind_rows but a little more quickly
local_bind <- function(data, num = 20)
{
  chunk_bind_rows(data, num) %>% chunk_bind_rows()
}

# selects the columns of data with the top num standard deviations
ind_sd_top <- function(data, num)
{
  vals <- apply(data, 2, sd)
  order(vals)[1:num]
}

# given a list representing a table, remove the preamble
# (such as the exRNA data access policy) and make a data frame
# the preamble is identified by not having at least min_size entries
rem_preamble <- function(tsv_list, min_size)
{
  selected_rows <- sapply(tsv_list, length) > min_size
  shortened <- tsv_list[selected_rows]
  do.call(rbind, shortened) %>% r1_to_cols() %>% data.frame()
}

# removes duplicate rows from a matrix
rem_dupe_rows <- function(data)
{
  data[!duplicated(data),,drop=FALSE]
}

# orders the rows of a matrix by the entries in a column
order_by_col <- function(data, column)
{
  data[order(data[,column]),,drop=FALSE]
}
