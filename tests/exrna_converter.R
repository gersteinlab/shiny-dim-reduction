# The goal of this script is to store functions related
# to converting from raw data to combined data and metadata.
# Actual converter files should source this file.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

# ---------
# FUNCTIONS
# ---------

#' syntactic sugar for saving a variable
#' in the current working directory
#'
#' @param name A string.
get_self_rds <- function(name)
{
  assign_global(name, readRDS(sprintf("%s.rds", name)))
}

#' syntactic sugar for loading a variable
#' from the current working directory
#'
#' @param name A string.
set_self_rds <- function(name)
{
  saveRDS(get(name), sprintf("%s.rds", name))
}

# reads tsv text
read_tsv_text <- function(filename)
{
  strsplit(readLines(filename), split = '\t', fixed = TRUE)
}

# converts a matrix to a numeric matrix,
# removing columns with no valid entries
convert_to_num <- function(data){
  data[] <- lapply(data, as.numeric)
  data <- as.matrix(data)
  data[is.na(data) | is.nan(data)] <- 0
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
  chunk_seq <- seq_len(num_chunks)

  result <- empty_named_list(chunk_seq)

  for (i in chunk_seq)
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

# given a vector of file locations, open the files as .tsv and rbind their contents
tsv_files_to_matrix <- function(locs)
{
  result <- empty_named_list(locs)

  for (i in seq_along(locs))
    result[[i]] <- do.call(rbind, read_tsv_text(locs[i]))

  result
}

# takes output of tsv_files_to_matrix and assembles it into a metadata df,
# assuming the first row can act as colnames
simple_mat_to_df <- function(result)
{
  result %>% r1_to_cols() %>% data.frame()
}

# takes output of tsv_files_to_matrix and assembles it into a metadata df,
# assuming duplicates and transpose with 1st initial col being future colnames
transpose_to_df <- function(result){
  if (is.null(result))
    return(empty_df)

  result[,1] <- make.unique(result[,1])

  result %>% t() %>% r1_to_cols() %>% data.frame()
}

# given a list of results from tsv_files_to_matrix, rbind into combined df
transpose_to_df_multi <- function(res_list)
{
  lapply(res_list, transpose_to_df) %>% dplyr::bind_rows()
}

# gives the interval that each number lies in
# ex: get_intervals(3:27, 5)
get_intervals <- function(x, interval_size){
  num <- floor(as.numeric(x)/interval_size) * interval_size
  sprintf("%s to %s", num, num+interval_size)
}

# given a list of data frames, give the indices of all
# data frames not containing a column named "Unknown"
get_known_df_indices <- function(df_list)
{
  good_indices <- numeric()

  for (i in seq_along(df_list))
  {
    if (!("Unknown" %in% colnames(df_list[[i]])))
      good_indices <- c(good_indices, i)
  }

  good_indices
}

# given sample_names and a metadata df where metadata[[col]] is supposed
# to match sample_names, add "Unknown" blanks and reorder metadata so that
# all.equal(metadata[[col]], sample_names) now holds true
match_metadata_to_samples <- function(sample_names, ord, col = "FASTQ_IDENTIFIER")
{
  missed <- setdiff(sample_names, ord[[col]])
  seq_missed <- seq_along(missed)
  original_len <- nrow(ord)
  ord[original_len + seq_missed,] <- "Unknown"
  ord[original_len + seq_missed, col] <- missed
  result <- ord[match(sample_names, ord[[col]]),]
  rownames(result) <- NULL
  result
}

# remove columns where the ratio of NAs to total entries exceeds frac
remove_excess_na_cols <- function(data, frac){
  cutoff <- frac*nrow(data)
  col_n <- ncol(data)
  keep_indices <- rep(TRUE, col_n)

  for (j in seq_len(col_n))
    keep_indices[j] <- (sum(is.na(data[[j]])) <= cutoff)

  data[,keep_indices,drop=FALSE]
}

# example input for make_categories_full
exrna_raw_categories_full <- list(
  "Extracellular RNA" = c(
    "miRNA",
    "piRNA",
    "tRNA",
    "circRNA",
    "ex_miRNA"
  ),
  "Exogenous RNA" = c(
    "cumulative_ex_genomes",
    "specific_ex_genomes",
    "cumulative_ex_ribosomes",
    "specific_ex_ribosomes"
  ),
  "Taxonomy" = c(
    "rRNA_Species",
    "Gene_Species",
    "rRNA_Transpose",
    "Gene_Transpose"
  ),
  "RNA Binding Proteins" = c(
    "RNA_binding_proteins"
  )
)

# makes a list of lists used for total dimensions
make_categories_full <- function(raw_categories_full)
{
  groups <- names(raw_categories_full)
  answer <- empty_named_list(groups)

  for (grp in groups)
  {
    categories <- raw_categories_full[[grp]]
    answer[[grp]] <- empty_named_list(categories)
    for (cat in categories)
      answer[[grp]][[cat]] <- dim(readRDS(sprintf("%s/combined_%s.rds", com_loc, cat)))
  }

  answer
}

# ------------
# EXPERIMENTAL
# ------------

# the following functions work with rRNA / Gene files downloaded
# from the exRNA Atlas:

# converts a list of levels / names into a sparse table
populate_taxa <- function(data)
{
  result <- data.frame(matrix(0, nrow=nrow(data), ncol=num_taxa))
  for (i in seq_len(nrow(data)))
    result[i, taxonomic_ordering == data[i, 1]] <- data[i, 3]
  result
}

# a super-optimized function for populating the tree to species only
species_only <- function(data)
{
  working <- rep(0, num_spe-1)

  for (i in seq_len(nrow(data)))
  {
    if (data[i, num_spe] != 0)
    {
      data[i, 1:(num_spe-1)] <- working
    }
    else
    {
      for (j in (num_spe-1):1)
      {
        if (data[i,j] != 0)
        {
          working[j] <- data[i,j]
          break
        }
        else
        {
          working[j] <- 0
        }
      }
    }
  }

  data[data[,num_spe] != 0,1:num_spe]
}

truncate_data <- function(data)
{
  data[data$level != "no rank",c(2,3,5,6)]
}

ran_df_to_combined <- function(df)
{
  df$FASTQ_IDENTIFIER <- NULL
  rownames(df) <- NULL
  result <- convert_to_num(df)
  sprintf_clean("Was a valid table made? %s", valid_table(result))
  result
}
