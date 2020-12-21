# The goal of this script is to store functions related 
# to converting from raw data to combined data and metadata.
# Actual converter.R files should source this file.
# source("converter.R", encoding="UTF-8")

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("pipeline.R", encoding="UTF-8")

# ---------
# FUNCTIONS
# ---------

# reads tsv text
read_tsv_text <- function(filename)
{
  strsplit(readLines(filename), split='\t', fixed=TRUE)
}

# attempts to save an object with the given name to the current directory
self_save <- function(filenames, compress=TRUE)
{
  for (filename in filenames)
    if (exists(filename))
      saveRDS(get(filename), sprintf("%s.rds", filename), compress=compress)
  return(invisible())
}

# attempts to load an object from the current directory to itself
self_load <- function(filenames)
{
  for (filename in filenames)
    get_from_dir(filename, NULL, dir=getwd())
  return(invisible())
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
  if (nrow(data) < 1)
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
    warning = function(e){
      return()
    },
    error = function(e){
      return()
    },
    finally=NULL
  )
  
  return(invisible())
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
  
  failed_indices <- 0
  
  if (len < 1)
    return(failed_indices)
  
  # separate into chunks
  chunk_indices <- c(seq(1, len, chunk_size), len+1)
  num_chunks <- length(chunk_indices)-1
  
  start <- my_timer()
  
  for (i in 1:num_chunks)
  {
    print_clean(sprintf("Downloading chunk: %s/%s", i, num_chunks))
    
    chunk <- chunk_indices[i]:(chunk_indices[i+1]-1)
    
    try_catch_ignore(
      download.file(url_vec[chunk], loc_vec[chunk], method="libcurl", quiet=TRUE)
    )
    
    for (j in chunk)
      if (!file.exists(loc_vec[j]))
        failed_indices <- c(failed_indices, j)
    
    print_clean(sprintf(
      "Number of items failed: %s", length(failed_indices)-1
    ))
    
    print_clean(sprintf(
      "Seconds per item: %s", 
      round(my_timer(start)/max(i*chunk_size, len), digits=4)
    ))
  }
  
  failed_indices
}