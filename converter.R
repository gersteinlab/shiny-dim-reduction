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
  return(NULL)
}

# attempts to load an object from the current directory to itself
self_load <- function(filenames)
{
  for (filename in filenames)
    get_from_dir(filename, NULL, dir=getwd())
  return(NULL)
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