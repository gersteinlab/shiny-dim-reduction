# The goal of this script is to store functions related 
# to converting from raw data to combined data and metadata.
# Actual converter.R files should source this file.
# source("~/Justin-Tool/shiny-dim-reduction/converter.R")

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/build.R")

# ---------
# FUNCTIONS
# ---------

# reads tsv text
read_tsv_text <- function(filename)
{
  conn <- file(filename, open="rt")
  x <- conn %>% readLines() %>% strsplit(split='\t')
  close(conn)
  x
}

# converts a matrix to a numeric matrix,
# removing columns with no valid entries
convert_to_num <- function(data){
  storage.mode(data) <- "numeric"
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