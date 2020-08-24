# This file stores all functions and constants that should be preserved
# across all files, from data validation to processing to the tool.
# source("~/Justin-Tool/code/inherit.R")

require("shiny")
require("dplyr")
require("stringi")
require("aws.s3")

# ---------
# FUNCTIONS
# ---------

# returns the current time in seconds, rounded to 4 digits
# If a start time is given, returns the elapsed time
my_timer <- function(start){
  if (missing(start))
    start <- 0
  
  round(as.numeric(Sys.time()) - start, 4)
}

# creates an empty list with names
my_empty_list <- function(names)
{
  target <- vector(mode="list", length=length(names))
  names(target) <- names
  target
}

# my version of RDS operations
myRDS <- function(name, data)
{
  if (!missing(data))
  {
    saveRDS(data, name, compress=TRUE)
    return(NULL)
  }
  readRDS(name)
}

# fixed pattern replacement in a vector of strings
repStr <- function(x_stringi, pattern, replacement)
{
  stri_replace_all_fixed(
    x_stringi, pattern = pattern, 
    replacement = replacement, 
    vectorize_all = FALSE)
}

# regex pattern replacement in a vector of strings
regStr <- function(x_stringi, pattern, replacement)
{
  stri_replace_all_regex(
    x_stringi, pattern = pattern, 
    replacement = replacement, 
    vectorize_all = FALSE)
}

# adds a percent sign to a string or number
add_perc <- function(str)
{
  sprintf("%s%%", str)
}

# removes a percent sign from a string
rem_perc <- function(str)
{
  repStr(str, "%", "") %>% as.numeric()
}

# scale options 
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Raw", "Normalized")
# feature options
fea_options <- c("100%", "10%", "1%")
# embedding options
emb_options <- c("PCA", "VAE", "UMAP", "PHATE", "Sets")
# visualization options
vis_options <- c("Explore", "Summarize", "tSNE")
# visualization options as nouns
vis_nouns <- c("Exploration of ", "Summary of ", "tSNE of ")

# For emb and vis (non-Sets) ...
# PCA: 1 (explore) + 1 (summarize) + 5*2 (tSNE) = 12
# VAE: 1 (explore) + 1 (summarize) + 5*2 (tsNE) = 12
# UMAP: 5 (explore) + 1 (summarize) + 5*2 (tSNE) = 16
# PHATE: 5*2 (2D and 3D)
# combinatorially ... 2 (sca) * 2 (nor) * 3 (fea) * 50 (emb) = 600 files
# Note that Sets undergoes neither grouping nor decompression ...
# For subsets and categories, we expect a leap in file number for AWS ...

# makes a file name for app data storage, does not apply to sets
make_file_name <- function(sca, nor, fea, emb, vis, dim_ind, per_ind)
{
  sca_ind <- which(sca_options == sca)
  nor_ind <- which(nor_options == nor)
  fea_ind <- which(fea_options == add_perc(fea))
  emb_ind <- which(emb_options == emb)
  vis_ind <- which(vis_options == vis)
  
  if (emb == "PHATE")
  {
    vis_ind <- "X"
  }
  
  if (emb == "PCA" || emb == "VAE")
  {
    if (vis == "Explore" || vis == "Summarize")
    {
      per_ind <- "X"
      dim_ind <- "X"
    }
  }
  
  if (emb == "UMAP")
  {
    if (vis == "Explore" || vis == "Summarize")
    {
      dim_ind <- "X"
    }
    if (vis == "Summarize")
    {
      per_ind <- "X"
    }
  }
  
  sprintf("Dim_Red/%s_%s_%s_%s_%s_%s_%s.rds",
          sca_ind, nor_ind, fea_ind, emb_ind, vis_ind, dim_ind, per_ind)
}

# converts a file name to an AWS name
make_aws_name <- function(name, sub, cat)
{
  clean_addr <- regStr(name, c("^Dim_Red/", ".rds$"), c("", ""))
  sprintf("Dim_Red/%s_%s_%s.rds", clean_addr, sub, cat)
}

# saves an object to Amazon AWS
save_db <- function(dat, bucket, filename){
  my_amazon_obj <- dat
  s3save(my_amazon_obj, bucket=bucket, object=filename)
  my_amazon_obj <- NULL
}

# loads an object from Amazon AWS
load_db <- function(filename, bucket){
  s3load(filename, bucket)
  my_amazon_obj
}

# attempts to retrieve a file from a directory, returning a default otherwise
get_from_dir <- function(name, default, dir)
{
  if (name %in% list.files(dir))
    return(myRDS(sprintf("%s/%s", dir, name)))
  return(default)
}