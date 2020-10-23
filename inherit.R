# This file stores all functions and constants that should be preserved
# across all files, from data validation to processing to the tool.
# source("~/Justin-Tool/code/inherit.R")

require("shiny")
require("dplyr")
require("stringi")
require("aws.s3")
require("bcrypt")

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

# attempts to retrieve a file from a directory, returning a default otherwise
get_from_dir <- function(filename, default, dir = "dependencies")
{
  if (sprintf("%s.rds", filename) %in% list.files(dir))
    default <- readRDS(sprintf("%s/%s.rds", dir, filename))
  assign(filename, default, envir = .GlobalEnv)
  return(NULL)
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

# assigns Amazon keys
assign_keys <- function(amazon_keys)
{
  Sys.setenv("AWS_ACCESS_KEY_ID" = amazon_keys[1],
             "AWS_SECRET_ACCESS_KEY" = amazon_keys[2])
  assign("aws_bucket", amazon_keys[3], envir = .GlobalEnv)
  return(NULL)
}

# straightforward password hashing
my_hash <- function(password)
{
  bcrypt::hashpw(password, gensalt(12))
}

# -----------------------
# CONSTANT INITIALIZATION
# -----------------------

# scale options 
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Global Min-Max", "Local Min-Max", 
                 "Global Z-Score", "Local Z-Score", 
                 "Local Quantile")
# feature options
fea_options <- c("100%", "10%", "1%")
# embedding options
emb_options <- c("PCA", "VAE", "UMAP", "PHATE", "Sets")
# visualization options
vis_options <- c("Explore", "Summarize", "tSNE")
# visualization options as nouns
vis_nouns <- c("Exploration of ", "Summary of ", "tSNE of ")

# creates the name of a file in AWS
# For emb and vis (non-Sets) ...
# PCA: 1 (explore) + 1 (summarize) + 5*2 (tSNE) = 12
# VAE: 1 (explore) + 1 (summarize) + 5*2 (tsNE) = 12
# UMAP: 5 (explore) + 1 (summarize) + 5*2 (tSNE) = 16
# PHATE: 5*2 (2D and 3D)
# combinatorially ... 2 (sca) * 2 (nor) * 3 (fea) * 50 (emb) = 600 files
# Note that Sets undergoes neither grouping nor decompression ...
# For subsets and categories, we expect a leap in file number for AWS ...
make_aws_name <- function(sca, nor, fea, emb, vis, dim_ind, per_ind, sub_ind, cat_ind)
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
  
  sprintf("Dim_Red/%s_%s_%s_%s_%s_%s_%s_%s_%s.rds",
          sca_ind, nor_ind, fea_ind, emb_ind, vis_ind, 
          dim_ind, per_ind, sub_ind, cat_ind)
}

# creates category-related and subset-related data
# to remove: rm(cat_groups, name_cat, num_cat, categories, sub_groups)
init_cat_sub <- function(categories_full, decorations)
{
  # cat groups
  assign("cat_groups", lapply(categories_full, names), envir = .GlobalEnv)
  
  # name_cat
  name_cat <- unlist(cat_groups)
  names(name_cat) <- NULL
  assign("name_cat", name_cat, envir = .GlobalEnv)
  
  # num_cat
  assign("num_cat", length(name_cat), envir = .GlobalEnv)
  
  # categories
  categories <- unlist(categories_full, recursive=FALSE)
  names(categories) <- name_cat
  assign("categories", categories, envir = .GlobalEnv)
  
  # sub_groups
  sub_groups <- my_empty_list(name_cat)

  for (cat in name_cat)
    sub_groups[[cat]] <- "Total"
  
  for (dec_group in decorations)
    for (gc in dec_group$Categories)
      sub_groups[[gc]] <- c(sub_groups[[gc]], names(dec_group$Subsets)[-1])
  
  assign("sub_groups", sub_groups, envir = .GlobalEnv)
  
  return(NULL)
}
