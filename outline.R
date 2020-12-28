# This file stores all functions and constants that should be preserved
# across all files, from data validation to processing to the tool.
# source("outline.R", encoding="UTF-8")

source("installer.R", encoding="UTF-8")

require("shiny")
require("dplyr")
require("stringi")
require("aws.s3")
require("bcrypt")
require("evaluate")

# ---------
# FUNCTIONS
# ---------

# returns the current time in seconds, rounded to 4 digits
# If a start time is given, returns the elapsed time
my_timer <- function(start, num_digits = 4){
  if (missing(start))
    start <- 0
  
  round(as.numeric(Sys.time()) - start, num_digits)
}

# creates an empty list with names
my_empty_list <- function(names)
{
  target <- vector(mode="list", length=length(names))
  names(target) <- names
  target
}

# straightforward password hashing
my_hash <- function(password)
{
  bcrypt::hashpw(password, gensalt(12))
}

# checks if user_credentials[[username]] == password
my_auth <- function(username, password, user_credentials)
{
  length(username) == 1 && length(password) == 1 && is.character(username) &&
    is.character(password) && (username %in% names(user_credentials)) && 
    checkpw(input$password, user_credentials[[input$username]])
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

# removes a percent sign from a string and converts to a number
rem_perc <- function(str)
{
  repStr(str, "%", "") %>% as.numeric()
}

# --------------
# AWS S3 STORAGE
# --------------

# keys linked to a test user
tester_keys <- c(
  "AKIAVI2HZGPOEH4RVQIH",
  "6cteE5iRcwkBIVBtZMm3x0u7J7ncPsbsjZ0PaU4o",
  "shiny-app-data-justin-test"
)

# assigns Amazon Web Service keys
assign_keys <- function(aws_keys)
{
  if (length(aws_keys) != 3)
    stop("Not all AWS keys provided.")
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_keys[1],
             "AWS_SECRET_ACCESS_KEY" = aws_keys[2])
  assign("aws_bucket", aws_keys[3], envir = .GlobalEnv)
  invisible()
}

# checks if a single line evaluation is successful
single_line_eval <- function(single_line, check_warnings = TRUE, check_errors = TRUE)
{
  evaluation <- evaluate::evaluate(quote(single_line))
  no_warnings <- sum("warning" %in% unlist(lapply(evaluation, class))) == 0
  no_errors <- sum("error" %in% unlist(lapply(evaluation, class))) == 0
  (!check_warnings || no_warnings) && (!check_errors || no_errors)
}

# saves a single object to AWS.s3 - modified from s3save
# assumes the existence of an object called 'aws_bucket'
custom_s3save <- function(data, object) 
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save(data, file = tmp, envir = parent.frame())
  put_object(file = tmp, bucket = aws_bucket, object = object)
}

# loads a single object from AWS.s3 - modified from s3load
# assumes the existence of an object called 'aws_bucket'
custom_s3load <- function(object) 
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save_object(bucket = aws_bucket, object = object, file = tmp)
  load(tmp, envir = parent.frame())
  data
}

# ----------------
# ANALYSIS OPTIONS
# ----------------

# scale options 
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Global Min-Max", "Local Min-Max", 
                 "Global Z-Score", "Local Z-Score", 
                 "Quantile")
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
# combinatorially ... 2 (sca) * 5 (nor) * 3 (fea) * 50 (emb) = 1500 files per folder
# Note that Sets undergoes neither grouping nor decompression ...
# For subsets and categories, we expect a leap in file number for AWS ...
make_aws_name <- function(cat, sub, sca, nor, fea, emb, vis, dim_ind, per_ind)
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
  
  sprintf("Dim_Red/%s/%s/%s_%s_%s_%s_%s_%s_%s.rds",
          cat, sub, sca_ind, nor_ind, fea_ind, emb_ind, vis_ind, 
          dim_ind, per_ind)
}

# ------------------
# DEPENDENCY LOADING
# ------------------

# attempts to retrieve the file with the given name and assign it to itself
get_from_dir <- function(filename, default = NULL, dir = "dependencies")
{
  if (sprintf("%s.rds", filename) %in% list.files(dir))
    default <- readRDS(sprintf("%s/%s.rds", dir, filename))
  assign(filename, default, envir = .GlobalEnv)
  invisible()
}

# retrieves a subset based on the list of subsets, the subset name, and the category
# assumes the existence of an object named 'decorations'
get_decor_subset <- function(cat, sub)
{
  for (dec_group in decorations)
  {
    if (cat %in% dec_group$Categories)
    {
      ref <- dec_group$Subsets$Reference
      ind <- dec_group$Subsets[[sub]]
      return(ref[ind])
    }
  }
  
  return(NULL)
}

# obtains a subset of data's rows (margin 1) / columns (margin 2)
# assumes the existence of an object named 'decorations'
get_safe_sub <- function(data, cat, sub, margin=2)
{
  if (sub != "Total")
  {
    indices <- get_decor_subset(cat, sub)
    
    if (margin == 1)
      return(data[rownames(data) %in% indices,,drop=FALSE])
    if (margin == 2)
      return(data[,colnames(data) %in% indices,drop=FALSE])
  }
  
  data
}

# creates category-related data
# assumes the existence of an object named 'categories_full'
# to remove: rm(cat_groups, name_cat, num_cat, categories)
init_cat <- function()
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
  
  invisible()
}

# creates subset-related data, using a subset_map function that
# converts a list of vectors into a vector of characters
# assumes the existence of objects named 'decorations' and 'categories'
# to remove: rm(sub_groups)
init_sub <- function(subset_map)
{
  sub_groups <- my_empty_list(name_cat)
  
  for (cat in name_cat)
    sub_groups[[cat]] <- list("Total"=rep(0, categories[[cat]])) %>% subset_map()
  
  for (dec_group in decorations)
  {
    mapping <- dec_group$Subsets[-1] %>% subset_map()
    
    for (good_cat in dec_group$Categories)
      sub_groups[[good_cat]] <- c(sub_groups[[good_cat]], mapping) 
  }
  
  assign("sub_groups", sub_groups, envir = .GlobalEnv)
  
  invisible()
}