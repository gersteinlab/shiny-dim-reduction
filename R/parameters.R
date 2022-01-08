# This file stores all functions and constants that should be preserved
# across all files, from data validation to processing to the tool.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

library(dplyr)
library(shiny)

# ----------------
# ANALYSIS OPTIONS
# ----------------

# scale options
sca_options <- c("Logarithmic", "Linear")
# normalization options
nor_options <- c("Global Min-Max", "Local Min-Max",
                 "Global Z-Score", "Local Z-Score",
                 "Quantile")
# embedding options
emb_options <- c("PCA", "VAE", "UMAP", "PHATE", "Sets")
# visualization options
vis_options <- c("Explore", "Summarize", "tSNE")
# visualization options as nouns
vis_nouns <- c("Exploration of ", "Summary of ", "tSNE of ")

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

# retrieves a row subset, which is a vector of indices
# row subsets are index-based because metadata is expected
# to maintain the same row ordering as the numerical data.
# assumes the existence of an object named 'decorations'
get_row_decor_subset <- function(cat, row)
{
  for (dec_group in decorations)
    if (cat %in% dec_group$CATEGORIES)
      return(dec_group$ROW_SUBSETS[[row]])

  return(NULL)
}

# retrieves a col subset, which is a vector of column names
# column subsets are name-based because columns can be reordered
# based on metrics (ex: standard deviation) or excluded in a non-trivial
# way (example: thresholding for Sets dimensionality reduction).
# assumes the existence of an object named 'decorations'
get_col_decor_subset <- function(cat, col)
{
  for (dec_group in decorations)
  {
    if (cat %in% dec_group$CATEGORIES)
    {
      ref <- dec_group$COL_SUBSETS$Reference
      ind <- dec_group$COL_SUBSETS[[col]]
      return(ref[ind])
    }
  }

  return(NULL)
}

# obtains a subset of data's rows
# assumes the existence of an object named 'decorations'
get_row_sub <- function(data, cat, sub)
{
  if (sub != "Total")
    return(data[get_row_decor_subset(cat, sub),,drop=FALSE])

  data
}

# obtains a subset of data's cols
# assumes the existence of an object named 'decorations'
get_col_sub <- function(data, cat, sub)
{
  if (sub != "Total")
    return(data[,colnames(data) %in% get_col_decor_subset(cat, sub),drop=FALSE])

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
  sub_row_groups <- empty_named_list(name_cat)
  sub_col_groups <- empty_named_list(name_cat)

  for (cat in name_cat)
  {
    sub_row_groups[[cat]] <- list("Total"=rep(0, categories[[cat]][1])) %>% subset_map()
    sub_col_groups[[cat]] <- list("Total"=rep(0, categories[[cat]][2])) %>% subset_map()
  }

  for (dec_group in decorations)
  {
    mapping_row <- dec_group$ROW_SUBSETS[-1] %>% subset_map()
    mapping_col <- dec_group$COL_SUBSETS[-1] %>% subset_map()

    for (good_cat in dec_group$CATEGORIES)
    {
      sub_row_groups[[good_cat]] <- c(sub_row_groups[[good_cat]], mapping_row)
      sub_col_groups[[good_cat]] <- c(sub_col_groups[[good_cat]], mapping_col)
    }
  }

  assign("sub_row_groups", sub_row_groups, envir = .GlobalEnv)
  assign("sub_col_groups", sub_col_groups, envir = .GlobalEnv)

  invisible()
}
