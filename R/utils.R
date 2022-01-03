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

# saveRDS but we force the creation of the directory
mkdir_saveRDS <- function(data, file)
{
  dest_dir <- dirname(file)
  if (!dir.exists(dest_dir))
    dir.create(dest_dir, recursive=TRUE)
  saveRDS(data, file)
}

make_sdr_name <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat, thr, cha)
{
  sca_ind <- which(sca_options == sca)
  nor_ind <- which(nor_options == nor)

  if (emb == "PCA")
  {
    if (vis == "Explore")
      return(sprintf("PCA_E/%s/%s_%s_S%s_N%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com))
    if (vis == "Summarize")
      return(sprintf("PCA_S/%s/%s_%s_S%s_N%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com))
    if (vis == "tSNE")
      return(sprintf("PCA_T/%s/%s_%s_S%s_N%s_%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, dim, per))
  }

  if (emb == "VAE")
  {
    if (vis == "Explore")
      return(sprintf("VAE_E/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, bat))
    if (vis == "Summarize")
      return(sprintf("VAE_S/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, bat))
    if (vis == "tSNE")
      return(sprintf("VAE_T/%s/%s_%s_S%s_N%s_%s_%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, dim, per, bat))
  }

  if (emb == "UMAP")
  {
    if (vis == "Explore")
      return(sprintf("UMAP_E/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, per))
    if (vis == "Summarize")
      return(sprintf("UMAP_S/%s/%s_%s_S%s_N%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, per))
    if (vis == "tSNE")
      return(sprintf("UMAP_T/%s/%s_%s_S%s_N%s_%s_%s_%s.rds",
                     cat, row, col, sca_ind, nor_ind, com, dim, per))
  }

  if (emb == "Sets")
    return(sprintf("Sets/%s/S%s_%0.3f_%s.rds", cat, sca_ind, thr, cha))

  # PHATE
  return(sprintf("PHATE/%s/%s_%s_S%s_N%s_%s_%s.rds", cat, row, col, sca_ind, nor_ind, com, per))
}

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
get_row_decor_subset <- function(cat, row)
{
  for (dec_group in decorations)
  {
    if (cat %in% dec_group$CATEGORIES)
    {
      ref <- dec_group$ROW_SUBSETS$Reference
      ind <- dec_group$ROW_SUBSETS[[row]]
      return(ref[ind])
    }
  }

  return(NULL)
}

# used for order
get_row_decor_indices <- function(cat, row)
{
  for (dec_group in decorations)
  {
    if (cat %in% dec_group$CATEGORIES)
    {
      return(dec_group$ROW_SUBSETS[[row]])
    }
  }

  return(NULL)
}

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
    return(data[rownames(data) %in% get_row_decor_subset(cat, sub),,drop=FALSE])

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
