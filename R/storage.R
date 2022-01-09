# The purpose of this file is to name, initialize, and set roles for reduction parameters,
# while also enabling the naming, storing, and loading of reduction files.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("read_write.R")

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

# ----------------
# CATEGORY LOADING
# ----------------

# creates category-related data; requires categories_full
# to remove: rm(categories_full, cat_groups, name_cat, num_cat, categories)
get_dependency("categories_full", stop("Critical dependency missing!"))

# cat groups
assign_global("cat_groups", lapply(categories_full, names))

# name_cat
name_cat <- unlist(cat_groups)
names(name_cat) <- NULL
assign_global("name_cat", name_cat)

# num_cat
assign_global("num_cat", length(name_cat))

# categories
categories <- unlist(categories_full, recursive=FALSE)
names(categories) <- name_cat
assign_global("categories", categories)

# ----------------
# SUBSET FUNCTIONS
# ----------------

# creates subset-related data, using a subset_map function that
# converts a list of vectors into a vector of characters
# to remove: rm(decorations, sub_row_groups, sub_col_groups)
init_sub <- function(subset_map)
{
  get_dependency("decorations")

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

# retrieves a row subset, which is a vector of indices
# row subsets are index-based because metadata is expected
# to maintain the same row ordering as the numerical data.
# requires init_sub() to be run first
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
# requires init_sub() to be run first
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
# requires init_sub() to be run first
get_row_sub <- function(data, cat, sub)
{
  if (sub != "Total")
    return(data[get_row_decor_subset(cat, sub),,drop=FALSE])

  data
}

# obtains a subset of data's cols
# requires init_sub() to be run first
get_col_sub <- function(data, cat, sub)
{
  if (sub != "Total")
    return(data[,colnames(data) %in% get_col_decor_subset(cat, sub),drop=FALSE])

  data
}

# ---------------
# ANALYSIS NAMING
# ---------------

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
