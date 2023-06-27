# The purpose of this file is to name, initialize, and set roles for reduction parameters,
# while also enabling the naming, storing, and loading of reduction files.

if (!exists("sdr_config"))
  source("app/install.R")

source_sdr("storage.R")

# -------------
# LOAD APP DATA
# -------------

is_app_data <- function(app_data) {
  TRUE
}

# assign globally with default

load_app_data <- function(app_data) {
  for (dep in names(app_data))
    assign_global(dep, app_data[[dep]])
}

# ensure the data for the application is valid
app_data_loc <- get_app_loc("app_data.rds")
if (!file.exists(app_data_loc))
{

}

app_data <- readRDS("app_data.rds")
if (!is_app_data(app_data))
  stop("The application data ('app_data.rds') is invalid.
Please delete 'app_data.rds' and rerun the application.")
load_app_data(app_data)

# ----------------
# ANALYSIS OPTIONS
# ----------------

# scale options
sca_options <- c(
  "Logarithmic",
  "Linear"
)

# normalization options
nor_options <- c(
  "Global Min-Max",
  "Local Min-Max",
  "Global Z-Score",
  "Local Z-Score",
  "Quantile"
)

# embedding options
emb_options <- c(
  "PCA",
  "VAE",
  "UMAP",
  "PHATE",
  "Sets"
)

# visualization options
vis_options <- c(
  "Explore",
  "Summarize",
  "tSNE"
)

# number of digits of precision for non-integer numbers
num_digits <- 4

# -----------------
# CAT / SUB LOADING
# -----------------

# Note: The following functions can be run any time from the application. They must be run
# after workflows.R if used for dimensionality reduction in order to find dependencies.

# creates category-related data
init_cat <- function(groups)
{
  assign_global("cat_names", as.character(unlist(groups)))
  assign_global("cat_length", length(cat_names))
}

# creates subset-related data
# (must be run after init_cat)
init_sub <- function(row_axes, col_axes)
{
  sub_row_groups <- empty_named_list(cat_names)
  sub_col_groups <- empty_named_list(cat_names)

  for (cat in cat_names)
  {
    row_axs <- categories[[cat]]$row_axs
    row_axis <- row_axes[[row_axs]]
    sub_row_groups[[cat]] <- c(
      list("Total" = row_axis$length),
      lapply(row_axis$subsets, length)
    )

    col_axs <- categories[[cat]]$col_axs
    col_axis <- col_axes[[col_axs]]
    sub_col_groups[[cat]] <- c(
      list("Total" = col_axis$length),
      lapply(col_axis$subsets, length)
    )
  }

  assign_global("sub_row_groups", sub_row_groups)
  assign_global("sub_col_groups", sub_col_groups)
}

# ----------------
# SUBSET FUNCTIONS
# ----------------

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
      # ensure that the number of features according to the decoration agrees
      # with the number of features according to categories
      stopifnot(length(ref) == categories[[cat]][2])
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

make_sets_name <- function(cat, sca, thr, cha)
{
  sprintf(sprintf("Sets/%%s/S%%s_%%0.%sf_%%s.rds",
                  num_digits), cat, which(sca_options == sca), thr, cha)
}

make_phate_name <- function(cat, row, col, sca, nor, com, per)
{
  sprintf("PHATE/%s/%s_%s_S%s_N%s_%s_%s.rds",
          cat, row, col, which(sca_options == sca), which(nor_options == nor), com, per)
}

make_pvu_name <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
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

  # UMAP
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

make_sdr_name <- function(cat, row, col, sca, nor, emb, vis, com, dim, per, bat, thr, cha)
{
  if (emb == "Sets")
    return(make_sets_name(cat, sca, thr, cha))

  if (emb == "PHATE")
    return(make_phate_name(cat, row, col, sca, nor, com, per))

  # PCA, VAE, UMAP
  make_pvu_name(cat, row, col, sca, nor, emb, vis, com, dim, per, bat)
}
