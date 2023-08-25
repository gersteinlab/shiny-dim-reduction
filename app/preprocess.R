# The purpose of this file is to load application data
# and preprocess it for effective application / pipeline use.

if (!exists("sdr_config"))
  source("app/install.R")

# ---------------
# IMPORT APP DATA
# ---------------

#' whether x is a 'groups' object
#' note: succeeds if x is list()
#'
#' @param x [object]
#' @returns [boolean]
are_groups <- function(x)
{
  is.list(x) && has_safe_names(x) &&
    all_fun_true(x, is.character)
}

#' whether groups are compatible with categories
#'
#' @param groups [groups] not checked
#' @param categories [categories] not checked
#' @returns [boolean]
groups_match_categories <- function(groups, categories)
{
  group_matches_categories <- function(group)
  {
    all(group %in% names(categories))
  }

  all_fun_true(groups, group_matches_categories)
}

#' whether x is an 'app_data' object
#'
#' @param x [object]
#' @returns [boolean]
is_app_data <- function(x) {
  members <- c(
    "title",
    "citations",
    "row_axes",
    "col_axes",
    "categories",
    "groups"
  )

  is.list(x) && identical(names(x), members) &&
    is_str(x$title) && is_str(x$citations) &&
    are_axes(x$row_axes) && are_axes(x$col_axes) &&
    are_categories(x$categories) &&
    categories_match_axes(x$categories, x$row_axes, x$col_axes) &&
    are_groups(x$groups) &&
    groups_match_categories(x$groups, x$categories)
}

# default application data
app_data <- list(
  "title" = "Shiny Dimensionality Reduction",
  "citations" = "",
  "row_axes" = list(),
  "col_axes" = list(),
  "categories" = list(),
  "groups" = list()
)

#' sets app_data to a_data
#'
#' @param a_data [app_data]
update_app_data <- function(a_data)
{
  stopifnot(is_app_data(a_data))
  app_data <<- a_data
}

#' attempts to load app_data
load_app_data <- function(file)
{
   readRDS(get_app_loc("app_data.rds")) %>% update_app_data()
}

# test the application with this line commented out
load_app_data()
cat_f("APP_DATA LOAD TIME: %.1f (sec)\n", net_time())

# set row axes
assign_global("row_axes", app_data[["row_axes"]])
assign_global("row_axs_names", names(row_axes))

# set col axes
assign_global("col_axes", app_data[["col_axes"]])
assign_global("col_axs_names", names(col_axes))

# set categories
assign_global("categories", app_data[["categories"]])
assign_global("cat_names", names(categories))

# set groups
assign_global("groups", app_data[["groups"]])

#' saves current application data
save_app_data <- function()
{
  stopifnot(is_app_data(app_data), sdr_config$mode == "pipeline")
  saveRDS(app_data, get_app_loc("app_data.rds"))
}

# -----------------
# ROW / COL SUBSETS
# -----------------

#' cat to row_axs (row axis name)
#'
#' @param cat [string] not checked
#' @returns [string]
get_row_axs <- function(cat)
{
  categories[[cat]]$row_axs
}

#' cat to row_axis
#'
#' @param cat [string] not checked
#' @returns [string]
get_row_axis <- function(cat)
{
  row_axes[[get_row_axs(cat)]]
}

#' cat to col_axs (col axis name)
#'
#' @param cat [string] not checked
#' @returns [string]
get_col_axs <- function(cat)
{
  categories[[cat]]$col_axs
}

#' cat to col_axis
#'
#' @param cat [string] not checked
#' @returns [string]
get_col_axis <- function(cat)
{
  col_axes[[get_col_axs(cat)]]
}

#' gets the subset lengths of an axis
#'
#' @param axis [axis] not checked
#' @returns [list] of subset lengths
get_sub_lengths <- function(axis)
{
  c(
    list("Total" = axis$length),
    lapply(axis$subsets, length)
  )
}

# precomputed row subset lengths
row_sub_lengths <- empty_named_list(row_axs_names)
for (row_axs in row_axs_names)
  row_sub_lengths[[row_axs]] <- get_sub_lengths(
    row_axes[[row_axs]])

#' gets row subset lengths for a category
#'
#' @param cat [string] not checked
#' @returns [list] of row subset lengths
get_row_sub_lengths <- function(cat)
{
  row_sub_lengths[[get_row_axs(cat)]]
}

# precomputed col subset lengths
col_sub_lengths <- empty_named_list(col_axs_names)
for (col_axs in col_axs_names)
  col_sub_lengths[[col_axs]] <- get_sub_lengths(
    col_axes[[col_axs]])

#' gets col subset lengths for a category
#'
#' @param cat [string] not checked
#' @returns [list] of col subset lengths
get_col_sub_lengths <- function(cat)
{
  col_sub_lengths[[get_col_axs(cat)]]
}

#' gets the row subset as row indices
#'
#' @param cat [string] not checked
#' @param row [string] not checked, can't be 'Total'
#' @returns [integer]
get_row_sub <- function(cat, row)
{
  get_row_axis(cat)$subsets[[row]]
}

#' gets the row subset as rownames
#'
#' @param cat [string] not checked
#' @param row [string] not checked, can't be 'Total'
#' @returns [character]
get_row_sub_names <- function(cat, row)
{
  row_axis <- get_row_axis(cat)
  indices <- row_axis$subsets[[row]]
  row_axis$metadata[indices, 1]
}

#' subsets data by row indices
#'
#' @param data [matrix, data.frame] not checked
#' @param row_sub [integer] not checked
#' @returns [matrix, data.frame]
subset_by_row <- function(data, row_sub)
{
  data[row_sub, , drop = FALSE]
}

#' subsets data by rownames
#'
#' @param data [matrix, data.frame] not checked
#' @param row_sub_names [character] not checked
#' @returns [matrix, data.frame]
subset_by_row_names <- function(data, row_sub_names)
{
  subset_by_row(data, rownames(data) %in% row_sub_names)
}

#' gets the col subset as indices
#'
#' @param cat [string] not checked
#' @param col [string] not checked, can't be 'Total'
#' @returns [integer]
get_col_sub <- function(cat, col)
{
  get_col_axis(cat)$subsets[[col]]
}

#' gets the col subset as colnames
#'
#' @param cat [string] not checked
#' @param col [string] not checked, can't be 'Total'
#' @returns [character]
get_col_sub_names <- function(cat, col)
{
  col_axis <- get_col_axis(cat)
  indices <- col_axis$subsets[[col]]
  col_axis$metadata[indices, 1]
}

#' subsets data by col indices
#'
#' @param data [matrix, data.frame] not checked
#' @param col_sub [integer] not checked
#' @returns [matrix, data.frame]
subset_by_col <- function(data, col_sub)
{
  data[, col_sub, drop = FALSE]
}

#' subsets data by colnames
#'
#' @param data [matrix, data.frame] not checked
#' @param col_sub_names [character] not checked
#' @returns [matrix, data.frame]
subset_by_col_names <- function(data, col_sub_names)
{
  subset_by_col(data, colnames(data) %in% col_sub_names)
}

cat_f("PREPROCESSING TIME: %.1f (sec)\n", net_time())
