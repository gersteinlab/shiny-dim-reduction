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
  "citations" = "<br><br> No citations found.",
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

#' saves current application data
save_app_data <- function()
{
  stopifnot(is_app_data(app_data), sdr_config$mode == "pipeline")
  saveRDS(app_data, get_app_loc("app_data.rds"))
}

# -----------------
# ROW / COL SUBSETS
# -----------------

get_row_axs_names <- function()
{
  names(app_data$row_axes)
}

#' row_axs to row_axis
#'
#' @param row_axs [row_axs]
#' @returns [string]
get_row_axis <- function(row_axs)
{
  stopifnot(row_axs %in% get_row_axs_names())
  app_data$row_axes[[row_axs]]
}

get_col_axs_names <- function()
{
  names(app_data$col_axes)
}

get_col_axis <- function(col_axs)
{
  stopifnot(col_axs %in% get_col_axs_names())
  app_data$col_axes[[col_axs]]
}

# ----------------
# SUBSET SUMMARIES
# ----------------

#' gets the summary of an axis, which
#' focuses on subsets
#'
#' @param axis [axis] not checked
#' @returns [list] of subset lengths
get_axis_summary <- function(axis)
{
  c(
    list("Total" = axis$length),
    lapply(axis$subsets, length)
  )
}

get_row_axis_summary <- function(row_axs)
{
  get_row_axis(row_axs) %>% get_axis_summary()
}

get_col_axis_summary <- function(col_axs)
{
  get_col_axis(col_axs) %>% get_axis_summary()
}

# ---------------
# CATEGORY LOOKUP
# ---------------

get_cat_names <- function()
{
  names(app_data$categories)
}

#' cat to row_axs (row axis name)
#'
#' @param cat [string]
#' @returns [string]
cat_to_row_axs <- function(cat)
{
  stopifnot(cat %in% get_cat_names())
  app_data$categories[[cat]]$row_axs
}

cat_to_row_axis <- function(cat)
{
  cat_to_row_axs(cat) %>% get_row_axis()
}

#' cat to col_axs (col axis name)
#'
#' @param cat [string] not checked
#' @returns [string]
cat_to_col_axs <- function(cat)
{
  stopifnot(cat %in% get_cat_names())
  app_data$categories[[cat]]$col_axs
}

#' cat to col_axis
#'
#' @param cat [string] not checked
#' @returns [string]
cat_to_col_axis <- function(cat)
{
  cat_to_col_axs(cat) %>% get_col_axis()
}

cat_to_row_axis_summary <- function(cat)
{
  cat_to_row_axs(cat) %>% get_row_axis_summary()
}

cat_to_col_axis_summary <- function(cat)
{
  cat_to_col_axs(cat) %>% get_col_axis_summary()
}

# ------------------
# PERFORM SUBSETTING
# ------------------

#' gets the row subset as row indices
#'
#' @param cat [string] not checked
#' @param row [string] not checked, can't be 'Total'
#' @returns [integer]
get_row_sub <- function(cat, row)
{
  cat_to_row_axis(cat)$subsets[[row]]
}

#' gets the row subset as rownames
#'
#' @param cat [string] not checked
#' @param row [string] not checked, can't be 'Total'
#' @returns [character]
get_row_sub_names <- function(cat, row)
{
  row_axis <- cat_to_row_axis(cat)
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
