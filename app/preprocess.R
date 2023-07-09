# The purpose of this file is to name, initialize, and set roles for reduction parameters,
# while also enabling the naming, storing, and loading of reduction files.

if (!exists("sdr_config"))
  source("app/install.R")

source_app("storage.R")

# ---------------
# HANDLE APP DATA
# ---------------

#' whether x is an 'app_data' object
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_app_data <- function(x) {
  members <- c(
    "title",
    "citations",
    "credentials",
    "row_axes",
    "col_axes",
    "categories",
    "groups",
    "local_store",
    "cloud_store"
  )
  is.list(x) && identical(names(x), members) &&
    is_str(x$title) && is_str(x$citations) &&
    are_credentials(x$credentials) &&
    are_axes(x$row_axes) && are_axes(x$col_axes) &&
    are_categories(x$categories) &&
    categories_match_axes(x$categories, x$row_axes, x$col_axes) &&
    are_groups(x$groups) &&
    groups_match_categories(x$groups, x$categories) &&
    is_local_store(x$local_store) &&
    is_cloud_store(x$cloud_store)
}

# ensure the data for the application is valid
app_data_loc <- get_app_loc("app_data.rds")
# saveRDS(app_data, app_data_loc)
if (!file.exists(app_data_loc))
  stop_f("The application data could not be found at: %s", app_data_loc)

app_data <- readRDS(app_data_loc)
if (!is_app_data(app_data))
  stop("The application data ('app_data.rds') is invalid.
Please delete 'app_data.rds' and rerun the application.")
assign_global("app_data", app_data)

for (dep in names(app_data))
  assign_global(dep, app_data[[dep]])
rm(dep)

assign_global("cat_names", names(categories))

# retrieves a row axis by category
get_row_axis <- function(cat)
{
  row_axs <- categories[[cat]]$row_axs
  row_axes[[row_axs]]
}

# retrieves a col axis by category
get_col_axis <- function(cat)
{
  col_axs <- categories[[cat]]$col_axs
  col_axes[[col_axs]]
}

# summarizes an axis
summarize_axis <- function(axis)
{
  c(
    list("Total" = axis$length),
    lapply(axis$subsets, length)
  )
}

# for checking if row subsets are valid
row_subset_lengths <- empty_named_list(cat_names)
col_subset_lengths <- empty_named_list(cat_names)

for (cat in cat_names)
{
  row_axis <- get_row_axis(cat)
  row_subset_lengths[[cat]] <- summarize_axis(row_axis)

  col_axis <- get_col_axis(cat)
  col_subset_lengths[[cat]] <- summarize_axis(col_axis)
}

# ---------------------------
# ROW / COL UTILITY FUNCTIONS
# ---------------------------

# subsets data by a row subset
subset_by_row <- function(data, cat, row)
{
  row_axis <- get_row_axis(cat)
  stopifnot(nrow(data) == row_axis$length)
  if (row == "Total")
    return(data)
  data[row_axis$subsets[[row]], , drop = FALSE]
}

# subsets data by a col subset
subset_by_col <- function(data, cat, col)
{
  if (col == "Total")
    return(data)
  col_axis <- get_col_axis(cat)
  data[, col_axis$subsets[[col]], drop = FALSE]
}

# gets the safe characteristics of a category
get_safe_chas <- function(cat)
{
  row_meta <- get_row_axis(cat)$metadata
  colnames(row_meta)[apply(row_meta, 2, num_unique) >= 2]
}
