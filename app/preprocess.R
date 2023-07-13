# The purpose of this file is to name, initialize, and set roles for reduction parameters,
# while also enabling the naming, storing, and loading of reduction files.

if (!exists("sdr_config"))
  source("app/install.R")

source_app("authentication.R")
source_app("storage.R")

# ---------------
# IMPORT APP DATA
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
# saveRDS(app_data, app_data_loc, compress = FALSE)
if (!file.exists(app_data_loc))
  stop_f("The application data could not be found at: %s", app_data_loc)

# assign application data
app_data <- readRDS(app_data_loc)
if (!is_app_data(app_data))
  stop("The application data ('app_data.rds') is invalid.
Please delete 'app_data.rds' and rerun the application.")
assign_global("app_data", app_data)
cat_f("APP_DATA LOAD TIME: %.1f (sec)\n", net_time())

# set credentials
set_credentials(app_data[["credentials"]])

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

# set stores
assign_global("connected", decide_store_mode(
  app_data[["local_store"]],
  app_data[["cloud_store"]]
))

# -----------------------
# GENERAL STORE FUNCTIONS
# -----------------------

list_store <- function(prefix)
{
  if (connected == "local")
    return(list_local(prefix))
  if (connected == "cloud")
    return(list_aws_s3(prefix))
  stop("Invalid connection mode for list_store.")
}

find_store <- function(filename)
{
  if (connected == "local")
    return(find_local(filename))
  if (connected == "cloud")
    return(find_aws_s3(filename))
  stop("Invalid connection mode for find_store.")
}

save_store <- function(data, filename)
{
  if (connected == "local")
    return(save_local(data, filename))
  if (connected == "cloud")
    return(save_aws_s3(data, filename))
  stop("Invalid connection mode for find_store.")
}

load_store <- function(filename, default = NULL)
{
  if (connected == "local")
    return(load_local(filename, default))
  if (connected == "cloud")
    return(load_aws_s3(filename, default))
  stop("Invalid connection mode for load_store.")
}

# -----------------
# ROW / COL SUBSETS
# -----------------

# retrieves a row_axs by category
get_row_axs <- function(cat)
{
  categories[[cat]]$row_axs
}

# retrieves a row axis by category
get_row_axis <- function(cat)
{
  row_axes[[get_row_axs(cat)]]
}

# retrieves a col_axs by category
get_col_axs <- function(cat)
{
  categories[[cat]]$col_axs
}

# retrieves a col axis by category
get_col_axis <- function(cat)
{
  col_axes[[get_col_axs(cat)]]
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
row_subset_lengths <- empty_named_list(row_axs_names)
for (row_axs in row_axs_names)
  row_subset_lengths[[row_axs]] <- summarize_axis(row_axes[[row_axs]])

col_subset_lengths <- empty_named_list(col_axs_names)
for (col_axs in col_axs_names)
  col_subset_lengths[[col_axs]] <- summarize_axis(col_axes[[col_axs]])

# gets a row subset's length
get_row_subset_length <- function(cat, row)
{
  row_axis <- get_row_axis(cat)
  if (row == "Total")
    return(row_axis$length)
  length(row_axis$subsets[[row]])
}

# subsets data by a row subset
subset_by_row <- function(data, cat, row)
{
  row_axis <- get_row_axis(cat)
  stopifnot(nrow(data) == row_axis$length)
  if (row == "Total")
    return(data)
  data[row_axis$subsets[[row]], , drop = FALSE]
}

# gets a col subset's length
get_col_subset_length <- function(cat, col)
{
  col_axis <- get_col_axis(cat)
  if (col == "Total")
    return(col_axis$length)
  length(col_axis$subsets[[col]])
}

# subsets data by a col subset
subset_by_col <- function(data, cat, col)
{
  if (col == "Total")
    return(data)
  col_axis <- get_col_axis(cat)
  data[, col_axis$subsets[[col]], drop = FALSE]
}

cat_f("PREPROCESSING TIME: %.1f (sec)\n", net_time())
