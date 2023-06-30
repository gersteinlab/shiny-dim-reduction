# The purpose of this file is to name, initialize, and set roles for reduction parameters,
# while also enabling the naming, storing, and loading of reduction files.

if (!exists("sdr_config"))
  source("app/install.R")

source_sdr("storage.R")

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
    is_local_store(x$local_store) && is_cloud_store(x$cloud_store)
}

# ensure the data for the application is valid
app_data_loc <- get_app_loc("app_data.rds")
while (!file.exists(app_data_loc))
{
  if (sdr_config$mode == "cloud")
    stop_f("Missing %s localization is not supported for cloud applications.", app_data_loc)
  confirm_install <- readline(prompt = "
To install these packages, type 'y' and press enter.
To exit, type anything else and press enter. ")
}

app_data <- readRDS(app_data_loc)
if (!is_app_data(app_data))
  stop("The application data ('app_data.rds') is invalid.
Please delete 'app_data.rds' and rerun the application.")
assign_global("app_data", app_data)

for (dep in names(app_data))
  assign_global(dep, app_data[[dep]])

assign_global("cat_names", names(categories))

# retrieves a row axis by category
get_row_axis <- function(cat)
{
  row_axs <- categories[[cat]]$row_axs
  row_axes[[row_axs]]
}

# retrieves a row subset by category
get_row_subset <- function(cat, row)
{
  row_axis <- get_row_axis(cat)
  row_axis$subsets[[row]]
}

# subsets data by a row subset
subset_by_row <- function(data, cat, row)
{
  if (row == "Total")
    return(data)
  data[get_row_subset(cat, row), , drop = FALSE]
}

# retrieves a col axis by category
get_col_axis <- function(cat)
{
  col_axs <- categories[[cat]]$col_axs
  col_axes[[col_axs]]
}

# retrieves a col subset by category
get_col_subset <- function(cat, col)
{
  col_axis <- get_col_axis(cat)
  col_axis$subsets[[col]]
}

# subsets data by a col subset
subset_by_col <- function(data, cat, col)
{
  if (col == "Total")
    return(data)
  data[, get_col_subset(cat, col), drop = FALSE]
}

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
