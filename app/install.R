# The purpose of this file is to perform the following operations:
# --declare several utility functions with base R
# --create sdr_config, a list of install settings
# --install missing packages and raise errors if unsuccessful

# ---------------
# FORMAT WRAPPERS
# ---------------

#' wrapper for cat(sprintf(...))
#'
#' @param ... A dots construct.
cat_f <- function(...)
{
  cat(sprintf(...))
}

#' wrapper for message(sprintf(...))
#'
#' @param ... A dots construct.
message_f <- function(...)
{
  message(sprintf(...))
}

#' wrapper for stop(sprintf(...))
#'
#' @param ... A dots construct.
stop_f <- function(...)
{
  stop(sprintf(...))
}

# --------------------
# VALIDATION FUNCTIONS
# --------------------

# note: for each type, we generally do
# is_type, make_type (for complex types)
# and make_type calls is_type

#' whether x is an integer of length n
#'
#' @param x An object.
#' @param n An integer (not checked).
#' @returns TRUE or FALSE.
is_int <- function(x, n = 1L)
{
  is.integer(x) && length(x) == n
}

#' whether x is a numeric of length n
#'
#' @param x An object.
#' @param n An integer (not checked).
#' @returns TRUE or FALSE.
is_num <- function(x, n = 1L)
{
  is.numeric(x) && length(x) == n
}

#' whether x is a character of length n
#'
#' @param x An object.
#' @param n An integer (not checked).
#' @returns TRUE or FALSE.
is_str <- function(x, n = 1L)
{
  is.character(x) && length(x) == n
}

#' whether all elements of x are finite
#' note: succeeds if x is length 0
#'
#' @param x An object (e.g. vector, matrix).
#' @returns TRUE or FALSE.
all_fin <- function(x)
{
  all(is.finite(x))
}

#' whether all elements of x are non-NA
#' note: succeeds if x is length 0
#'
#' @param x An object (e.g. vector, matrix).
#' @returns TRUE or FALSE.
none_na <- function(x)
{
  !any(is.na(x))
}

#' whether fun(x) is TRUE for all x in X
#' note: succeeds if x is length 0
#'
#' @param X A vector (not checked).
#' @returns TRUE or FALSE.
all_fun_true <- function(X, FUN)
{
  all(vapply(X, FUN, FALSE))
}

#' whether x contains only unique values
#' note: for any integer n, "!(n)" is
#' TRUE if n = 0, FALSE otherwise
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_unique <- function(x)
{
  # aka: length(unique(x)) == length(x)
  # aka: !any(duplicated(x))
  !anyDuplicated(x)
}

#' whether x is a character with unique
#' values not equal to NA
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_list_names <- function(x)
{
  is.character(x) && is_unique(x) && none_na(x)
}

#' whether x is a 'subsets' object
#' given an original set of size n
#' note: succeeds if x is list()
#'
#' @param x An object.
#' @param n An integer (not checked).
#' @returns TRUE or FALSE.
are_subsets <- function(x, n)
{
  set_seq <- seq_len(n)

  len_n_subset <- function(subset)
  {
    is.integer(subset) && all(subset %in% set_seq)
  }

  is.list(x) && all_fun_true(x, len_n_subset)
}

#' whether x is a 'groups' object
#' note: succeeds if x is list()
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_groups <- function(x)
{
  is.list(x) && all_fun_true(x, is.character)
}

#' whether x is a 'metadata' object with row_n rows
#' note: metadata rownames should not be used
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_metadata <- function(x)
{
  # must be a data.frame
  is.data.frame(x) &&
    # positive dimensions
    nrow(x) > 0 && ncol(x) > 0 &&
    # each column is a character
    all_fun_true(x, is.character) &&
    # primary key is first column
    is_unique(x[, 1])
}

#' whether x is a 'color vector' object
#' note: succeeds if x is character()
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_color_vector <- function(x)
{
  is.character(x) &&
    all(grepl('^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$', x))
}

#' whether x is a 'color scale' object
#' with character names
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_color_scale <- function(x)
{
  is_color_vector(x) &&
    is.character(names(x)) && is_unique(names(x)) && none_na(names(x))
}

#' whether x is a 'color scales' object
#' note: succeeds if x is list()
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_color_scales <- function(x)
{
  is.list(x) && all_fun_true(x, is_color_scale)
}

#' whether color_scales are compatible with metadata
#'
#' @param color_scales A color_scales object (not checked).
#' @param metadata A metadata object (not checked).
#' @returns TRUE or FALSE.
color_scales_match_metadata <- function(color_scales, metadata)
{
  color_scale_match_meta_col <- function(cha)
  {
    all(names(color_scales[[cha]]) %in% metadata[[cha]])
  }

  all_fun_true(names(color_scales), color_scale_match_meta_col)
}

# the expected members of an axis object
axis_members <- c(
  "length",
  "metadata",
  "subsets",
  "color_scales",
  "rel_meta"
)

#' whether x is an 'axis' object
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_axis <- function(x)
{
  is.list(x) && identical(names(x), axis_members) &&
    is_int(x$length) && x$length > 0L &&
    are_subsets(x$subsets, x$length) &&
    is_metadata(x$metadata) &&
    nrow(x$metadata) == x$length &&
    are_color_scales(x$color_scales) &&
    color_scales_match_metadata(x$color_scales, x$metadata) &&
    all(x$rel_meta %in% names(x$metadata))
}

#' makes an axis object
#'
#' @param metadata A metadata object.
#' @param subsets A subsets object.
#' @param color_scales A color_scales object.
#' @returns An axis object if successful.
make_axis <- function(metadata, subsets, color_scales)
{
  result <- list(
    "length" = nrow(metadata),
    "metadata" = metadata,
    "subsets" = subsets,
    "color_scales" = color_scales
  )

  stopifnot(is_axis(result))

  result
}

#' whether x is an 'axes' object
#' note: succeeds if x is list()
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_axes <- function(x)
{
  is.list(x) && all_fun_true(x, is_axis)
}

#' whether x is a 'category' object
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_category <- function(x)
{
  members <- c("row_axs", "col_axs", "note")
  is.list(x) && identical(names(x), members) &&
    is_str(x$row_axs) && is_str(x$col_axs) && is_str(x$note)
}

#' makes a category object
#'
#' @param row_axs A string for a row axis name.
#' @param col_axs A string for a col axis name.
#' @returns A category object if successful.
make_category <- function(row_axs, col_axs, note = "")
{
  result <- list(
    "row_axs" = row_axs,
    "col_axs" = col_axs,
    "note" = note
  )

  stopifnot(is_category(result))

  result
}

#' whether x is a 'categories' object
#' note: succeeds if x is list()
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_categories <- function(x)
{
  is.list(x) && all_fun_true(x, is_category)
}

#' whether categories are compatible with row_axes, col_axes
#'
#' @param categories A categories object (not checked).
#' @param row_axes A row axis object (not checked).
#' @param col_axes A col axis object (not checked).
#' @returns TRUE or FALSE.
categories_match_axes <- function(categories, row_axes, col_axes)
{
  category_match_axes <- function(cat)
  {
    categories[[cat]]$row_axs %in% names(row_axes) &&
      categories[[cat]]$col_axs %in% names(col_axes)
  }

  all_fun_true(names(categories), category_match_axes)
}

#' whether groups are compatible with categories
#'
#' @param groups A groups object (not checked).
#' @param categories A categories object (not checked).
#' @returns TRUE or FALSE.
groups_match_categories <- function(groups, categories)
{
  group_matches_categories <- function(group)
  {
    all(group %in% names(categories))
  }

  all_fun_true(groups, group_matches_categories)
}

#' whether x is a table object
#'
#' @param x An object.
#' @returns TRUE or FALSE.
is_table <- function(x)
{
  # must be a matrix
  is.matrix(x) &&
    # positive dimensions
    nrow(x) > 0 && ncol(x) > 0 &&
    # prevents NA, NaN, Inf, -Inf, non-numerics
    all_fin(x) &&
    # no rownames (tidyverse convention)
    is.null(rownames(x))
}

#' whether table has corresponding row / col counts
#'
#' @param table A table (not checked).
#' @param row_n An integer (not checked).
#' @param col_n An integer (not checked).
#' @returns TRUE or FALSE.
table_has_dim_n <- function(table, row_n, col_n)
{
  nrow(table) == row_n && ncol(table) == col_n
}

#' A useful utility function, similar to dplyr::between
#'
#' @param x A numeric (not checked).
#' @param left A numeric (not checked).
#' @param right A numeric (not checked).
#' @returns A logical.
vec_between <- function(x, left, right)
{
  (x >= left) & (x <= right)
}

#' Whether a perplexity is valid given the number of rows
#'
#' @param per A numeric (not checked).
perplexity_is_valid <- function(per, row_n)
{
  vec_between(per, 1, (row_n - 1) / 3)
}

# -----------------
# UTILITY FUNCTIONS
# -----------------

#' the number of unique values in x
#'
#' @param x An object.
#' @returns An integer.
num_unique <- function(x)
{
  length(unique(x))
}

#' assert that x has at most n unique values
#'
#' @param x An object.
#' @returns TRUE or FALSE.
has_n_unique <- function(x, n)
{
  tryCatch(
    return(unique(x, nmax = n) <= n),
    error = function(e) return(FALSE),
    warning = function(e) return(FALSE)
  )
}

#' returns (time t2 - time t1) in seconds
#'
#' @param t1 A POSIXct.
#' @param t2 A POSIXct.
#' @returns A numeric.
time_diff <- function(t1, t2 = Sys.time())
{
  stopifnot(
    "POSIXct" %in% class(t1),
    "POSIXct" %in% class(t2)
  )
  as.numeric(t2 - t1)
}

#' converts a vector to a string
#'
#' @param v A vector.
#' @returns A string.
vec_str <- function(v)
{
  stopifnot(is.vector(v))
  paste(v, collapse = ", ")
}

#' creates an empty list of length n
#'
#' @param n An integer.
#' @returns A list.
len_n_list <- function(n)
{
  stopifnot(is_int(n))
  vector(mode = "list", length = n)
}

#' creates an empty list with specified list_names
#' to improve performance (vs expanding a list)
#'
#' @param list_names A character.
#' @returns A list.
empty_named_list <- function(list_names)
{
  stopifnot(is.character(list_names))
  setNames(len_n_list(length(list_names)), list_names)
}

#' wrapper for assigning to global env
#'
#' @param name A string.
#' @param value An object.
assign_global <- function(name, value)
{
  stopifnot(is_str(name))
  assign(name, value, envir = .GlobalEnv)
}

# -----------------
# CREATE SDR CONFIG
# -----------------

#' creates an sdr_config object
#' @returns A list.
create_sdr_config <- function()
{
  config <- list("start_time" = Sys.time())
  app_files <- c("install.R", "app.R")

  # if you are in the repository base folder ...
  if (all(app_files %in% list.files("app")))
  {
    # pipeline: perform dimensionality reduction
    config$mode <- "pipeline"
    config$path <- getwd()
  }
  else
  {
    # if you are in the application folder ...
    stopifnot(file.exists(app_files))

    # local vs cloud application
    if (Sys.getenv("SHINY_PORT") == "")
      config$mode <- "local"
    else
      config$mode <- "cloud"
  }

  config
}

# Note: to update sdr_config, source install.R again.
gc()
sdr_config <- create_sdr_config()
rm(create_sdr_config)

# ------------
# INSTALLATION
# ------------

#' checks loaded packages and sends prompts
#' to prepare for package installation
#'
#' @param pkg_names A character.
prep_pkgs_install <- function(pkg_names)
{
  stopifnot(is.character(pkg_names))
  cat("\nThe following packages are missing and necessary:\n")
  cat_f("%s\n", vec_str(pkg_names))
  loaded_pkg_names <- intersect(.packages(), pkg_names)

  if (length(loaded_pkg_names) > 0)
    stop_f("Installation cannot proceed because outdated
versions of the following packages are attached:\n%s",
           vec_str(loaded_pkg_names))

  confirm_install <- readline(prompt = "
To install these packages, type 'y' and press enter.
To exit, type anything else and press enter. ")

  if (confirm_install != "y")
    stop("Quitting installation - some packages remain uninstalled.")
}

sdr_pkgs_installed <- installed.packages()[,1]
stopifnot(is.character(sdr_pkgs_installed))

# a list of packages necessary for application code
sdr_pkgs_base <- c(
  "shiny"
  , "dplyr"
  , "stringi"
  , "aws.s3"
  , "bcrypt"
  , "hash"

  , "shinydashboard"
  , "shinyjs"
  , "shinycssloaders"
  , "shinyWidgets"
  , "viridis"
  , "ggplot2"
  , "plotly"
  , "UpSetR"
  , "VennDiagram"
  , "beeswarm"
  , "heatmaply"
  , "DT"
)

sdr_pkgs_missing_base <- setdiff(sdr_pkgs_base, sdr_pkgs_installed)

# if base packages are missing, try to install them
if (length(sdr_pkgs_missing_base) > 0)
{
  if (sdr_config$mode == "cloud")
    stop("Automated installation is not supported for cloud applications.")

  cat("\nAnalyzing application packages ...\n")
  prep_pkgs_install(sdr_pkgs_missing_base)
  install.packages(sdr_pkgs_missing_base,
                   type = "binary", character.only = TRUE)
}

rm(sdr_pkgs_base, sdr_pkgs_missing_base)

if (sdr_config$mode == "pipeline")
{
  # a list of extra packages necessary for reduction code
  sdr_pkgs_data <- c(
    "Matrix"
    , "BiocManager"
    , "Rtsne"
    , "reticulate"
    , "tensorflow"
    , "keras"
    , "umap"
    , "phateR"
    , "limma"
  )

  sdr_pkgs_missing_data <- setdiff(sdr_pkgs_data, sdr_pkgs_installed)

  # if data packages are missing, try to install them
  if (length(sdr_pkgs_missing_data) > 0)
  {
    cat("\nAnalyzing workflow packages ...\n")
    prep_pkgs_install(sdr_pkgs_missing_data)
    all_but_limma <- setdiff(sdr_pkgs_missing_data, "limma")
    install.packages(all_but_limma, type = "binary", character.only = TRUE)

    if ("limma" %in% sdr_pkgs_missing_data) {
      readline(prompt ="
You are missing 'limma', which is managed by Bioconductor.
If Bioconductor later asks to update other packages '[a/s/n]',
we recommend you type 'n' and press enter.
Press enter to proceed.\n")
      BiocManager::install("limma")
    }

    rm(all_but_limma)
  }

  rm(sdr_pkgs_data, sdr_pkgs_missing_data)
}

rm(prep_pkgs_install, sdr_pkgs_installed)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

# --------------------------
# PROJECT LOCATION FUNCTIONS
# --------------------------

#' gets the location of a project file
#' note: can ONLY be used in pipeline mode
#'
#' @param file A string.
get_project_loc <- function(file)
{
  file.path(sdr_config$path, file)
}

#' gets the location of an application file, accounting
#' for the values of sdr_config$mode
#'
#' @param file A string.
get_app_loc <- function(file)
{
  stopifnot(is_str(file))

  if (sdr_config$mode == "pipeline")
    file <- file.path("app", file) %>% get_project_loc()

  if (file.exists(file))
    return(file)

  stop_f("Source file could not be found: %s", file)
}

#' sources a file in "app/" in the context of various use cases
#'
#' @param file A string.
source_app <- function(file)
{
  # UTF-8 to maximize compatibility (especially with JSON)
  source(get_app_loc(file), encoding = "UTF-8")
}

# ---------------------
# COMPLETE INSTALLATION
# ---------------------

#' Syntactic sugar for time_diff.
#'
#' @returns A numeric.
net_time <- function() {
  time_diff(sdr_config$start_time)
}

message("\n--- SHINY DIMENSIONALITY REDUCTION ---")
message("DEVELOPER: Justin Chang @ Gerstein Lab")
message("ALL R PACKAGES INSTALLED; CHECK README")
cat_f("\nSDR MODE: %s\n", sdr_config$mode)
cat_f("SDR TIME: %.1f (sec)\n", net_time())
# note: prints nothing if path is NULL
cat_f("SDR PATH: %s\n\n", sdr_config$path)
