# The purpose of this file is to perform the following operations:
# --declare several utility functions with base R
# --create sdr_config, a list of install settings
# --install missing packages and raise errors if unsuccessful

# ---------------
# FORMAT WRAPPERS
# ---------------

#' wrapper for cat(sprintf(...))
#'
#' @param ... [ellipsis]
cat_f <- function(...)
{
  cat(sprintf(...))
}

#' wrapper for message(sprintf(...))
#'
#' @param ... [ellipsis]
message_f <- function(...)
{
  message(sprintf(...))
}

#' wrapper for stop(sprintf(...))
#'
#' @param ... [ellipsis]
stop_f <- function(...)
{
  stop(sprintf(...))
}

# --------------------
# VALIDATION FUNCTIONS
# --------------------

# note: for each type, we generally do
# is_type, make_type (for complex types)
# such that make_type calls is_type

# additionally, we initially describe types
# in is_type as 'type' objects and refer with
# the type directly in subsequent uses.

#' whether x is an integer of length n
#'
#' @param x [object]
#' @param n [int] not checked
#' @returns [boolean]
is_int <- function(x, n = 1L)
{
  is.integer(x) && length(x) == n
}

#' whether x is a numeric of length n
#'
#' @param x [object]
#' @param n [int] not checked
#' @returns [boolean]
is_num <- function(x, n = 1L)
{
  is.numeric(x) && length(x) == n
}

#' whether x is a character of length n
#'
#' @param x [object]
#' @param n [int] not checked
#' @returns [boolean]
is_str <- function(x, n = 1L)
{
  is.character(x) && length(x) == n
}

#' whether x is a POSIXct object
#'
#' @param x [object]
#' @returns [boolean]
is.POSIXct <- function(x)
{
  "POSIXct" %in% class(x)
}

#' whether all elements of x are finite
#' note: succeeds if x is length 0
#'
#' @param x [object], e.g. vector, matrix, data.frame
#' @returns [boolean]
all_fin <- function(x)
{
  all(is.finite(x))
}

#' whether fun(x) is TRUE for all x in X
#' note: succeeds if x is length 0
#'
#' @param X [vector] not checked
#' @param FUN [function] not checked
#' @returns [boolean]
all_fun_true <- function(X, FUN)
{
  all(vapply(X, FUN, FALSE))
}

#' whether x would contribute to unsafe file paths
#' (see fs::path_sanitize for regex information)
#'
#' @param x [character] not checked
#' @returns [boolean]
makes_unsafe_paths <- function(x)
{
  regex_illegal <- "[/\\?<>\\:*|\":]"
  regex_control <- "[[:cntrl:]]"
  regex_reserved <- "^[.]+$"
  regex_win_files <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  regex_win_tails <- "[. ]+$"

  any(
    x == "",
    grepl(regex_illegal, x),
    grepl(regex_control, x),
    grepl(regex_reserved, x),
    grepl(regex_win_files, x, ignore.case = TRUE),
    grepl(regex_win_tails, x)
  )
}

#' whether x is a 'safe_names' object:
#' a character with unique values that work
#' as names (non-NA, nonempty) and in file paths
#'
#' @param x [object]
#' @returns [boolean]
are_safe_names <- function(x)
{
  is.null(x) || (is.character(x) && !anyDuplicated(x) &&
                   !anyNA(x) && !makes_unsafe_paths(x))
}

#' whether x has names that are safe
#'
#' @param x [object], e.g. vector, data.frame
#' @returns [boolean]
has_safe_names <- function(x)
{
  are_safe_names(names(x))
}

#' whether x is a 'subsets' object
#' given an original set of size n
#'
#' @param x [object]
#' @param n [integer] not checked
#' @returns [boolean]
are_subsets <- function(x, n)
{
  set_seq <- seq_len(n)

  len_n_subset <- function(subset)
  {
    is.integer(subset) &&
      all(subset %in% set_seq)
  }

  is.list(x) && has_safe_names(x) &&
    all_fun_true(x, len_n_subset)
}

#' whether x is a 'metadata' object with row_n rows
#' note: metadata rownames should not be used
#' in agreeing with the tidyverse convention
#'
#' @param x [object]
#' @returns [boolean]
is_metadata <- function(x)
{
  # must be a data.frame
  is.data.frame(x) && has_safe_names(x) &&
    # positive dimensions
    nrow(x) > 0 && ncol(x) > 0 &&
    # each column is a character
    all_fun_true(x, is.character) &&
    # primary key is first column
    !anyDuplicated(x[, 1])
}

#' whether each member of x is a hex string of
#' length 6 (opaque) or 8 (transparent)
#'
#' @param x [object]
#' @returns [logical]
is_color_seq <- function(x)
{
  all(grepl('^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$', x))
}

#' whether x is a 'color_scale' object, aka
#' a named vector of hex values
#'
#' @param x [object]
#' @returns [boolean]
is_color_scale <- function(x)
{
  is.character(x) && has_safe_names(x) && is_color_seq(x)
}

#' whether x is a 'color_scales' object
#'
#' @param x [object]
#' @returns [boolean]
are_color_scales <- function(x)
{
  is.list(x) && has_safe_names(x) &&
    all_fun_true(x, is_color_scale)
}

#' whether color_scales align with metadata
#'
#' @param color_scales [color_scales] not checked
#' @param metadata [metadata] not checked
#' @returns [boolean]
color_scales_match_metadata <- function(color_scales, metadata)
{
  scale_matches_col <- function(cha)
  {
    all(names(color_scales[[cha]]) %in% metadata[[cha]])
  }

  all_fun_true(names(color_scales), scale_matches_col)
}

#' whether x is an 'axis' object
#'
#' @param x [object]
#' @returns [boolean]
is_axis <- function(x)
{
  members <- c(
    "length",
    "metadata",
    "subsets",
    "color_scales",
    "rel_meta"
  )

  is.list(x) && identical(names(x), members) &&
    is_int(x$length) && x$length > 0L &&
    are_subsets(x$subsets, x$length) &&
    is_metadata(x$metadata) &&
    nrow(x$metadata) == x$length &&
    are_color_scales(x$color_scales) &&
    color_scales_match_metadata(
      x$color_scales, x$metadata) &&
    all(x$rel_meta %in% names(x$metadata))
}

#' makes an axis
#'
#' @param metadata [metadata]
#' @param subsets [subsets]
#' @param color_scales [color_scales]
#' @returns [axis]
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
#'
#' @param x [object]
#' @returns [boolean]
are_axes <- function(x)
{
  is.list(x) && has_safe_names(x) &&
    all_fun_true(x, is_axis)
}

#' whether x is a 'category' object
#'
#' @param x [object]
#' @returns [boolean]
is_category <- function(x)
{
  members <- c("row_axs", "col_axs", "note")
  is.list(x) && identical(names(x), members) &&
    is_str(x$row_axs) && is_str(x$col_axs) && is_str(x$note)
}

#' makes a category
#'
#' @param row_axs [string] for a row axis name
#' @param col_axs [string] for a col axis name
#' @param note [string] describing the category
#' @returns [category]
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
#'
#' @param x [object]
#' @returns [boolean]
are_categories <- function(x)
{
  is.list(x) && has_safe_names(x) &&
    all_fun_true(x, is_category)
}

#' whether categories align with row_axes, col_axes
#'
#' @param categories [categories] not checked
#' @param row_axes [row_axes] not checked
#' @param col_axes [col_axes] not checked
#' @returns [boolean]
categories_match_axes <- function(categories, row_axes, col_axes)
{
  category_in_axes <- function(cat)
  {
    categories[[cat]]$row_axs %in% names(row_axes) &&
      categories[[cat]]$col_axs %in% names(col_axes)
  }

  all_fun_true(names(categories), category_in_axes)
}

#' whether x is a 'table' object
#'
#' @param x [object]
#' @returns [boolean]
is_table <- function(x)
{
  # must be a matrix
  is.matrix(x) &&
    # positive dimensions
    nrow(x) > 0 && ncol(x) > 0 &&
    # prevents NA, NaN, Inf, -Inf, non-numerics
    all_fin(x)
}

#' more flexible version of dplyr::between
#'
#' @param x [numeric] not checked
#' @param left [numeric] not checked
#' @param right [numeric] not checked
#' @returns [logical]
vec_between <- function(x, left, right)
{
  (x >= left) & (x <= right)
}

#' whether a perplexity is valid for n samples
#'
#' @param per [numeric] not checked
#' @param n [numeric] not checked
#' @returns [logical]
perplexity_is_valid <- function(per, n)
{
  vec_between(per, 1, (n - 1) / 3)
}

#' the median index of a vector of length n
#'
#' @param n [int] not checked
#' @returns [int]
median_index <- function(n)
{
  ceiling(n / 2) %>% as.integer()
}

# -----------------
# UTILITY FUNCTIONS
# -----------------

#' the number of unique values in x
#'
#' @param x [object]
#' @returns [int]
num_unique <- function(x)
{
  length(unique(x))
}

#' returns (t2 - t1) in seconds
#'
#' @param t1 [POSIXct]
#' @param t2 [POSIXct]
#' @returns [numeric]
time_diff <- function(t1, t2 = Sys.time())
{
  stopifnot(is.POSIXct(t1), is.POSIXct(t2))
  as.numeric(t2 - t1)
}

#' string representation of a vector
#'
#' @param v [vector], e.g. character, numeric
#' @returns [string]
vec_str <- function(v)
{
  stopifnot(is.vector(v))
  paste(v, collapse = ", ")
}

#' creates an empty list of length n
#'
#' @param n [int]
#' @returns [list]
len_n_list <- function(n)
{
  stopifnot(is_int(n))
  vector(mode = "list", length = n)
}

#' creates an empty list with specified safe_names
#' to improve performance (vs expanding a list)
#'
#' @param safe_names [safe_names]
#' @returns [list]
empty_named_list <- function(safe_names)
{
  stopifnot(are_safe_names(safe_names))
  n <- length(safe_names)
  setNames(len_n_list(n), safe_names)
}

#' wrapper for assigning to global env
#'
#' @param name [string]
#' @param value [object]
assign_global <- function(name, value)
{
  stopifnot(is_str(name))
  assign(name, value, envir = .GlobalEnv)
}

# -----------------
# CREATE SDR CONFIG
# -----------------

#' creates an sdr_config object
#'
#' @returns [list]
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
#' @param pkg_names [character]
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
#' @param file [string] not checked
#' @returns [string]
get_project_loc <- function(file)
{
  file.path(sdr_config$path, file)
}

#' gets the location of an application file,
#' accounting for differences in sdr_config$mode
#'
#' @param file [string]
#' @returns [string]
get_app_loc <- function(file)
{
  stopifnot(is_str(file))

  if (sdr_config$mode == "pipeline")
    file <- file.path("app", file) %>% get_project_loc()

  if (!file.exists(file))
    stop_f("Source file could not be found: %s", file)

  file
}

#' sources an application file
#'
#' @param file [string]
source_app <- function(file)
{
  # UTF-8 to maximize compatibility (especially with JSON)
  source(get_app_loc(file), encoding = "UTF-8")
}

# ---------------------
# COMPLETE INSTALLATION
# ---------------------

#' syntactic sugar for time_diff
#'
#' @returns [numeric]
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
