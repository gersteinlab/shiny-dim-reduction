# The purpose of this file is to perform the following operations:
# --declare several utility functions with base R
# --create sdr_config, a list of install settings
# --install missing packages and raise errors if unsuccessful

# -----------------
# UTILITY FUNCTIONS
# -----------------

#' wrapper for cat(sprintf(...))
#'
#' @param ... A dots construct.
cat_f <- function(...)
{
  cat(sprintf(...))
}

#' wrapper for stop(sprintf(...))
#'
#' @param ... A dots construct.
stop_f <- function(...)
{
  stop(sprintf(...))
}

#' whether all elements of x are finite
#' (note: succeeds if x is length 0)
#'
#' @param x An object.
#' @returns A logical of length one.
all_fin <- function(x)
{
  all(is.finite(x))
}

#' whether x is a single number
#'
#' @param x An object.
#' @returns A logical of length one.
is_num <- function(x)
{
  is.numeric(x) && (length(x) == 1)
}

#' whether x is a string
#'
#' @param x An object.
#' @returns A logical of length one.
is_str <- function(x)
{
  is.character(x) && (length(x) == 1)
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
  stopifnot(is_num(n))
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

#' checks loaded packages and sends prompts
#' to prepare for package installation
#'
#' @param pkg_names A character.
prep_pkgs_install <- function(pkg_names)
{
  stopifnot(is.character(pkg_names))
  cat("The following packages are missing and necessary:\n")
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

# -----------------
# CREATE SDR CONFIG
# -----------------

sdr_config <- list("start_time" = Sys.time())

# if you are in the repository base folder ...
if (all(file.exists(
  "app/install.R", "app/app.R"
)))
{
  # pipeline: perform dimensionality reduction
  sdr_config$mode <- "pipeline"
  sdr_config$proj_loc <- getwd()
} else
{
  # if you are in the application folder ...
  stopifnot(file.exists(
    "install.R", "app.R"
  ))

  # local vs cloud application
  if (Sys.getenv("SHINY_PORT") == "")
    sdr_config$mode <- "local"
  else
    sdr_config$mode <- "cloud"
}

# Note: to change sdr_config$mode, source install.R again.

# ------------
# INSTALLATION
# ------------

cat("\nChecking Installation ...\n")

sdr_pkgs_installed <- as.vector(installed.packages()[,1])

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
  install.packages(sdr_pkgs_missing_base, type = "binary", character.only = TRUE)
}

rm(sdr_pkgs_base, sdr_pkgs_missing_base) # comment out for debugging

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
    if ("limma" %in% sdr_pkgs_missing_data)
    {
      cat("Bioconductor may ask if you wish to update other packages.
We recommend you type 'n' and press enter.\n")
      library(BiocManager)
      BiocManager::install("limma")
    }

    rm(all_but_limma)
  }

  rm(sdr_pkgs_data, sdr_pkgs_missing_data) # comment out for debugging
}

rm(prep_pkgs_install, sdr_pkgs_installed) # comment out for debugging

# --------------------------
# PROJECT LOCATION FUNCTIONS
# --------------------------

#' gets the location of a source file, accounting
#' for pipeline vs application differences
#'
#' @param file A string.
get_source_loc <- function(file)
{
  stopifnot(is_str(file))

  if (sdr_config$mode == "pipeline")
  {
    proj_loc <- sdr_config$proj_loc

    a_file <- file.path(proj_loc, "app", file)
    if (file.exists(a_file))
      return(a_file)

    p_file <- file.path(proj_loc, "pipeline", file)
    if (file.exists(p_file))
      return(p_file)

    stop_f("Source file could not be found: %s", file)
  }

  file
}

#' sources a file in the context of this project
#'
#' @param file A string.
source_sdr <- function(file)
{
  # UTF-8 to maximize compatibility (especially with JSON)
  source(get_source_loc(file), encoding = "UTF-8")
}

# ---------------------
# COMPLETE INSTALLATION
# ---------------------

require(shiny)
require(dplyr)

init_time <- time_diff(sdr_config$start_time)

cat("\n")
message("--- SHINY DIMENSIONALITY REDUCTION ---")
message("DEVELOPER: Justin Chang @ Gerstein Lab")
message("ALL R PACKAGES INSTALLED; CHECK README")
cat_f("SDR RUNTIME MODE: %s\n", sdr_config$mode)
cat_f("VALIDATION TIMER: %.2f secs\n", init_time)
# note: prints nothing if proj_loc is NULL
cat_f("PROJECT LOCATION: %s\n", sdr_config$proj_loc)
cat("\n")
