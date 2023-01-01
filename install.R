# The purpose of this file is to perform the following operations:
# --confirm that the current working directory contains install.R
# --declare several utility functions with base R
# --create sdr_config, a list of install settings
# ----start_time: when install.R was first sourced
# ----mode: one of c("local", "online", "workflow")
# ------local: local visualization app
# ------online: online visualization app
# ------workflow: perform dimensionality reduction
# ----proj_loc: the path of the project (Shiny port if online)
# --install missing packages and raise errors if unsuccessful
# Note: to make "local" or "online" your mode, sdr_from_app must
# exist before we run source("install.R"). If the code below runs
# without raising an error, sdr_from_app will no longer exist. To
# change the mode, source install.R with / without sdr_from_app.

stopifnot(file.exists("install.R"))

# -----------------
# UTILITY FUNCTIONS
# -----------------

# whether x is an integer
is_int <- function(x)
{
  is.integer(x) && length(x) == 1
}

# whether x is a number
is_num <- function(x)
{
  is.numeric(x) && length(x) == 1
}

# whether x is a string
is_str <- function(x)
{
  is.character(x) && length(x) == 1
}

# returns the rounded elapsed system time since 'start'
my_timer <- function(start = 0, num_digits = 4L){
  stopifnot(is_num(start), is_int(num_digits))
  round(as.numeric(Sys.time()) - start, num_digits)
}

# prints cleanly and adds a new line
print_clean <- function(msg = ""){
  stopifnot(is_str(msg))
  cat(sprintf("%s\n", msg))
}

# wrapper for print_clean and sprintf
sprintf_clean <- function(...)
{
  print_clean(sprintf(...))
}

# creates an empty list of integer length n
len_n_list <- function(n)
{
  stopifnot(is_int(n))
  vector(mode = "list", length = n)
}

# creates an empty list, has faster performance than expanding list
# note: empty_named_list(c("A", "B")) = empty_named_list("A", "B")
empty_named_list <- function(...)
{
  names <- unlist(list(...))
  target <- len_n_list(length(names))
  names(target) <- names
  target
}

# assigns the given value to a global variable with the given name
assign_global <- function(name, value)
{
  assign(name, value, envir = .GlobalEnv)
}

# an empty data frame used for various purposes
empty_df <- data.frame(matrix(nrow = 0, ncol = 1))
colnames(empty_df) <- "Unknown"

# prepares to install each package in pkg_vec
prep_pkgs_install <- function(pkg_vec)
{
  print_clean("The following packages are missing and necessary:")
  print_clean(paste(pkg_vec, collapse = ", "))
  pkg_vec_loaded <- intersect(.packages(), pkg_vec)

  if (length(pkg_vec_loaded) > 0)
    stop(sprintf("Installation cannot proceed because outdated
versions of the following packages are attached:\n%s",
                 paste(pkg_vec_loaded, collapse = ", ")))

  confirm_install <- readline(prompt = "
Type 'Y' and press enter to install these packages.
Type anything else and press enter to exit. ")

  if (confirm_install != "Y")
    stop("Quitting installation - some packages remain uninstalled.")
}

# -----------------
# CREATE SDR CONFIG
# -----------------

sdr_config <- list(
  "start_time" = my_timer(),
  "proj_loc" = Sys.getenv('SHINY_PORT')
)

if (exists("sdr_from_app"))
{
  rm(sdr_from_app, envir = .GlobalEnv)
  if (sdr_config$proj_loc == "")
  {
    sdr_config$mode <- "local"
    sdr_config$proj_loc <- getwd()
  }
  else
    sdr_config$mode <- "online"
} else
{
  sdr_config$mode <- "workflow"
  sdr_config$proj_loc <- getwd()
}

# ------------
# INSTALLATION
# ------------

print_clean()
print_clean("Checking Installation ...")

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
  if (sdr_config$mode == "online")
    stop("Automatic installation to an online application is not supported.")

  print_clean()
  print_clean("Analyzing application packages ...")
  prep_pkgs_install(sdr_pkgs_missing_base)
  install.packages(sdr_pkgs_missing_base, type = "binary", character.only = TRUE)
}

rm(sdr_pkgs_base, sdr_pkgs_missing_base) # comment out for debugging

if (sdr_config$mode == "workflow")
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
    print_clean()
    print_clean("Analyzing workflow packages ...")
    prep_pkgs_install(sdr_pkg_names$missing_data)

    all_but_limma <- setdiff(sdr_pkg_names$missing_data, "limma")
    install.packages(all_but_limma, type = "binary", character.only = TRUE)
    if ("limma" %in% sdr_pkg_names$missing_data)
    {
      print_clean("Bioconductor may ask if you wish to update other packages.
We recommend you type 'n' and press enter.")
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

# gets the absolute path of a file given its relative path to the project
get_project_loc <- function(file)
{
  stopifnot(sdr_config$mode == "workflow", is_str(file))
  file.path(sdr_config$proj_loc, file)
}

# gets the location of a source file (does NOT work on app.R, install.R)
get_source_loc <- function(file)
{
  if (sdr_config$mode == "workflow")
    return(get_project_loc(sprintf("R/%s", file)))
  stopifnot(is_str(file))
  sprintf("src/%s", file)
}

# sources a file, accounting for workflow vs application uses
source_sdr <- function(file)
{
  source(get_source_loc(file), encoding = "UTF-8")
}

# ---------------------
# COMPLETE INSTALLATION
# ---------------------

require(shiny)
require(dplyr)

print_clean()
message("*** SHINY DIMENSIONALITY REDUCTION ***")
message("DEVELOPER: Justin Chang @ Gerstein Lab")
message("ALL R PACKAGES INSTALLED; CHECK README")
print_clean()
sprintf_clean("Project Location: %s", sdr_config$proj_loc)
sprintf_clean("Initialization time (seconds): %s",
              my_timer(sdr_config$start_time))
print_clean()
