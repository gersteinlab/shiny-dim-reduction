# The purpose of this file is to fulfill the following assumptions:
# 1) that the start time is recorded
# 2) that we have some useful functions needing only base R
# 3) that we know where code is being run from (online app, local app, or reduction workflow)
# 4) that all required packages are installed
# 5) that, if we are running a reduction workflow, functions exist to set the project location
# The project location cannot be stored since a user could simply move the project.
# We also cannot use getwd() because that does not work on an online app.
# source("install.R")

# Should work out of the box (replace with your app directory):
# library(shiny)
# shiny::runApp("C:/Users/Justin Chang/Desktop/exrna_app_3_16/app")

# Key FLAGS for Goal 3:
# - sdr_from_app: whether you are sourcing this file from an application [set first by app.R]
# - sdr_running_local: whether the app is running locally or on a server
# - there are three states: (T, T), (T, F), and (F, *)

# ----------
# START TIME
# ----------

# returns the rounded elapsed system time since 'start'
my_timer <- function(start = 0, num_digits = 4){
  round(as.numeric(Sys.time()) - start, num_digits)
}

install_start <- my_timer()

# ---------------
# BASIC FUNCTIONS
# ---------------

# prints a single line cleanly
print_clean <- function(msg = ""){
  cat(sprintf("%s\n", msg))
}

# prints the result of sprintf cleanly
sprintf_clean <- function(...)
{
  print_clean(sprintf(...))
}

# creates an empty list of length n
len_n_list <- function(n)
{
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

# --------------
# LOCAL / ONLINE
# --------------

shiny_port <- Sys.getenv('SHINY_PORT')
sdr_running_local <- (shiny_port == "")

# sdr_from_app should be the first thing assigned by app.R
if (!exists("sdr_from_app"))
  assign_global("sdr_from_app", FALSE)

# ------------
# INSTALLATION
# ------------

print_clean()
print_clean("Checking Installation ...")

sdr_pkg_names <- empty_named_list(
  "installed", "loaded", "base", "data", "missing_base", "missing_data"
)

sdr_pkg_names$installed <- as.vector(installed.packages()[,1])
sdr_pkg_names$loaded <- .packages()

# a list of all packages necessary for the project / generated applications
sdr_pkg_names$base <- c(
  "shiny"
  , "dplyr"
  , "stringi"
  , "aws.s3"
  , "bcrypt"

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

# a list of extra packages for data exploration and processing
sdr_pkg_names$data <- c(
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

sdr_pkg_names$missing_base <- setdiff(sdr_pkg_names$base, sdr_pkg_names$installed)
sdr_pkg_names$missing_data <- setdiff(sdr_pkg_names$data, sdr_pkg_names$installed)

# if base packages are missing, try to install them
if (length(sdr_pkg_names$missing_base) > 0)
{
  missing_base_str <- paste(sdr_pkg_names$missing_base, collapse = ", ")
  print_clean("
The following packages are missing and necessary:")
  print_clean(missing_base_str)
  if (!sdr_running_local)
    stop("Automatic installation to a server-based host is not supported.")

  missing_base_loaded <- intersect(sdr_pkg_names$loaded, sdr_pkg_names$missing_base)
  if (length(missing_base_loaded) > 0)
    stop(sprintf("Installation cannot proceed because outdated
versions of the following packages are attached:\n%s",
                 paste(missing_base_loaded, collapse = ", ")))

  confirm_base <- readline(prompt = "
Type 'Y' and press enter to install these packages.
Type anything else and press enter to exit. ")

  if (confirm_base != "Y")
    stop("Quitting installation - some packages remain uninstalled.")

  install.packages(sdr_pkg_names$missing_base, type = "binary", character.only = TRUE)
  rm(missing_base_str, missing_base_loaded, confirm_base)
}

if (length(sdr_pkg_names$missing_data) > 0 && sdr_running_local && !sdr_from_app)
{
  print_clean()
  print_clean("Do you wish to generate dimensionality reduction workflows?")
  print_clean("If so, the following packages are missing and necessary:")
  print_clean(paste(sdr_pkg_names$missing_data, collapse = ", "))
  confirm_data <- readline(prompt="
Type 'Y' and press enter to install these additional packages.
Type anything else and press enter to skip this step.
If you are prompted by Bioconductor to install further packages, type 'n' and press enter. ")

  if (confirm_data == "Y")
  {
    missing_data_loaded <- intersect(sdr_pkg_names$loaded, sdr_pkg_names$missing_data)
    if (length(missing_data_loaded) > 0)
      stop(sprintf("Installation cannot proceed because outdated
versions of the following packages are attached:\n%s",
                   paste(missing_data_loaded, collapse = ", ")))

    all_but_limma <- setdiff(sdr_pkg_names$missing_data, "limma")
    install.packages(all_but_limma, type = "binary", character.only = TRUE)
    if ("limma" %in% sdr_pkg_names$missing_data)
    {
      library(BiocManager)
      BiocManager::install("limma")
    }

    rm(missing_data_loaded, all_but_limma)
  }

  rm(confirm_data)
}

# --------------------------
# PROJECT LOCATION FUNCTIONS
# --------------------------

# sets the location of the project folder (R project from shiny-dim-reduction)
# note: only used for the workflow (doesn't work if sdr_from_app is TRUE)
set_project_loc <- function(loc = getwd())
{
  while (!("install.R" %in% list.files(loc)))
    loc <- readline(prompt = "
Error: This location does not contain install.R or app.R.
Please type the location of the project directory and press enter. ")

  assign_global("sdr_project_loc", loc)
}

# gets the absolute path of a file given its relative path to the project
get_project_loc <- function(file)
{
  if (!sdr_from_app && !exists("sdr_project_loc"))
    set_project_loc()
  sprintf("%s/%s", sdr_project_loc, file)
}

# gets the location of a source file, accounting for workflow vs application uses
get_source_loc <- function(file)
{
  if (sdr_from_app)
    return(sprintf("src/%s", file))
  get_project_loc(sprintf("R/%s", file))
}

# sources a file, accounting for workflow vs application uses
source_sdr <- function(file)
{
  source_loc <- get_source_loc(file)
  stopifnot(file.exists(source_loc))
  source(source_loc, encoding = "UTF-8")
}

# displays either the project URL or the port it is running on
display_location <- function()
{
  if (sdr_from_app)
  {
    if (sdr_running_local)
      sprintf_clean("Project Location: %s", getwd())
    else
      sprintf_clean("Shiny Port %s", shiny_port)
  }
  else
  {
    if (!exists("sdr_project_loc"))
      set_project_loc()
    sprintf_clean("Project Location: %s", sdr_project_loc)
  }
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
message("CHANGE PROJECT PATH: set_project_loc()")
message("*** SHINY DIMENSIONALITY REDUCTION ***")
print_clean()
display_location()
sprintf_clean("Installation verification time (seconds): %s", my_timer(install_start))
print_clean()

assign_global("ran_install", TRUE)
