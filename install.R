# The purpose of this file is to fulfill the following assumptions:
# 1) that we have some basic functions and a start time
# 2) that we know where code is being run from (local/online)
# 3) that all required packages are installed
# 4) that functions exist to set the project location
# 5) that functions exist to set a folder to store workflows in
# source("install.R")

# ---------------
# BASIC FUNCTIONS
# ---------------

# returns the rounded elapsed system time since 'start'
my_timer <- function(start = 0, num_digits = 4){
  round(as.numeric(Sys.time()) - start, num_digits)
}

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
  vector(mode="list", length=n)
}

# creates an empty list
# note: empty_named_list(c("A", "B")) = empty_named_list("A", "B")
empty_named_list <- function(...)
{
  names <- unlist(list(...))
  target <- len_n_list(length(names))
  names(target) <- names
  target
}

install_start <- my_timer()

# --------------
# LOCAL / ONLINE
# --------------

shiny_port <- Sys.getenv('SHINY_PORT')
sdr_running_local <- (shiny_port == "")

# ------------
# INSTALLATION
# ------------

sdr_pkg_names <- empty_named_list("installed", "loaded", "base", "data", "missing_base", "missing_data")

sdr_pkg_names$installed <- as.vector(installed.packages()[,1])
sdr_pkg_names$loaded <- .packages()

# a list of all packages necessary for the project / generated applications
sdr_pkg_names$base <- c(
  "shiny"
  , "dplyr"
  , "stringi"
  , "aws.s3"
  , "bcrypt"
  , "evaluate"

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
  , "tfruns"
  , "keras"
  , "dbscan"
  , "umap"
  , "phateR"
  , "limma"
)

sdr_pkg_names$missing_base <- setdiff(sdr_pkg_names$base, sdr_pkg_names$installed)
sdr_pkg_names$missing_data <- setdiff(sdr_pkg_names$data, sdr_pkg_names$installed)

if (length(sdr_pkg_names$missing_base) > 0)
{
  if (!sdr_running_local)
    stop("An essential package is missing and cannot be installed to this host.")

  print_clean("The following R packages are missing and necessary:")
  print_clean(paste(sdr_pkg_names$missing_base, collapse = ", "))
  confirm_base <- readline(prompt = "
Type 'Y' and press enter to install these packages.
Type anything else and press enter to exit. ")

  if (confirm_base == "Y")
  {
    if (length(intersect(sdr_pkg_names$loaded, sdr_pkg_names$missing_base)) > 0)
      stop("An essential package is missing and cannot be installed since an old version is attached.")
    install.packages(sdr_pkg_names$missing_base, type = "binary", character.only = TRUE)
  }

  else
    stop("Quitting installation - some packages remain uninstalled.")
}

if (length(sdr_pkg_names$missing_data) > 0 && sdr_running_local)
{
  print_clean()
  print_clean("Do you wish to generate dimensionality reduction workflows?")
  print_clean("If so, the following packages are missing and necessary:")
  print_clean(paste(sdr_pkg_names$missing_data, collapse = ", "))
  confirm_data <- readline(prompt="
To proceed with additional package installation, type 'Y' and press enter to continue.
Type anything else and press enter to skip this step.
If you are prompted by Bioconductor to install further packages, type 'n' and press enter. ")

  if (confirm_data == "Y")
  {
    if (length(intersect(sdr_pkg_names$loaded, sdr_pkg_names$missing_data)) > 0)
      stop("An essential package is missing and cannot be installed since an old version is attached.")
    install.packages(setdiff(sdr_pkg_names$missing_data, "limma"), type = "binary", character.only = TRUE)
    if ("limma" %in% sdr_pkg_names$missing_data)
      BiocManager::install("limma")
  }
}

# ----------------
# PROJECT LOCATION
# ----------------

# sets the location of the project folder (R project from shiny-dim-reduction)
set_project_loc <- function(loc = getwd())
{
  if (!sdr_running_local)
  {
    assign("sdr_project_loc", ".", envir = .GlobalEnv)
    invisible()
  }

  while (!("install.R" %in% list.files(loc)))
    loc <- readline(prompt = "
Error: install.R is not contained in this location.
Please type the location of the project directory and press enter. ")
  assign("sdr_project_loc", loc, envir = .GlobalEnv)
}

# gets the absolute path of a file given its relative path to the project
get_project_loc <- function(file)
{
  if(!exists(sdr_project_loc))
    set_project_loc()
  sprintf("%s/%s", sdr_project_loc, file)
}

# sources a file given its relative path to the R folder of the project
source_sdr <- function(file)
{
  if (file == "install.R")
  {
    source("install.R")
    invisible()
  }

  source_loc <- get_project_loc(sprintf("R/%s", file))
  stopifnot(file.exists(source_loc))
  source(source_loc, encoding = "UTF-8")
}

if (!exists("sdr_project_loc"))
  set_project_loc()

print_clean()
message("*** SHINY DIMENSIONALITY REDUCTION ***")
message("DEVELOPER: Justin Chang @ Gerstein Lab")
message("ALL R PACKAGES INSTALLED; CHECK README")
message("CHANGE PROJECT PATH: set_project_loc()")
message("SET WORKFLOW ROOT: set_workflow_root()")
message("*** SHINY DIMENSIONALITY REDUCTION ***")
print_clean()

if (sdr_running_local)
{
  sprintf_clean("Project location: %s", sdr_project_loc)
} else
{
  print_clean("Project location: Shiny Port %s", shiny_port)
}

ran_install <- TRUE
sprintf_clean("Installation time (seconds): %s", my_timer(install_start))


