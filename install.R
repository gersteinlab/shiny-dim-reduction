# The purpose of this file is to fulfill the following assumptions:
# 1) that the start time is recorded
# 2) that we have some useful functions needing only base R
# 3) that we know where code is being run from (local/online, app/source)
# 4) that all required packages are installed
# 5) that functions exist to set the project location
# The project location cannot be stored since a user could simply move the project.
# We also cannot use getwd() because it's possible the user has performed setwd().
# source("install.R")

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

# --------------
# LOCAL / ONLINE
# --------------

shiny_port <- Sys.getenv('SHINY_PORT')
sdr_running_local <- (shiny_port == "")

if (!exists("sdr_from_app"))
  sdr_from_app <- FALSE

# ------------
# INSTALLATION
# ------------

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
  , "tfruns"
  , "keras"
  , "dbscan"
  , "umap"
  , "phateR"
  , "limma"
)

sdr_pkg_names$missing_base <- setdiff(sdr_pkg_names$base, sdr_pkg_names$installed)
sdr_pkg_names$missing_data <- setdiff(sdr_pkg_names$data, sdr_pkg_names$installed)

print(sdr_from_app)

if (length(sdr_pkg_names$missing_base) > 0)
{
  if (!sdr_running_local)
    stop(sprintf("The following packages are missing and cannot be installed to this host: %s",
                 sdr_pkg_names$missing_base))

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

  rm(confirm_base)
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
    if (length(intersect(sdr_pkg_names$loaded, sdr_pkg_names$missing_data)) > 0)
      stop("An essential package is missing and cannot be installed since an old version is attached.")
    install.packages(setdiff(sdr_pkg_names$missing_data, "limma"), type = "binary", character.only = TRUE)
    if ("limma" %in% sdr_pkg_names$missing_data)
      BiocManager::install("limma")
  }

  rm(confirm_data)
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

  while (!("install.R" %in% list.files(loc)) || ("app.R" %in% list.files(loc)))
    loc <- readline(prompt = "
Error: install.R is not contained in this location.
Please type the location of the project directory and press enter. ")

  assign("sdr_project_loc", loc, envir = .GlobalEnv)
}

# gets the absolute path of a file given its relative path to the project
get_project_loc <- function(file)
{
  if(!exists("sdr_project_loc"))
    set_project_loc()
  sprintf("%s/%s", sdr_project_loc, file)
}

# sources a file given its relative path to the R folder of the project
source_sdr <- function(file)
{
  source_loc <- sprintf("src/%s", file)
  if (!sdr_from_app)
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


