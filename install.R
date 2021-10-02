# The purpose of this file is to install packages necessary for using this project.
# source("install.R")

# ---------------
# BASIC FUNCTIONS
# ---------------

# prints a single line cleanly
print_clean <- function(msg){
  cat(sprintf("%s\n", msg))
}

# prints the result of sprintf cleanly
sprintf_clean <- function(...)
{
  print_clean(sprintf(...))
}

# returns the rounded elapsed system time since 'start'
my_timer <- function(start = 0, num_digits = 4){
  round(as.numeric(Sys.time()) - start, num_digits)
}

# sets the location of the workflow folder (R project from shiny-dim-reduction)
set_workflow_loc <- function(loc = getwd())
{
  assign("sdr_workflow_loc", loc, envir = .GlobalEnv)
}

# gets the absolute path of a file given its location in the workflow folder
get_workflow_loc <- function(file)
{
  stopifnot(exists(sdr_workflow_loc))
  sprintf("%s/%s", sdr_workflow_loc, file)
}

# sources a file located
source_from_workflow <- function(file)
{
  if (!exists(sdr_workflow_loc))
    source(sprintf("%s/R/%s", workflow_loc, file), encoding = "UTF-8")
}

# a list of all libraries necessary for the tool
lib_list <- c(
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

# a list of extra libraries for data exploration and processing
extra_lib_list <- c(
  "Matrix"
  , "BiocManager"
  , "Rtsne"
  , "reticulate"
  , "tfruns"
  , "keras"
  , "dbscan"
  , "umap"
  , "phateR"
)

# installation function
install <- function(){
  package_list <- installed.packages()[,1]
  missing_packages <- lib_list[!(lib_list %in% package_list)]

  if (length(missing_packages) > 0)
  {
    print_clean("The following R packages are missing:")
    print_clean(paste(lib_list, collapse = ", "))
    confirm <- readline(prompt = "
Type 'Y' and press enter to install these packages.
Type anything else and press enter to quit.")

    if (confirm == "Y")
    {
      for (lib in missing_packages)
      {
        install.packages(lib, type="binary", character.only=TRUE)
      }
    }
    else
    {
      print_clean("Quitting installation - some packages remain uninstalled.")
      invisible()
    }
  }

  extra_missing_packages <- extra_lib_list[!(extra_lib_list %in% package_list)]

  if (length(extra_missing_packages) > 0)
  {
    print_clean("Do you wish to use this tool's data analysis workflow?")
    print_clean("If so, the following additional packages are necessary:")
    print_clean(paste(extra_missing_packages, collapse = ", "))
    extras <- readline(prompt="
To proceed with additional package installation, type 'Y' and press enter to continue.
Type anything else and press enter to skip this step.")

    if (extras == "Y")
    {
      for (lib in extra_missing_packages)
      {
        install.packages(lib, type="binary", character.only=TRUE)
      }
    }

    limma_install <- readline(prompt="
Type 'Y' and press enter to install limma for quantile normalization.
Type anything else and press enter to skip this step.")
    if (limma_install == "Y")
    {
      BiocManager::install("limma")
    }
  }

  print_clean("Please check that the README is satisfied.")
  print_clean("All necessary R packages have been installed.")
}

set_workflow_loc()

message("*** SHINY DIMENSIONALITY REDUCTION ***")
message("DEVELOPER: Justin Chang @ Gerstein Lab")
message("BEGIN OR CHECK INSTALLATION: install()")
message("CHANGE PROJECT PATH: set_project_loc()")
message("ADD / OPEN WORKFLOW: select_workflow()")
message("*** SHINY DIMENSIONALITY REDUCTION ***")
print_clean(sdr_workflow_loc)


