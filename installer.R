# The purpose of this file is to install packages
# and run the offline version of the tool.
# source("installer.R", encoding="UTF-8")

# prints a single line cleanly
print_clean <- function(msg){
  cat(sprintf("%s\n", msg))
}

# prints the result of sprintf cleanly
sprintf_clean <- function(...)
{
  print_clean(sprintf(...))
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

# run installation - further notes are located below
print_clean("This tool was developed at the Gerstein Lab from 2019-2020.")
print_clean("You can call the installation function by typing 'install()'.")
