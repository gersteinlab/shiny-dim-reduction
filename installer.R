# The purpose of this file is to install packages 
# and run the offline version of the tool.
# source("~/Justin-Tool/shiny-dim-reduction/installer.R")

# prints a single line cleanly
print_clean <- function(msg){
  cat(sprintf("%s\n", msg))
}

# a list of all libraries necessary for the tool
lib_list <- c(
  "shiny"
  , "dplyr"
  , "stringi"
  , "aws.s3"
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
  "lobstr"
  , "Matrix"
  , "Rtsne"
  , "reticulate"
  , "tfruns"
  , "keras"
  , "dbscan"
  , "umap"
  , "phateR"
  , "readxl"
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
      print_clean("Not all necessary packages have been installed.
                  Quitting installation.")
      invisible()
    }
  }
  
  extra_missing_packages <- extra_lib_list[!(extra_lib_list %in% package_list)]
  
  if (length(extra_missing_packages) > 0)
  {
    print_clean("Do you wish to use this tool's data analysis workflow?")
    print_clean("If so, the following additional packages are necessary:")
    print_clean(paste(extra_missing_packages, collapse = ", "))
    print_clean("Please check that the README is satisfied before proceeding.")
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
  }
  
  run_app <- readline(prompt="
Type 'Y' and press enter to run the app. 
Type anything else and press enter to quit.")
  
  if (run_app == "Y")
  {
    while(TRUE)
    {
      if ("app" %in% list.files())
      {
        break
      }
      else
      {
        correct_dir <- readline(sprintf("
The current directory is %s and it does not contain 'app' as a subfolder.
Please type 'Q' to quit or type in another directory.", getwd()))
        
        if (correct_dir == "Q")
        {
          print_clean("You have quit the installation.")
          invisible()
        }
        else
        {
          tryCatch(
            setwd(correct_dir),
            error = function(e){
              print("Failed to change working directory.")
            }
          )
        }
      }
    }
    
    shiny::runApp("app")
  }
}

# run installation - further notes are located below
print_clean("This tool was developed at the Gerstein Lab from 2019-2020.")
print_clean("If you are running the offline version for the first time, please
            call the installation function by typing 'install()'.")