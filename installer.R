# The purpose of this file is to install packages 
# and run the offline version of the tool.
# source("~/Justin-Tool/installer.R")

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
  print("You are about to install a Dimensionality Reduction Tool.")
  print("Developed at Gerstein Lab from 2019-2020.")
  print("The following R packages are necessary:")
  print(paste(lib_list, collapse = ", "))
  print("If a package is not already installed, it will be installed.")
  confirm <- readline(prompt = "
Please type 'Y' and press enter to continue. 
Type anything else and press enter to quit.")
  
  if (confirm == "Y")
  {
    for (lib in lib_list)
    {
      if (!require(lib, character.only=TRUE))
      {
        print(sprintf("Installing %s ...", lib))
        install.packages(lib, type="binary", character.only=TRUE)
        library(lib, character.only=TRUE)
      }
    }
    
    print("Do you wish to use this tool's processing
workflow with your own datasets?")
    print("If so, the following additional packages are necessary:")
    print(paste(extra_lib_list, collapse = ", "))
    print("Please check Anaconda and the README is satisfied before proceeding.")
    extras <- readline(prompt="
To proceed with additional package installation, type 'Y' and press enter to continue. 
Type anything else and press enter to skip this step.")
    
    if (extras == "Y")
    {
      for (lib in extra_lib_list)
      {
        if (!require(lib, character.only=TRUE, quietly=TRUE))
        {
          print(sprintf("Installing %s ...", lib))
          install.packages(lib, type="binary", character.only=TRUE)
          library(lib, character.only=TRUE)
        }
      }
    }
    
    dirCorrect <- FALSE
    
    while(!dirCorrect)
    {
      print(sprintf("As a reference, the current directory is: %s", getwd()))
      print("This directory ought to contain 'app' as a direct subfolder.")
      mydear <- readline(
        prompt=sprintf("
Is this correct? Type 'Y' and press enter to continue.
Type 'Q' and press enter to quit.
Otherwise, type in an address and press enter to change directories.",
                       paste(list.files(), collapse=", ")))
      if (mydear == "Q")
      {
        print("You have quit the installation.")
        return()
      }
      if (mydear != "Y")
      {
        tryCatch(
          setwd(mydear),
          error = function(e){
            print("Failed to change working directory.")
          }
        )
      }
      else
        dirCorrect <- TRUE
    }
    
    shiny::runApp("app")
  }
  else
  {
    print("You have quit the installation.")
  }
}

# run installation - further notes are located below
install()