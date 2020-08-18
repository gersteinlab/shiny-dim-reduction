# The purpose of this script is to store functions for 
# building the files and folders of a dimensionality reduction app.
# source("~/Justin-Tool/code/build.R")

# ---------
# SHORTHAND
# ---------

source("~/Justin-Tool/code/inherit.R")

exrna <- function(){
  runApp("~/Justin-Tool/exRNA/app/app.R")
}

entex <- function(){
  runApp("~/Justin-Tool/ENTEx/app/app.R")
}

# --------------
# USER VARIABLES
# --------------

root <- "~/Justin-Tool"
project_name <- "ENTEx"

# roo (root)
# -- raw (raw)
# -- pro (processing)
# -- -- com (combined)
# -- app (app)
# -- -- dep (dependencies)
# -- -- dat (data)
roo_loc <- sprintf("%s/%s", root, project_name)
raw_loc <- sprintf("%s/raw", roo_loc)
pro_loc <- sprintf("%s/processing", roo_loc)
com_loc <- sprintf("%s/combined", pro_loc)
app_loc <- sprintf("%s/app", roo_loc)
dep_loc <- sprintf("%s/dependencies", app_loc)
dat_loc <- sprintf("%s/data", app_loc)

# ---------
# FUNCTIONS
# ---------

# builds the skeleton of the app
basic_build <- function() {
  dir.create(roo_loc)
  dir.create(raw_loc)
  dir.create(pro_loc)
  dir.create(com_loc)
  dir.create(app_loc)
  dir.create(dep_loc)
  dir.create(dat_loc)
}

# update the selected files in the app
update_app <- function(filenames) {
  current <- getwd()
  setwd(root)
  for (file in filenames)
  {
    loc <- sprintf("%s/%s", app_loc, file)
    if (file.exists(loc))
      file.remove(loc)
    file.copy(sprintf("code/%s", file), app_loc)
  }
  setwd(current)
}

# shorter version of update_app
uapp <- function(){
  update_app(c("app.R", 
               "interface.R", 
               "functions.R", 
               "options.R", 
               "inherit.R"))
}