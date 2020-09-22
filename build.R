# The purpose of this script is to store functions for 
# building the files and folders of a dimensionality reduction app.
# source("~/Justin-Tool/shiny-dim-reduction/build.R")

source("~/Justin-Tool/shiny-dim-reduction/inherit.R")

# --------------
# USER VARIABLES
# --------------

root <- "~/Justin-Tool"
project_names <- c("exRNA", "ENTEx")

if (!exists("project_name"))
  project_name <- project_names[1]

# roo (root)
# -- raw (raw)
# -- pro (processing)
# -- app (app)
# -- -- dep (dependencies)
roo_loc <- sprintf("%s/%s", root, project_name)
raw_loc <- sprintf("%s/raw", roo_loc)
pro_loc <- sprintf("%s/processing", roo_loc)
app_loc <- sprintf("%s/app", roo_loc)
dep_loc <- sprintf("%s/dependencies", app_loc)

# ---------
# FUNCTIONS
# ---------

switch_project <- function(name)
{
  if (missing(name))
    name <- setdiff(project_names, project_name)[1]
  
  project_name <<- name
  
  roo_loc <<- sprintf("%s/%s", root, project_name)
  raw_loc <<- sprintf("%s/raw", roo_loc)
  pro_loc <<- sprintf("%s/processing", roo_loc)
  app_loc <<- sprintf("%s/app", roo_loc)
  dep_loc <<- sprintf("%s/dependencies", app_loc)
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
    file.copy(sprintf("shiny-dim-reduction/%s", file), app_loc)
  }
  setwd(current)
}

# runs the app
rapp <- function(...){
  runApp(sprintf("%s/app/app.R", roo_loc))
}

# updates and runs the app
uapp <- function(...){
  update_app(c("app.R", 
               "interface.R", 
               "functions.R", 
               "options.R", 
               "inherit.R"))
  rapp()
}