# The purpose of this script is to store functions for 
# building the files and folders of a dimensionality reduction app.
# source("pipeline.R", encoding="UTF-8")

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("outline.R", encoding="UTF-8")

# --------------
# USER VARIABLES
# --------------

root <- Sys.getenv("SHINY_DIM_REDUCTION_ROOT")

# have the user enter the project name
while (!exists("project_name"))
{
  attempted_name <- readline(prompt = "
Please enter the name of the project that you would like to work on.")
  
  if (attempted_name %in% list.files(root))
    assign("project_name", attempted_name, envir = .GlobalEnv)
}

# creates a directory carefully
safe_dir <- function(path)
{
  if (!dir.exists(path))
    dir.create(path)
  invisible()
}

# roo (root)
# -- raw (raw)
# -- pro (processing)
# -- ref (reference)
# -- app (app)
# -- -- dep (dependencies)
roo_loc <- sprintf("%s/%s", root, project_name)
safe_dir(roo_loc)
raw_loc <- sprintf("%s/raw", roo_loc)
safe_dir(raw_loc)
pro_loc <- sprintf("%s/processing", roo_loc)
safe_dir(pro_loc)
ref_loc <- sprintf("%s/reference", roo_loc)
safe_dir(ref_loc)
app_loc <- sprintf("%s/app", roo_loc)
safe_dir(app_loc)
dep_loc <- sprintf("%s/dependencies", app_loc)
safe_dir(dep_loc)

# ---------
# FUNCTIONS
# ---------

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

# switches projects
pro_swi <- function()
{
  rm(project_name, envir=.GlobalEnv)
  setwd(sprintf("%s/shiny-dim-reduction", root))
  source("pipeline.R", encoding="UTF-8")
}

save_ref <- function(object, file)
{
  curdir <- getwd()
  setwd(ref_loc)
  dn <- dirname(file)
  if (!dir.exists(dn))
    dir.create(dn, recursive=TRUE)
  saveRDS(object, file)
  setwd(curdir)
  invisible()
}

# runs the app
rapp <- function(){
  runApp(sprintf("%s/app/app.R", roo_loc))
}

# updates and runs the app
uapp <- function(){
  update_app(c("app.R", 
               "interface.R", 
               "app_functions.R", 
               "ui_functions.R", 
               "options.R", 
               "outline.R",
               "installer.R"))
  rapp()
}