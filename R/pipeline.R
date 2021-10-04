# The purpose of this script is to store functions for
# building the files and folders of a dimensionality reduction app.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

# -----------------
# SET WORKFLOW ROOT
# -----------------

workflow_root_loc <- get_project_loc("sdr_workflow_root.rds")

# interactive prompt to set the location of the workflow root
set_workflow_root_loc <- function()
{
  while (TRUE)
  {
    loc <- readline(prompt = "
Type a valid directory to store all future workflows in.
Type 'Q' and press enter to quit. ")

    if (loc == "Q")
      stop("Quitting reduction pipeline ... workflow root not set.")

    if (dir.exists(loc))
    {
      assign("sdr_workflow_root", loc, envir = .GlobalEnv)
      saveRDS(loc, workflow_root_loc)
      break
    }
  }
}

# gets the absolute path of a file given its relative path to the workflow root
get_workflow_loc <- function(file)
{
  if(!exists("sdr_workflow_root"))
    set_workflow_root_loc()
  sprintf("%s/%s", sdr_workflow_root, file)
}

# loads the workflow root or sets its location
if (file.exists(workflow_root_loc))
{
  sdr_workflow_root <- readRDS(workflow_root_loc)
  if (!dir.exists(sdr_workflow_root))
    set_workflow_root_loc()
} else
{
  set_workflow_root_loc()
}

source_sdr("utils.R")
source_sdr("storage.R")

# have the user enter the project name
while (!exists("project_name"))
{
  attempted_name <- readline(prompt = "
Type the name of the project that you would like to work on and press enter.
To change projects, restart this session. Type 'Q' and press enter to quit. ")

  if (attempted_name == "Q")
    stop("Quitting ... project not selected.")

  if (attempted_name %in% list.files(sdr_workflow_root))
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
roo_loc <- sprintf("%s/%s", sdr_workflow_root, project_name)
safe_dir(roo_loc)
raw_loc <- sprintf("%s/raw", roo_loc)
safe_dir(raw_loc)
pro_loc <- sprintf("%s/processing", roo_loc)
safe_dir(pro_loc)
ref_loc <- sprintf("%s/reference", roo_loc)
safe_dir(ref_loc)
assign_root(ref_loc)
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

# runs the app
rapp <- function(){
  runApp(sprintf("%s/app.R", app_loc))
}

# updates and runs the app
uapp <- function(){
  update_app(c("app.R",
               "options.R",
               "ui_functions.R",
               "utils.R",
               "installer.R",
               "output_clean.R",
               "text_work.R",
               "find_replace.R",
               "plotting.R",
               "storage.R",
               "authentication.R"))
  rapp()
}
