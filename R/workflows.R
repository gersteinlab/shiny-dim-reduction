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
set_workflow_root <- function()
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
      assign_global("sdr_workflow_root", loc)
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
    set_workflow_root()
} else
{
  set_workflow_root()
}

# have the user enter the workflow name
while (!exists("workflow_name"))
{
  sprintf_clean("Current Workflow Root: %s", sdr_workflow_root)
  workflow_names <- list.files(sdr_workflow_root)
  sprintf_clean("Current Workflows Available: %s", paste(workflow_names, collapse = ", "))
  sprintf_clean("To change the workflow root, type set_workflow_root().")
  attempted_name <- readline(
    prompt = "Type the name of the workflow that you would like to use and press enter.
To change projects, restart this session. Type 'Q' and press enter to quit. ")

  if (attempted_name == "set_workflow_root()")
    set_workflow_root()

  if (attempted_name == "Q")
    stop("Quitting ... project not selected.")

  if (attempted_name %in% workflow_names)
    assign_global("workflow_name", attempted_name)
}

# creates a directory carefully
safe_dir <- function(path)
{
  if (!dir.exists(path))
    dir.create(path)
  invisible()
}

# ALTERNATE DESIGN PHILOSOPHY:
# roo (root, ex: exRNA)
# -- com (combined)
# -- int (intermediate)
# -- ref (reference)
# -- req (requests)
# -- app (app)
# -- -- dep (dependencies)

# roo (root)
# -- raw (raw)
# -- pro (processing)
# -- -- com (combined)
# -- -- int (inter)
# -- ref (reference)
# -- app (app)
# -- -- dep (dependencies)
roo_loc <- sprintf("%s/%s", sdr_workflow_root, workflow_name)
safe_dir(roo_loc)
raw_loc <- sprintf("%s/raw", roo_loc)
safe_dir(raw_loc)
pro_loc <- sprintf("%s/processing", roo_loc)
safe_dir(pro_loc)
com_loc <- sprintf("%s/combined", pro_loc)
safe_dir(com_loc)
int_loc <- sprintf("%s/inter", pro_loc)
safe_dir(int_loc)
ref_loc <- sprintf("%s/reference", roo_loc)
safe_dir(ref_loc)
app_loc <- sprintf("%s/app", roo_loc)
safe_dir(app_loc)
dep_loc <- sprintf("%s/dependencies", app_loc)
safe_dir(dep_loc)

# --------------------
# DEPLOYMENT FUNCTIONS
# --------------------

# update the selected files in the app
update_app <- function(filenames) {
  safe_dir(sprintf("%s/src", app_loc))

  file.copy(get_project_loc("R/app.R"), app_loc, overwrite = TRUE)
  file.copy(get_project_loc("install.R"), app_loc, overwrite = TRUE)

  for (file in filenames)
    file.copy(get_project_loc(sprintf("R/%s", file)),
              sprintf("%s/src", app_loc), overwrite = TRUE)
}

# runs the app
rapp <- function(){
  shiny::runApp(sprintf("%s/app.R", app_loc))
}

# updates and runs the app
uapp <- function(){
  print_clean("Beginning update ...")
  update_app(c("find_replace.R",
               "text_work.R",
               "ui_functions.R",
               "storage.R",
               "preprocess.R",
               "make_requests.R",
               "options.R",
               "plotting.R",
               "authentication.R"))
  print_clean("Updates complete ...")
  rapp()
}

# rsconnect::deployApp(app_loc, appName = "exrna_F21",
#                      account = "justinchang1124", launch.browser = TRUE)
