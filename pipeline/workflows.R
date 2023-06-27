# The purpose of this file is to assign a workflow for the given
# session and manipulate / create directories for the assigned workflow.

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

# -----------------
# SET WORKFLOW ROOT
# -----------------

if (!exists("wf_config"))
  wf_config <- list()

wf_config$root_storage_loc <- get_project_loc("sdr_workflow_root.rds")

# displays the current workflow root (aka workflows folder)
display_workflow_root <- function()
{
  if (is.null(wf_config$root))
    cat("Workflows Folder: NULL (unspecified)\n")
  else
    cat_f("Workflows Folder: %s\n", wf_config$root)
}

# interactive prompt to set the location of the workflow root
set_workflow_root <- function()
{
  while (TRUE)
  {
    cat("\n")
    display_workflow_root()
    confirm_loc <- readline(
      prompt = "To assign all future workflows to an (ideally empty) folder,
type the folder's path and press enter.
To quit, type 'q()' and press enter.")

    if (confirm_loc == "q()")
    {
      if (is.null(wf_config$root))
        stop("Workflows folder not specified!")
      else
      {
        message("Workflows folder unchanged!")
        break
      }
    }

    if (dir.exists(confirm_loc))
    {
      wf_config$root <- confirm_loc
      saveRDS(confirm_loc, wf_config$root_storage_loc)
      break
    }
  }
}

# loads the workflow root or sets its location
if (file.exists(wf_config$root_storage_loc))
{
  cand_root <- readRDS(wf_config$root_storage_loc)
  if (!dir.exists(cand_root))
    set_workflow_root()
  else
    wf_config$root <- cand_root
  rm(cand_root)
} else
{
  set_workflow_root()
}

# NOTE: past this point, we assume a valid wf_config$root

# gets the absolute path of a file given its relative path to the workflow root
get_workflow_loc <- function(file)
{
  stopifnot(is_str(file))
  sprintf("%s/%s", wf_config$root, file)
}

# creates a directory if it does not exist
safe_dir <- function(path)
{
  if (!dir.exists(path))
    dir.create(path)
}

# have the user enter the workflow name
while (is.null(wf_config$workflow))
{
  display_workflow_root()
  workflow_names <- list.dirs(wf_config$root, full.names = FALSE, recursive = FALSE)
  cat_f("Available Workflows: %s\n", paste(workflow_names, collapse = ", "))
  confirm_wf <- readline(
    prompt = "To quit, type 'q()' and press enter.
To change the workflows folder, type 'root()' and press enter.
To open an available workflow, type its name and press enter.
To create a new workflow, type its name and press enter.")

  if (confirm_wf == "q()")
    stop("Section workflow not specified.")

  if (confirm_wf == "root()")
    set_workflow_root()
  else
  {
    if (confirm_wf != make.names(confirm_wf))
      message("This workflow name is not valid (see base::make.names).")
    else
    {
      if (confirm_wf %in% workflow_names)
      {
        wf_config$workflow <- confirm_wf
        message("Your workflow has been assigned.
The R session must be restarted to change your workflow.")
      }
      else
      {
        confirm_create_wf <- readline(
          prompt = sprintf("Are you sure you want to create the workflow '%s'?
To proceed, type 'Y' and press enter.
Type anything else and press enter to quit.",
                           confirm_wf))

        if (confirm_create_wf == "Y")
        {
          safe_dir(get_workflow_loc(confirm_wf))
          wf_config$workflow <- confirm_wf
          message("Your workflow has been assigned.
The R session must be restarted to change your workflow.")
        }

        rm(confirm_create_wf)
      }
    }
  }

  rm(workflow_names, confirm_wf)
}

# roo (root)
# -- raw (raw) [can be edited by the user]
# -- pro (processing)
# -- -- com (combined)
# -- -- int (inter)
# -- -- req (requests) [can be edited by the user]
# -- ref (reference)
# -- app (app)
# -- -- dep (dependencies)
wf_config$roo_loc <- file.path(wf_config$root, wf_config$workflow)
safe_dir(wf_config$roo_loc)
wf_config$raw_loc <- file.path(wf_config$roo_loc, "raw")
safe_dir(wf_config$raw_loc)
wf_config$pro_loc <- file.path(wf_config$roo_loc, "processing")
safe_dir(wf_config$pro_loc)
wf_config$com_loc <- file.path(wf_config$pro_loc, "combined")
safe_dir(wf_config$com_loc)
wf_config$int_loc <- file.path(wf_config$pro_loc, "inter")
safe_dir(wf_config$int_loc)
wf_config$req_loc <- file.path(wf_config$pro_loc, "requests")
safe_dir(wf_config$req_loc)
wf_config$ref_loc <- file.path(wf_config$roo_loc, "reference")
safe_dir(wf_config$ref_loc)

# --------------------
# DEPLOYMENT FUNCTIONS
# --------------------

# source_sdr("storage.R")
# query_storage(wf_config$ref_loc)
# app_requests <- load_store("app_requests.rds")
# supported_cats <- unique(app_requests$CATEGORIES)
# get_dependency("categories_full")
#
# for (group in names(categories_full))
#   for (cat in names(categories_full[[group]]))
#     if (!(cat %in% supported_cats))
#       categories_full[[group]][[cat]] <- NULL
#
# set_dependency("categories_full")

# app_data <- list(
#   "amazon_keys" = amazon_keys,
#   "app_citations" = app_citations,
#   "app_title" = app_title,
#   "categories_full" = categories_full,
#   "custom_color_scales" = custom_color_scales,
#   "decorations" = decorations,
#   "order_total" = order_total,
#   "user_credentials" = user_credentials
# )
#
# saveRDS(app_data, "app/app_data.rds")
