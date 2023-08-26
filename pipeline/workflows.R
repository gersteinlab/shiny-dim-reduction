# The purpose of this file is to manage workflows and indicate
# a current workflow (while reasoning about its directories).

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

# ---------------
# WORKFLOW CONFIG
# ---------------

#' whether R would accept x as variable names
#'
#' @param x [object]
#' @returns [boolean]
are_var_names <- function(x)
{
  all(make.names(x) == x)
}

#' wrapper for are_var_names
#'
#' @param x [object], e.g. vector, list, data.frame
#' @returns [boolean]
has_var_names <- function(x)
{
  are_var_names(names(x))
}

#' whether x is a 'wf_config' object, where
#' each element of wf_config is the directory
#' of that workflow accessed by workflow name
#' note: workflow data can be valid without
#' the folders of that workflow existing
#'
#' @param x [object]
#' @returns [boolean]
is_wf_config <- function(x)
{
  is.list(x) && all_fun_true(x, is_str) &&
    has_safe_names(x) &&
    has_var_names(x)
}

# default wf_config
wf_config <- list()

#' cat wf_config to console
cat_wf_config <- function()
{
  cat_f("--- WORKFLOWS ---\n")
  for (wf in names(wf_config))
    cat_f("%s: %s\n", wf, wf_config[[wf]])
  cat_f("\n")
}

#' sets wf_config to wf_cfg
#'
#' @param wf_cfg [wf_config]
update_wf_config <- function(wf_cfg)
{
  stopifnot(is_wf_config(wf_cfg))
  wf_config <<- wf_cfg
}

#' updates / inserts workflow wf into wf_config by path
#' (when needed, a folder for wf will be created in path)
#'
#' @param wf [workflow]
#' @param wf_loc [string or NULL] representing a directory
upsert_workflow <- function(wf, path)
{
  stopifnot(
    is_str(wf),
    is_str(path),
    dir.exists(path) # ensure the directory exists
  )

  wf_cfg <- wf_config
  wf_cfg[[wf]] <- path.expand(file.path(path, wf))
  update_wf_config(wf_cfg)
}

#' attempts to delete a workflow wf by name
#' note: deleting a nonexistent workflow succeeds
#' note: deleting a workflow does not immediately
#' delete the corresponding files for that workflow
#'
#' @param wf [string]
unlink_workflow <- function(wf)
{
  stopifnot(is_str(wf))
  wf_config[[wf]] <<- NULL
}

#' attempts to load wf_config
load_wf_config <- function()
{
  stopifnot(sdr_config$mode == "pipeline")
  readRDS(get_project_loc("sdr_wf_config.rds")
          ) %>% update_wf_config()
}

#' attempts to save wf_config
save_wf_config <- function()
{
  stopifnot(is_wf_config(wf_config),
            sdr_config$mode == "pipeline")
  saveRDS(wf_config,
          get_project_loc("sdr_wf_config.rds"))
}

# --------------------
# NAVIGATING WORKFLOWS
# --------------------

# loc_wf (workflow)
# -- loc_table (tables)
# -- loc_inter (intermediates)
# -- loc_store (local store)
# -- loc_reque (request queue)
# -- loc_app_d (app data)

#' whether a string is a workflow
#'
#' @param x [object]
#' @returns [boolean]
is_workflow <- function(x)
{
  is_str(x) && (x %in% names(wf_config))
}

#' cat workflow wf to console
#'
#' @param wf [workflow] not checked
cat_workflow <- function(wf)
{
  cat_f("WORKFLOW: %s\n", wf)
}

#' gets workflow
#'
#' @returns [workflow]
get_workflow <- function()
{
  Sys.getenv("SDR_WORKFLOW")
}

#' sets workflow to x
#'
#' @param x [workflow]
set_workflow <- function(x)
{
  stopifnot(is_workflow(x))
  cat_workflow(x)
  Sys.setenv("SDR_WORKFLOW" = x)
}

#' gets a relative location in the active workflow
#'
#' @param file [character]
#' @returns [string]
get_loc_wf <- function(file = "")
{
  stopifnot(is.character(file))
  workflow <- get_workflow()
  stopifnot(is_workflow(workflow))
  file.path(wf_config[[workflow]], file)
}

#' prepend a tables folder to file
#'
#' @param file [character]
#' @returns [character]
get_loc_table <- function(file = "")
{
  stopifnot(is.character(file))
  file.path("sdr_tables", file) %>% get_loc_wf()
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
get_loc_inter <- function(file = "")
{
  stopifnot(is.character(file))
  file.path("sdr_intermediates", file) %>% get_loc_wf()
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
get_loc_store <- function(file = "")
{
  stopifnot(is.character(file))
  file.path("sdr_local_store", file) %>% get_loc_wf()
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
get_loc_reque <- function(file = "")
{
  stopifnot(is.character(file))
  file.path("sdr_requests", file) %>% get_loc_wf()
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
get_loc_app_d <- function(file = "")
{
  stopifnot(is.character(file))
  file.path("sdr_app_data", file) %>% get_loc_wf()
}

# ---------------------
# SYNC APPS / WORKFLOWS
# ---------------------

# expected files to copy between app / workflows
app_wf_files <- c("app_data.rds", "local_store.rds", "cloud_store.rds")

#' copies files from app folder to workflow folder
#'
#' @param file [character] not checked
copy_app_to_wf <- function(file = app_wf_files)
{
  stopifnot(sdr_config$mode == "pipeline")
  ensure_dir(get_loc_app_d())
  file.copy(get_app_loc(file), get_loc_app_d(file), overwrite = TRUE)
}

#' copies files from workflow folder to app folder
#'
#' @param file [character] not checked
copy_wf_to_app <- function(file = app_wf_files)
{
  stopifnot(sdr_config$mode == "pipeline")
  file.copy(get_loc_app_d(file), get_app_loc(file), overwrite = TRUE)
}

#' copy_app_to_wf with a message
#'
#' @param file [character] not checked
copy_app_to_wf_msg <- function(file = app_wf_files)
{
  message_f("app to %s: %s", get_workflow(),
            vec_str(file[copy_app_to_wf(file)]))
}

#' copy_wf_to_app with a message
#'
#' @param file [character] not checked
copy_wf_to_app_msg <- function(file = app_wf_files)
{
  message_f("%s to app: %s", get_workflow(),
            vec_str(file[copy_wf_to_app(file)]))
}

# ------------------
# SYNC LOCAL / CLOUD
# ------------------

#' the location of the administrative cloud_store
#' (administrative means read/write capabilities)
#'
#' @returns [string]
cloud_store_admin_loc <- function()
{
  stopifnot(sdr_config$mode == "pipeline")
  get_loc_wf("sdr_cloud_store_admin.rds")
}

#' copies files from the local store to the cloud store
#'
#' @param files [character] of existing files
copy_local_to_cloud <- function(files, each = 100)
{
  # require that an administrative store is available
  cloud_store_admin <- readRDS(cloud_store_admin_loc())
  stopifnot(cloud_connects(cloud_store_admin))

  # require that all files exist before proceeding
  stopifnot(is.character(files))
  file_n <- length(files)

  for (i in seq_len(file_n))
  {
    file <- files[i]
    load_local(
      file, stop_f("missing %s", file)
    ) %>% save_cloud(file)
    if (i %% each == 1 || i == file_n)
      cat_f("uploaded %s/%s: %s\n", i, file_n, file)
  }
}

#' copies files from the cloud store to the local store
#'
#' @param files [character] of existing files
copy_cloud_to_local <- function(files, each = 100)
{
  # require that an administrative store is available
  cloud_store_admin <- readRDS(cloud_store_admin_loc())
  stopifnot(cloud_connects(cloud_store_admin))

  # require that all files exist before proceeding
  stopifnot(is.character(files))
  file_n <- length(files)

  for (i in seq_len(file_n))
  {
    file <- files[i]
    load_cloud(
      file, stop_f("missing %s", file)
    ) %>% save_local(file)
    if (i %% each == 1 || i == file_n)
      cat_f("uploaded %s/%s: %s\n", i, file_n, file)
  }
}

