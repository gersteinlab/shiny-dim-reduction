# The purpose of this file is to manage workflows and indicate
# a current workflow (while reasoning about its directories).

if (!exists("sdr_config") || sdr_config$mode != "pipeline")
  source("app/install.R")
stopifnot(sdr_config$mode == "pipeline")

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

#' whether x is a 'wf_data' object
#' note: workflow data can be valid without
#' the folders of that workflow existing
#'
#' @param x [object]
#' @returns [boolean]
is_wf_data <- function(x)
{
  is.list(x) && all_fun_true(x, is_str) &&
    has_safe_names(x) &&
    has_var_names(x)
}

#' whether x is a 'wf_config' object
#'
#' @param x [object]
#' @returns [boolean]
is_wf_config <- function(x)
{
  members <- c("data", "history")
  is.list(x) && identical(names(x), members) &&
    is_wf_data(x$data) &&
    all(x$history %in% names(x$data))
}

# default wf_config
wf_config <- list(
  "data" = list(),
  "history" = character()
)

#' sets wf_config to wf_cfg
#'
#' @param wf_cfg [wf_config]
update_wf_config <- function(wf_cfg)
{
  stopifnot(is_wf_config(wf_cfg))
  wf_config <<- wf_cfg
}

#' whether the workflow history is empty
#'
#' @returns [boolean]
history_is_empty <- function()
{
  length(wf_config$history) < 1
}

#' indicates the current workflow
#'
#' @returns [string]
get_current_workflow <- function()
{
  if (history_is_empty())
    stop("No workflow history!")
  tail(wf_config$history, 1)
}

#' sets the current workflow
#'
#' @param wf_name [string] not checked
set_current_workflow <- function(wf_name)
{
  wf_cfg <- wf_config
  wf_cfg$history <- c(wf_cfg$history, wf_name)
  update_wf_config(wf_cfg)
}

#' indicator for the current workflow
#'
#' @param wf_name [string] not checked
#' @returns [string]
star_current <- function(wf_name)
{
  if (!history_is_empty() &&
      wf_name == get_current_workflow())
    return("*")
  "-"
}

#' helpful printing of all workflows
list_workflows <- function()
{
  wf_data <- wf_config$data
  message_f("--- WORKFLOWS ---")
  for (wf_name in names(wf_data))
    message_f("%s %s: %s", star_current(wf_name),
              wf_name, wf_data[[wf_name]])
}

#' gets the location of the current workflow
#'
#' @returns [string]
get_loc_wf <- function()
{
  wf_config$data[[get_current_workflow()]]
}

#' gets a location relative to the current workflow
#'
#' @param [character] not checked
#' @returns [string]
get_loc_rel_wf <- function(file)
{
  file.path(get_loc_wf(), file)
}

#' gets the default wf_loc
#'
#' @returns [string]
def_wf_loc <- function()
{
  if (history_is_empty())
    return(getwd())
  dirname(get_loc_wf())
}

#' updates / inserts a workflow with the given name;
#' a folder with that name will be created in wf_loc
#'
#' @param wf_name [string]
#' @param wf_loc [string or NULL] representing a directory
upsert_workflow <- function(wf_name, wf_loc = def_wf_loc(), mkdir = FALSE)
{
  stopifnot(
    is_str(wf_name),
    are_safe_names(wf_name),
    are_var_names(wf_name),
    is_str(wf_loc),
    dir.exists(wf_loc)
  )

  wf_cfg <- wf_config
  wf_dir <- file.path(wf_loc, wf_name)
  if (mkdir)
    ensure_dir(wf_dir)
  wf_cfg$data[[wf_name]] <- path.expand(wf_dir)
  wf_cfg$history <- c(wf_cfg$history, wf_name)
  update_wf_config(wf_cfg)
}

#' attempts to delete a workflow by name
#' note: deleting a nonexistent workflow succeeds
#' note: deleting a workflow does not immediately
#' delete the corresponding files for that workflow
#'
#' @param wf_name [string]
unlink_workflow <- function(wf_name)
{
  wf_cfg <- wf_config
  wf_cfg$data[[wf_name]] <- NULL
  h_vec <- wf_cfg$history
  wf_cfg$history <- h_vec[h_vec != wf_name]
  update_wf_config(wf_cfg)
}

# loc_wf (workflow)
# -- loc_table (tables)
# -- loc_inter (intermediates)
# -- loc_store (local store)
# -- loc_reque (request queue)
# -- loc_app_d (app data)

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
prepend_table <- function(file = "")
{
  file.path("sdr_tables", file)
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
prepend_inter <- function(file = "")
{
  file.path("sdr_intermediates", file)
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
prepend_store <- function(file = "")
{
  file.path("sdr_local_store", file)
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
prepend_reque <- function(file = "")
{
  file.path("sdr_requests", file)
}

#' see above
#'
#' @param file [character] not checked
#' @returns [character]
prepend_app_d <- function(file = "")
{
  file.path("sdr_app_data", file)
}

# ----------------
# SAVING / LOADING
# ----------------

# where to save / load wf_config
wf_config_loc <- get_project_loc("sdr_wf_config.rds")

#' attempts to load wf_config
load_wf_config <- function()
{
  readRDS(wf_config_loc) %>% update_wf_config()
}

#' attempts to save wf_config
save_wf_config <- function()
{
  stopifnot(is_wf_config(wf_config))
  saveRDS(wf_config, wf_config_loc)
}

# expected files to copy between app / workflows
app_wf_files <- c("app_data.rds", "local_store.rds", "cloud_store.rds")

#' copies files from app folder to workflow folder
#'
#' @param file [character] not checked
copy_app_to_wf <- function(file = app_wf_files)
{
  wf_dir <- get_loc_rel_wf(prepend_app_d())
  ensure_dir(wf_dir)
  file.copy(get_app_loc(file), prepend_app_d(file) %>%
              get_loc_rel_wf(), overwrite = TRUE)
}

#' copies files from workflow folder to app folder
#'
#' @param file [character] not checked
copy_wf_to_app <- function(file = app_wf_files)
{
  file.copy(prepend_app_d(file) %>% get_loc_rel_wf(),
            get_app_loc(file), overwrite = TRUE)
}

#' copy_app_to_wf with a message
#'
#' @param file [character] not checked
copy_app_to_wf_msg <- function(file = app_wf_files)
{
  message_f("app to %s: %s", get_current_workflow(),
            vec_str(file[copy_app_to_wf(file)]))
}

#' copy_wf_to_app with a message
#'
#' @param file [character] not checked
copy_wf_to_app_msg <- function(file = app_wf_files)
{
  message_f("%s to app: %s", get_current_workflow(),
            vec_str(file[copy_wf_to_app(file)]))
}

#' sets the current workflow while mounting files
#'
#' @param wf_name [string] not checked
mount_current_workflow <- function(wf_name)
{
  # save the current data if needed
  if (!history_is_empty())
    copy_app_to_wf_msg()
  # swap to new workflow and copy over app files
  set_current_workflow(wf_name)
  copy_wf_to_app_msg()
}

#' the location of the administrative cloud_store
#'
#' @returns [string]
cloud_store_admin_loc <- function()
{
  get_loc_rel_wf("sdr_cloud_store_admin.rds")
}

