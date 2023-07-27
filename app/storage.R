# The purpose of this file is to manage storage systems (stores).
# For AWS S3, you may find it difficult to explore available files
# with list_cloud because it retrieves all files with data.
# Instead, you can use the AWS CLI with the system() command.
# https://docs.aws.amazon.com/cli/latest/userguide/
# note: you may need to reinstall RStudio to get the CLI to work
# e.g. system("aws --version")

# Goals for each store:
# --is_store: is in correct format
# --make_store: make the store
# --set_store: set the store
# --store_connected: whether store connects
# --store_connects: try to connect and see the result
# --list_store: list all files with a prefix
# --find_store: returns whether a file exists
# --save_store: saves a file, creating the directory if it doesn't exist
# --load_store: loads a file, returning a default if it doesn't exist
# --delete_store: deletes a file

if (!exists("sdr_config"))
  source("app/install.R")

library(aws.s3)

# -------------
# LOCAL STORAGE
# -------------

#' whether x is a 'local_store' object
#' (note: local stores may not connect)
#'
#' @param x [object]
#' @returns [boolean]
is_local_store <- function(x)
{
  is_str(x)
}

#' makes a local_store
#'
#' @param dir [string], e.g. a directory path
#' @returns [local_store]
make_local_store <- function(dir = "")
{
  stopifnot(is_local_store(dir))
  dir
}

#' sets the global local_store to x
#'
#' @param x [local_store] not checked
set_local_store <- function(x)
{
  Sys.setenv("LOCAL_STORE" = x)
}

#' is the global local_store connected?
#'
#' @returns [boolean]
local_connected <- function()
{
  dir.exists(Sys.getenv("LOCAL_STORE"))
}

#' tries to connect a local_store and
#' returns whether connecting succeeded
#'
#' @param local_store [local_store]
#' @returns [boolean]
local_connects <- function(local_store)
{
  if (!is_local_store(local_store))
    return(FALSE)
  set_local_store(local_store)
  local_connected()
}

#' adds the local_store path as a prefix
#'
#' @param path [string]
#' @returns [string]
prefix_local <- function(path)
{
  file.path(Sys.getenv("LOCAL_STORE"), path)
}

#' lists all files at the given path in local_store
#'
#' @param path [string], usually a directory
#' @returns [character] representing files, no prefix
list_local <- function(path = "")
{
  stopifnot(is_str(path))
  list.files(prefix_local(path), recursive = TRUE)
}

#' whether file exists in local_store
#'
#' @param file [string]
#' @returns [boolean]
find_local <- function(file)
{
  stopifnot(is_str(file))
  file.exists(prefix_local(file))
}

#' ensures the existence of a directory
#'
#' @param path [string], usually a directory
ensure_dir <- function(path)
{
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)
}

#' saveRDS but ensure the directory first
#'
#' @param file [string]
mkdir_saveRDS <- function(data, file, compress = TRUE)
{
  ensure_dir(dirname(file))
  saveRDS(data, file, compress = compress)
}

#' saves data to file in the local_store
#'
#' @param file [string]
save_local <- function(data, file)
{
  stopifnot(is_str(file))
  mkdir_saveRDS(data, prefix_local(file))
}

#' reads data from file in the local_store
#'
#' @param file [string]
#' @returns [object]
load_local <- function(file, default = NULL)
{
  if (!find_local(file))
    return(default)
  readRDS(prefix_local(file))
}

#' deletes file in the local_store
#'
#' @param file [string]
#' @returns [object]
delete_local <- function(file)
{
  stopifnot(is_str(file))
  unlink(file)
}

# -------------
# CLOUD STORAGE
# -------------

#' whether x is a 'cloud_store' object
#' (note: cloud stores may not connect)
#'
#' @param x [object]
#' @returns [boolean]
is_cloud_store <- function(x)
{
  members <- c("id", "secret", "bucket")
  is.list(x) && identical(names(x), members) &&
    is_str(x$id) && is_str(x$secret) && is_str(x$bucket)
}

#' makes a cloud_store
#'
#' @param id [string] per aws.s3::s3HTTP
#' @param secret [string] per aws.s3::s3HTTP
#' @param bucket [string] naming the bucket
#' @returns [cloud_store]
make_cloud_store <- function(id = "", secret = "", bucket = "")
{
  result <- list(
    "id" = id,
    "secret" = secret,
    "bucket" = bucket
  )

  stopifnot(is_cloud_store(result))
  result
}

#' sets the global cloud_store to x
#'
#' @param x [cloud_store] not checked
set_cloud_store <- function(x)
{
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = x$id,
    "AWS_SECRET_ACCESS_KEY" = x$secret,
    "AWS_ACCESS_BUCKET" = x$bucket
  )
}

#' is the global cloud_store connected?
#'
#' @returns [boolean]
cloud_connected <- function()
{
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  tryCatch(
    return(aws.s3::bucket_exists(env_bucket)),
    warning = function(e) return(FALSE),
    error = function(e) return(FALSE)
  )
}

#' tries to connect a cloud_store and
#' returns whether connecting succeeded
#'
#' @param cloud_store [cloud_store]
#' @returns [boolean]
cloud_connects <- function(cloud_store)
{
  if (!is_cloud_store(cloud_store))
    return(FALSE)
  set_cloud_store(cloud_store)
  cloud_connected()
}

#' get first index past prefix, including slashes
#'
#' @param n [integer] length of prefix
#' @returns [integer]
get_prefix_start <- function(n = 0)
{
  if (n < 1)
    return(1)
  n + 2
}

#' lists all files at the given path in cloud_store
#'
#' @param path [string], usually a directory
#' @returns [character] representing files, no prefix
list_cloud <- function(path = "")
{
  stopifnot(is_str(path))
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  result <- as.character(sapply(
    aws.s3::get_bucket(env_bucket, path, max = Inf),
    function(x){x$Key}
  ))
  substring(result, get_prefix_start(nchar(path)))
}

#' summarizes a bucket
#'
#' @returns [integer] status code
summarize_bucket <- function()
{
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  # stopifnot(system2("aws", "--version") == 0)
  system2("aws", c("s3", "ls", env_bucket, "--summarize", "--recursive"))
}

#' whether file exists in cloud_store
#'
#' @param file [string]
#' @returns [boolean]
find_cloud <- function(file)
{
  stopifnot(is_str(file))
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  tryCatch(
    return(aws.s3::object_exists(file, env_bucket)),
    warning = function(e) return(FALSE),
    error = function(e) return(FALSE)
  )
}

#' saves data to file in the cloud_store
#' note: modified from aws.s3::s3save
#'
#' @param file [string]
save_cloud <- function(data, file)
{
  stopifnot(is_str(file))
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  amazon_object_data <- data
  save(amazon_object_data, file = tmp)
  aws.s3::put_object(tmp, file, env_bucket)
}

#' reads data from file in the cloud_store
#'
#' @param file [string]
#' @returns [object]
load_cloud <- function(file, default = NULL)
{
  if (!find_cloud(file))
    return(default)
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  aws.s3::save_object(file, env_bucket, tmp)
  load(tmp)
  amazon_object_data
}

#' deletes file in the cloud_store
#'
#' @param file [string]
#' @returns [object]
delete_cloud <- function(file)
{
  stopifnot(is_str(file))
  env_bucket <- Sys.getenv("AWS_ACCESS_BUCKET")
  aws.s3::delete_object(file, env_bucket)
}

# -----------------------
# GENERAL STORE FUNCTIONS
# -----------------------

#' sets store_mode to x
#'
#' @param x [store_mode]
set_store_mode <- function(x)
{
  stopifnot(x %in% c("local", "cloud"))
  Sys.setenv("SDR_STORE_MODE" = x)
}

#' raises an error message for an invalid store_mode
#'
#' @param x [store_mode]
stop_store_mode <- function(x)
{
  stop_f("Store mode '%s' is invalid!", x)
}

#' helper to swap store mode
swap_store_mode <- function()
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    set_store_mode("cloud")
  if (store_mode == "cloud")
    set_store_mode("local")
  stop_store_mode(store_mode)
}

#' lists all files at the given path
#'
#' @param path [string], usually a directory
#' @returns [character] representing files
list_store <- function(path)
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    return(list_local(path))
  if (store_mode == "cloud")
    return(list_cloud(path))
  stop_store_mode(store_mode)
}

#' whether file exists
#'
#' @param file [string]
#' @returns [boolean]
find_store <- function(file)
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    return(find_local(file))
  if (store_mode == "cloud")
    return(find_cloud(file))
  stop_store_mode(store_mode)
}

#' saves data to file
#'
#' @param file [string]
save_store <- function(data, file)
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    return(save_local(data, file))
  if (store_mode == "cloud")
    return(save_cloud(data, file))
  stop_store_mode(store_mode)
}

#' reads data from file
#'
#' @param file [string]
#' @param default [object] returned when the file doesn't exist
#' @returns [object]
load_store <- function(file, default = NULL)
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    return(load_local(file, default))
  if (store_mode == "cloud")
    return(load_cloud(file, default))
  stop_store_mode(store_mode)
}

#' deletes file
#'
#' @param file [string]
#' @returns [object]
delete_store <- function(file)
{
  store_mode <- Sys.getenv("SDR_STORE_MODE")
  if (store_mode == "local")
    return(delete_local(file))
  if (store_mode == "cloud")
    return(delete_cloud(file))
  stop_store_mode(store_mode)
}

# -----------------
# DECIDE STORE MODE
# -----------------

#' asks the user whether to use local or cloud storage
#'
#' @returns [string]
get_user_store_mode <- function()
{
  if (readline("Type 'y' and press enter to use local storage.
Type anything else and press enter to use cloud (AWS S3) storage."
  ) == "y")
    return("local")
  "cloud"
}

#' decides whether to use local or cloud storage
#'
#' @param local_store [local_store]
#' @param cloud_store [cloud_store]
#' @returns [string]
decide_store_mode <- function(local_store, cloud_store)
{
  if (local_connects(local_store))
  {
    if (cloud_connects(cloud_store))
      return(get_user_store_mode())
    else
      return("local")
  }
  else
  {
    if (cloud_connects(cloud_store))
      return("cloud")
    else
      stop("Could not connect to local_store or cloud_store.")
  }
}
