# The purpose of this file is to manage storage systems (stores).

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
#' @returns [character] representing files
list_local <- function(path)
{
  list.files(prefix_local(path), recursive = TRUE)
}

#' whether file exists in local_store
#'
#' @param file [string]
#' @returns [boolean]
find_local <- function(file)
{
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
  readRDS(path_local(file))
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
  tryCatch(
    return(bucket_exists(Sys.getenv("AWS_ACCESS_BUCKET"))),
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

#' lists all files at the given path in cloud_store
#'
#' @param path [string], usually a directory
#' @returns [character] representing files
list_aws_s3 <- function(path)
{
  as.character(sapply(
    get_bucket(
      Sys.getenv("AWS_ACCESS_BUCKET"),
      prefix = path,
      max = Inf
    ),
    function(x){x$Key}
  ))
}

#' whether file exists in cloud_store
#'
#' @param file [string]
#' @returns [boolean]
find_aws_s3 <- function(file)
{
  tryCatch(
    return(object_exists(
      file,
      Sys.getenv("AWS_ACCESS_BUCKET")
    )),
    warning = function(e) return(FALSE),
    error = function(e) return(FALSE)
  )
}

#' saves data to file in the cloud_store
#' note: modified from aws.s3::s3save
#'
#' @param file [string]
my_amazon_obj <- NULL
save_aws_s3 <- function(data, file)
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  my_amazon_obj <<- data
  save(my_amazon_obj, file = tmp, envir = .GlobalEnv)
  put_object(
    file = tmp,
    bucket = Sys.getenv("AWS_ACCESS_BUCKET"),
    object = file
  )
}

#' reads data from file in the cloud_store
#'
#' @param file [string]
#' @returns [object]
load_aws_s3 <- function(file, default = NULL)
{
  if (!find_aws_s3(file))
    return(default)
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save_object(
    bucket = Sys.getenv("AWS_ACCESS_BUCKET"),
    object = file,
    file = tmp
  )
  load(tmp, envir = .GlobalEnv)
  my_amazon_obj
}

# -------------
# STORAGE QUERY
# -------------

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
      stop("Local / cloud connections failed.")
  }
}
