# The purpose of this file is to manage storage systems (stores).

# Goals:
# list_store: list all files with a prefix
# find_store: returns whether a file exists
# save_store: saves a file, creating the directory if it doesn't exist
# load_store: loads a file, returning NULL if it doesn't exist

if (!exists("sdr_config"))
  source("app/install.R")

library(aws.s3)

# -------------
# LOCAL STORAGE
# -------------

# is x in the correct format for a local store?
# (note: can be in correct format and not connect)
is_local_store <- function(x)
{
  is_str(x)
}

# makes a local store object
make_local_store <- function(dirname = "")
{
  stopifnot(is_local_store(dirname))
  dirname
}

# set local store (not checked)
set_local_store <- function(local_store)
{
  Sys.setenv("LOCAL_STORE" = local_store)
}

# check if the local store is connected
local_connected <- function()
{
  sdr_config$mode != "cloud" && dir.exists(Sys.getenv("LOCAL_STORE"))
}

# check local connection
local_connects <- function(local_store)
{
  if (!is_local_store(local_store))
    return(FALSE)
  set_local_store(local_store)
  local_connected()
}

# gives the local storage path for a location
path_local <- function(loc)
{
  file.path(Sys.getenv("LOCAL_STORE"), loc)
}

# lists all files with the given prefix (usually a valid directory)
list_local <- function(prefix = "")
{
  list.files(path_local(prefix))
}

# determines whether a file with the given filename exists
find_local <- function(filename)
{
  file.exists(path_local(filename))
}

# saves data to filename in the root directory
save_local <- function(data, filename)
{
  mkdir_saveRDS(data, path_local(filename))
}

# loads data from filename in the root directory
load_local <- function(filename)
{
  if (!find_local(filename))
    return(NULL)
  readRDS(path_local(filename))
}

# -------------
# CLOUD STORAGE
# -------------

# is x in the correct format for a cloud store?
# (note: can be in correct format and not connect)
is_cloud_store <- function(x)
{
  members <- c("id", "secret", "bucket")
  is.list(x) && identical(names(x), members) &&
    is_str(x$id) && is_str(x$secret) && is_str(x$bucket)
}

# makes a cloud store object
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

# set cloud store (not checked)
set_cloud_store <- function(cloud_store)
{
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = cloud_store$id,
    "AWS_SECRET_ACCESS_KEY" = cloud_store$secret,
    "AWS_ACCESS_BUCKET" = cloud_store$bucket
  )
}

# checks if there exists a connection to AWS
cloud_connected <- function()
{
  tryCatch(
    return(bucket_exists(Sys.getenv("AWS_ACCESS_BUCKET"))),
    warning = function(e) return(FALSE),
    error = function(e) return(FALSE)
  )
}

# check cloud connection
cloud_connects <- function(cloud_store)
{
  if (!is_cloud_store(cloud_store))
    return(FALSE)
  set_cloud_store(cloud_store)
  cloud_connected()
}

# lists the contents of a bucket's prefix
list_aws_s3 <- function(prefix = NULL)
{
  as.character(sapply(
    get_bucket(Sys.getenv("AWS_ACCESS_BUCKET"), prefix = prefix, max = Inf),
    function(x){x$Key}
  ))
}

# determines whether a single object with the given filename exists
find_aws_s3 <- function(filename)
{
  tryCatch(
    return(object_exists(filename, Sys.getenv("AWS_ACCESS_BUCKET"))),
    warning = function(e) return(FALSE),
    error = function(e) return(FALSE)
  )
}

# for temporary use
my_amazon_obj <- NULL

# saves a single object to AWS.s3 - modified from s3save
save_aws_s3 <- function(data, filename)
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  my_amazon_obj <<- data
  save(my_amazon_obj, file = tmp, envir = .GlobalEnv)
  put_object(file = tmp, bucket = Sys.getenv("AWS_ACCESS_BUCKET"), object = filename)
}

# loads a single object from AWS.s3 - modified from s3load
load_aws_s3 <- function(filename)
{
  if (!find_aws_s3(filename))
    return(NULL)
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save_object(
    bucket = Sys.getenv("AWS_ACCESS_BUCKET"),
    object = filename,
    file = tmp
  )
  load(tmp, envir = .GlobalEnv)
  my_amazon_obj
}

# -------------
# STORAGE QUERY
# -------------

get_user_store_mode <- function()
{
  if (readline("Type 'Y' and press enter to use local storage.
Type anything else and press enter to use AWS storage.") == "Y")
    return("local")
  "cloud"
}

decide_store_mode <- function()
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
