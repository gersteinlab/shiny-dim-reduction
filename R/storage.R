# The purpose of this file is to manage Local / AWS.S3 storage.
# source("storage.R", encoding="UTF-8")

# Goals:
# find_store: returns whether a file exists
# save_store: saves a file, creating the directory if it doesn't exist
# load_store: loads a file, returning NULL if it doesn't exist

stopifnot(ran_install)
require(aws.s3)

# -------------
# LOCAL STORAGE
# -------------

master_key_loc <- get_project_loc("sdr_master_key.rds")

# create a master key and save it in the project directory
save_master_key <- function(id, secret)
{
  sdr_master_key <- list("id" = id, "secret" = secret)
  saveRDS(sdr_master_key, master_key_loc)
}

# load a master key from the project directory
load_master_key <- function()
{
  sdr_master_key <- readRDS(master_key_loc)
  assign("master_keys", sdr_master_key, envir = .GlobalEnv)
}

# assigns a root directory for local storage
assign_root <- function(root)
{
  stopifnot(length(root) == 1, is.character(root), dir.exists(root))
  Sys.setenv("LOCAL_STORAGE_ROOT" = root)
}

path_local <- function(filename)
{
  sprintf("%s/%s", Sys.getenv("LOCAL_STORAGE_ROOT"), filename)
}

# determines whether a file with the given filename exists
find_local <- function(filename)
{
  file.exists(path_local(filename))
}

# saves data to filename in the root directory
save_local <- function(data, filename)
{
  file <- path_local(filename)
  if (!dir.exists(dirname(file)))
    dir.create(dirname(file), recursive=TRUE)
  saveRDS(data, file)
}

# loads data from filename in the root directory
load_local <- function(filename)
{
  if (!find_local(filename))
    return(NULL)
  readRDS(path_local(filename))
}

# --------------
# AWS.S3 STORAGE
# --------------

my_amazon_obj <- NULL

# assigns keys for AWS.S3 - must be used first
assign_keys <- function(keys)
{
  if (class(keys) != "list")
    stop("Provided keys are not a list.")

  if (!isTRUE(all.equal(names(keys), c("id", "secret", "bucket"))))
    stop("Incorrect key components provided.")

  Sys.setenv("AWS_ACCESS_KEY_ID" = keys$id,
             "AWS_SECRET_ACCESS_KEY" = keys$secret,
             "AWS_ACCESS_BUCKET" = keys$bucket)
}

# determines whether a single object with the given filename exists
find_aws_s3 <- function(filename)
{
  length(get_bucket(Sys.getenv("AWS_ACCESS_BUCKET"), prefix=filename)) == 1
}

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
  save_object(bucket = Sys.getenv("AWS_ACCESS_BUCKET"), object = filename, file = tmp)
  load(tmp, envir = .GlobalEnv)
  my_amazon_obj
}

# ------------------
# STORAGE ASSIGNMENT
# ------------------

# changes the storage type to local or AWS
set_storage <- function(use_local)
{
  if (use_local)
  {
    assign("find_store", find_local, envir=.GlobalEnv)
    assign("save_store", save_local, envir=.GlobalEnv)
    assign("load_store", load_local, envir=.GlobalEnv)
  }
  else
  {
    assign("find_store", find_aws_s3, envir=.GlobalEnv)
    assign("save_store", save_aws_s3, envir=.GlobalEnv)
    assign("load_store", load_aws_s3, envir=.GlobalEnv)
  }
}

# checks if the script is running locally
is_local <- function()
{
  sdr_project_loc && dir.exists(Sys.getenv("LOCAL_STORAGE_ROOT"))
}

# queries the user for a storage type
storage_query <- function()
{
  user_local <- "N"
  if (is_local())
    user_local <- readline("Type 'Y' and press enter to use local storage.
Type anything else and press enter to use AWS storage.")
  set_storage(user_local == "Y")
}
