# The purpose of this file is to manage Local / AWS.S3 storage.
# source("storage.R", encoding="UTF-8")

# Goals:
# find_store: returns whether a file exists
# save_store: saves a file, creating the directory if it doesn't exist
# load_store: loads a file, returning NULL if it doesn't exist

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

require(aws.s3)

# --------------
# KEY MANAGEMENT
# --------------

# the expected location of the master key
master_key_loc <- get_project_loc("sdr_master_key.rds")

# sets the current working AWS access key, which comprises: id, secret, bucket
set_working_key <- function(key)
{
  if (class(key) != "list")
    stop("Provided keys are not a list.")

  if (!isTRUE(all.equal(names(key), c("id", "secret", "bucket"))))
    stop("Incorrect key components provided.")

  Sys.setenv("AWS_ACCESS_KEY_ID" = key$id,
             "AWS_SECRET_ACCESS_KEY" = key$secret,
             "AWS_ACCESS_BUCKET" = key$bucket)
}

# create a master key and save it in the project directory
save_master_key <- function(id, secret)
{
  sdr_master_key <- list("id" = id, "secret" = secret)
  saveRDS(sdr_master_key, master_key_loc)
}

# give the current working key MASTER KEY privileges
sudo_working_key <- function()
{
  sdr_master_key <- readRDS(master_key_loc)
  set_working_key(list(
    "id" = sdr_master_key$id,
    "secret" = sdr_master_key$secret,
    "bucket" = Sys.getenv("AWS_ACCESS_BUCKET")
  ))
}

# -------------
# LOCAL STORAGE
# -------------

# saveRDS but we force the creation of the directory
mkdir_saveRDS <- function(data, file, compress = TRUE)
{
  dest_dir <- dirname(file)
  if (!dir.exists(dest_dir))
    dir.create(dest_dir, recursive=TRUE)
  saveRDS(data, file, compress = compress)
}

# readRDS but returns a default if the file does not exist
w_def_readRDS <- function(file, default = NULL)
{
  if (file.exists(file))
    return(readRDS(file))
  default
}

# assigns the given value readRDS(loc) to a variable with the given name,
# assigning a default value if file.exists(loc) returns false.
get_from_loc <- function(name, loc, default = NULL)
{
  assign_global(var_name, w_def_readRDS(loc, default))
}

# wrapper for get_from_loc that assumes the file was saved with the variable name
get_self_loc <- function(name, dir = getwd(), default = NULL)
{
  get_from_loc(name, sprintf("%s/%s.rds", dir, name), default)
}

# wrapper for get_self_loc, only for dependencies
get_dependency <- function(name, default = NULL)
{
  if (sdr_from_app)
    return(get_self_loc(name, "dependencies", default))
  get_self_loc(name, dep_loc, default)
}

# performs saveRDS(loc) with the value of a variable with the given name,
# saving a default value if exists(name) returns false.
set_from_var <- function(name, loc, default = NULL, compress = TRUE)
{
  if (exists(name))
    default <- get(name)
  saveRDS(default, loc, compress = compress)
}

# wrapper for set_from_var that saves the file with the variable name
set_self_var <- function(name, dir = getwd(), default = NULL, compress = TRUE)
{
  set_from_var(name, sprintf("%s/%s.rds", dir, name), default, compress)
}

# wrapper for set_self_var, only for dependencies
set_dependency <- function(name, default = NULL, compress = TRUE)
{
  if (sdr_from_app)
    stop("Dependencies cannot be set from within the application.")
  set_self_var(name, dep_loc, default, compress)
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

# lists all files with the given prefix (usually a valid directory)
list_local <- function(prefix)
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
  mkdir_saveRDS(path_local(filename))
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

# lists the contents of a bucket's prefix
list_aws_s3 <- function(prefix)
{
  get_bucket(Sys.getenv("AWS_ACCESS_BUCKET"), prefix = prefix)
}

# determines whether a single object with the given filename exists
find_aws_s3 <- function(prefix)
{
  length(list_aws_s3(prefix)) == 1
}

# lol <- get_bucket(Sys.getenv("AWS_ACCESS_BUCKET"))
# bucket_names <- lapply(lol, function(x){
#   x$Key
# })
# bucket_table <- matrix(bucket_names, ncol = 1)

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

# queries the user for a storage type
storage_query <- function()
{
  user_local <- "N"
  if (sdr_running_local)
    user_local <- readline(prompt = "
Type 'Y' and press enter to use local storage.
Type anything else and press enter to use AWS storage. ")
  set_storage(user_local == "Y")
}
