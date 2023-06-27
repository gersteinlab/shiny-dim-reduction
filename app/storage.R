# The purpose of this file is to manage storage systems (stores).

# Goals:
# list_store: list all files with a prefix
# find_store: returns whether a file exists
# save_store: saves a file, creating the directory if it doesn't exist
# load_store: loads a file, returning NULL if it doesn't exist

if (!exists("sdr_config"))
  source("app/install.R")

library(aws.s3)

# ----------------
# LOCAL MANAGEMENT
# ----------------

#' syntactic sugar for saving a variable
#' in the current working directory
#'
#' @param name A string.
get_self_rds <- function(name)
{
  assign_global(name, readRDS(sprintf("%s.rds", name)))
}

#' syntactic sugar for loading a variable
#' from the current working directory
#'
#' @param name A string.
set_self_rds <- function(name)
{
  saveRDS(get(name), sprintf("%s.rds", name))
}

# -------------
# LOCAL STORAGE
# -------------

# sets the reference location used for local paths, does not check validity
set_working_ref <- function(loc = "this_is_not_a_file")
{
  Sys.setenv("LOCAL_STORAGE_REF" = loc) # the old ref is disconnected automatically
}

# determines whether R is connected to ref
ref_is_connected <- function()
{
  file.exists(Sys.getenv("LOCAL_STORAGE_REF"))
}

# checks if a reference is valid
is_valid_ref <- function(loc)
{
  is_str(loc) && file.exists(loc)
}

# gives the local storage path for a prefix (usually in reference)
path_local <- function(prefix)
{
  sprintf("%s/%s", Sys.getenv("LOCAL_STORAGE_REF"), prefix)
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
  w_def_readRDS(path_local(filename), NULL)
}

# --------------
# KEY MANAGEMENT
# --------------

# disconnects from AWS
disconnect_key <- function()
{
  Sys.setenv("AWS_ACCESS_KEY_ID" = "",
             "AWS_SECRET_ACCESS_KEY" = "",
             "AWS_ACCESS_BUCKET" = "")
}

# determines if R is connected to AWS
key_is_connected <- function()
{
  bucket_exists(Sys.getenv("AWS_ACCESS_BUCKET"))
}

# makes a list that qualifies as a key from an ID, a secret, and a bucket
make_key <- function(id = "", secret = "", bucket = "")
{
  stopifnot(is_str(id), is_str(secret), is_str(bucket))

  list(
    "id" = id,
    "secret" = secret,
    "bucket" = bucket
  )
}

# checks if a key is valid
is_valid_key <- function(key)
{
  (class(key) == "list") && isTRUE(all.equal(names(key), c("id", "secret", "bucket")))
}

# sets the current working AWS access key, which comprises: id, secret, bucket
set_working_key <- function(key = make_key())
{
  stopifnot(is_valid_key(key))
  disconnect_key()
  Sys.setenv("AWS_ACCESS_KEY_ID" = key$id,
             "AWS_SECRET_ACCESS_KEY" = key$secret,
             "AWS_ACCESS_BUCKET" = key$bucket)

  # if the provided keys don't allow bucket access
  if (!key_is_connected())
  {
    disconnect_key()
    stop("The provided keys do not correspond to a working bucket.")
  }
}

# create a master key and save it in the project directory
save_master_key <- function(id, secret)
{
  stopifnot(is_str(id), is_str(secret))
  master_key_loc <- get_project_loc("sdr_master_key.rds")
  sdr_master_key <- list("id" = id, "secret" = secret)
  saveRDS(sdr_master_key, master_key_loc)
}

# turns a key into a master key
sudo_key <- function(key)
{
  # to avoid master key privileges: don't include the master key file in apps!
  stopifnot(is_valid_key(key))
  master_key_loc <- get_project_loc("sdr_master_key.rds")
  sdr_master_key <- readRDS(master_key_loc)
  make_key(sdr_master_key$id, sdr_master_key$secret, key$bucket)
}

# wrapper for sudo_key that also assigns the key
sudo_working_key <- function(key)
{
  set_working_key(sudo_key(key))
}

# --------------
# AWS.S3 STORAGE
# --------------

my_amazon_obj <- NULL

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
  object_exists(filename, Sys.getenv("AWS_ACCESS_BUCKET"))
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

set_storage_local <- function(ref)
{
  set_working_ref(ref)
  assign_global("list_store", list_local)
  assign_global("find_store", find_local)
  assign_global("save_store", save_local)
  assign_global("load_store", load_local)
}

set_storage_aws <- function(key)
{
  set_working_key(key)
  assign_global("list_store", list_aws_s3)
  assign_global("find_store", find_aws_s3)
  assign_global("save_store", save_aws_s3)
  assign_global("load_store", load_aws_s3)
}

query_storage <- function(ref = NULL, key = NULL)
{
  if (sdr_config$mode != "cloud" && is_valid_ref(ref)) # can use local storage
  {
    if (is_valid_key(key)) # can use AWS storage
    {
      if ("Y" == readline(prompt = "
Type 'Y' and press enter to use local storage.
Type anything else and press enter to use AWS storage. "))
      {
        set_storage_local(ref)
        return(TRUE)
      }
      else
      {
        set_storage_aws(key)
        return(FALSE)
      }
    }
    else # cannot use AWS storage
    {
      set_storage_local(ref)
      return(TRUE)
    }
  }
  else # cannot use local storage
  {
    if (is_valid_key(key)) # can use AWS storage
    {
      set_storage_aws(key)
      return(FALSE)
    }
    else # cannot use AWS storage
    {
      stop("Neither local storage nor AWS storage are available.")
    }
  }
}
