# The purpose of this file is to manage Local / AWS storage.

use_local <- FALSE
if (Sys.getenv('SHINY_PORT') == "")
  use_local <- readline("Do you wish to run this app locally? (Y/N)") == 'Y'

# -------------
# LOCAL STORAGE
# -------------

# -------------
# S3AWS STORAGE
# -------------

# assigns Amazon Web Service keys
assign_keys <- function(aws_keys)
{
  if (length(aws_keys) != 3)
    stop("Not all AWS keys provided.")

  Sys.setenv("AWS_ACCESS_KEY_ID" = aws_keys[1],
             "AWS_SECRET_ACCESS_KEY" = aws_keys[2])
  assign("aws_bucket", aws_keys[3], envir = .GlobalEnv)
  invisible()
}

# saves a single object to AWS.s3 - modified from s3save
# assumes the existence of an object called 'aws_bucket'
custom_s3save <- function(my_amazon_obj, object)
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save(my_amazon_obj, file = tmp, envir = parent.frame())
  put_object(file = tmp, bucket = aws_bucket, object = object)
}

# loads a single object from AWS.s3 - modified from s3load
# assumes the existence of an object called 'aws_bucket'
custom_s3load <- function(object)
{
  tmp <- tempfile(fileext = ".rdata")
  on.exit(unlink(tmp))
  save_object(bucket = aws_bucket, object = object, file = tmp)
  load(tmp, envir = parent.frame())
  my_amazon_obj
}

# ------------
# OUTDATED AWS
# ------------

# saves an object to Amazon AWS, returning whether the process succeeded
# assumes the existence of an object called 'aws_bucket'
save_db <- function(dat, filename){
  my_amazon_obj <- dat
  s3save(my_amazon_obj, bucket=aws_bucket, object=filename)
  my_amazon_obj <- NULL
  length(evaluation) == 1
}

# loads an object from Amazon AWS
# assumes the existence of an object called 'aws_bucket'
load_db <- function(filename){
  s3load(filename, aws_bucket)
  my_amazon_obj
}
