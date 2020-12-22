# This file tests outline.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("outline.R", encoding="UTF-8")

assign_keys(tester_keys)


# -----
# TESTS
# -----

# saves an object to Amazon AWS, returning whether the process succeeded
# assumes the existence of an object called 'aws_bucket'
save_db <- function(dat, filename){
  my_amazon_obj <- dat
  evaluation <- evaluate::evaluate(quote(
    s3save(my_amazon_obj, bucket=aws_bucket, object=filename)), stop_on_error = 1)
  my_amazon_obj <- NULL
  length(evaluation) == 1
}

# loads an object from Amazon AWS
# assumes the existence of an object called 'aws_bucket'
load_db <- function(filename){
  s3load(filename, aws_bucket)
  my_amazon_obj
}

test1 <- function()
{
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"
  
  r1 <- single_line_eval(custom_s3save(test_data, test_object))
  print_clean(sprintf("Save success? %s", r1))
  r2 <- single_line_eval(custom_s3save(test_data, NULL)) 
  print_clean(sprintf("Save failure? %s", r2))
  r3 <- single_line_eval(nrow(custom_s3load(test_object))) 
  print_clean(sprintf("Load success? %s", r3))
  r4 <- single_line_eval(custom_s3load(NULL)) 
  print_clean(sprintf("Load failure? %s", r4))
}

test2 <- function(num = 10)
{
  print(system.time({
    for (i in 1:num)
      custom_s3save(test_data, test_object)
  }))
  
  print(system.time({
    for (i in 1:num)
      custom_s3load(test_object)
  }))
  
  print(system.time({
    for (i in 1:num)
      save_db(test_data, test_object)
  }))
  
  print(system.time({
    for (i in 1:num)
      load_db(test_object)
  }))
}

test1()
test2()
