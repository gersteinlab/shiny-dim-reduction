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
