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

aws_test_1 <- function()
{
  print_clean("Functions Tested: custom_s3save, custom_s3load")
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

aws_test_2 <- function(num = 10)
{
  print_clean("Functions Tested: custom_s3save, custom_s3load, save_db, load_db")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"
  
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

my_timer_test <- function()
{
  print_clean("Functions Tested: my_timer")
  start <- my_timer()
  print_clean("Sleeping for 2 seconds ...")
  Sys.sleep(2)
  print_clean(sprintf("Time elapsed: %s", my_timer(start)))
}

my_empty_list_test <- function()
{
  print_clean("Functions Tested: my_empty_list")
  target <- vector(mode="list", length=10)
  names(target) <- sprintf("P%s", 1:10)
  
  print_clean("Test all.equal with a conventionally generated list:")
  print(all.equal(target, my_empty_list(names(target))))
  
  print_clean("Test NULL as a parameter:")
  print(my_empty_list(NULL))
}

auth_test <- function()
{
  print_clean("Functions Tested: my_hash, my_auth")
  username <- "Justin"
  password <- "123456"
  user_credentials <- list()
  user_credentials[[username]] <- my_hash(password)
  print_clean("Hashed password:")
  print(user_credentials[[username]])
  print_clean("Did authentication succeed for correct username / password?")
  print(my_auth(username, password, user_credentials))
  print_clean("Did authentication succeed for incorrect username?")
  print(my_auth("Joel", password, user_credentials))
  print_clean("Did authentication succeed for incorrect password?")
  print(my_auth(username, "234567", user_credentials))
  print_clean("Did authentication succeed for incorrect username / password?")
  print(my_auth("Joel", "234567", user_credentials))
  print_clean("What happens when NULL / NULL / NULL are the inputs?")
  print(my_auth(NULL, NULL, NULL))
  print_clean("What happens when credentials are absent?")
  print(my_auth(username, password, NULL))
}

aws_test_1()
aws_test_2()
my_timer_test()
my_empty_list_test()
auth_test()