# This file tests storage.R.
# Note: storage_query() is not tested in detail because it is dependent on the app.
# Note: You will need a set of AWS master keys to perform these tests; use save_master_key().

# -----
# SETUP
# -----

source("install.R")
source_sdr("storage.R")

test_keys <- list(
  "id" = "to-be-sudo",
  "secret" = "to-be-sudo",
  "bucket" = "shiny-app-data-justin-test"
)

test_root <- "C:/Users/justin/Desktop/CodeR/DataR/sdr_workflows/test/reference"

# ---------
# AWS TESTS
# ---------

library(evaluate)

# checks if a single line evaluation is successful
single_line_eval <- function(single_line, check_warnings = TRUE, check_errors = TRUE)
{
  evaluation <- evaluate::evaluate(quote(single_line))
  no_warnings <- sum("warning" %in% unlist(lapply(evaluation, class))) == 0
  no_errors <- sum("error" %in% unlist(lapply(evaluation, class))) == 0
  (!check_warnings || no_warnings) && (!check_errors || no_errors)
}

aws_s3_test_1 <- function()
{
  print_clean("Functions Tested: list_aws_s3, find_aws_s3, save_aws_s3, load_aws_s3")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

  sudo_working_key(test_keys)

  r1 <- single_line_eval(save_aws_s3(test_data, test_object))
  sprintf_clean("Does normal saving work? %s", r1)

  r2 <- single_line_eval(save_aws_s3(test_data, NULL))
  sprintf_clean("Does saving to NULL work? %s", r2)

  r3 <- nrow(load_aws_s3(test_object)) == nrow(test_data)
  sprintf_clean("Is row number preserved? %s", r3)

  r4 <- single_line_eval(load_aws_s3(NULL))
  sprintf_clean("Does loading NULL work? %s", r4)

  r5 <- find_aws_s3(test_object)
  sprintf_clean("Does finding normally work? %s", r5)

  r6 <- find_aws_s3("this_is_not_a_file")
  sprintf_clean("Does finding a non-existent file work? %s", r6)

  save_aws_s3(test_data, "t2.rds")
  save_aws_s3(test_data, "t3.rds")

  sprintf_clean("We should have 3 saved files.
Expected [t2.rds, t3.rds, test], Received [%s]", paste(list_aws_s3(""), collapse = ", "))
}

aws_s3_test_2 <- function(num = 10)
{
  print_clean("Functions Tested: find_aws_s3, save_aws_s3, load_aws_s3")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

  sudo_working_key(test_keys)

  print(system.time({
    for (i in 1:num)
      save_aws_s3(test_data, test_object)
  }))

  print(system.time({
    for (i in 1:num)
      find_aws_s3(test_object)
  }))

  print(system.time({
    for (i in 1:num)
      load_aws_s3(test_object)
  }))
}

aws_s3_test_1()
print_clean()
aws_s3_test_2()
print_clean()

# -----------
# LOCAL TESTS
# -----------

local_test <- function()
{
  print_clean("Functions Tested: list_local, find_local, save_local, load_local")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

  set_working_ref(test_root)

  r1 <- single_line_eval(save_local(test_data, test_object))
  sprintf_clean("Does normal saving work? %s", r1)

  r2 <- single_line_eval(save_local(test_data, NULL))
  sprintf_clean("Does saving to NULL work? %s", r2)

  r3 <- nrow(load_local(test_object)) == nrow(test_data)
  sprintf_clean("Is row number preserved? %s", r3)

  r4 <- single_line_eval(load_local(NULL))
  sprintf_clean("Does loading NULL work? %s", r4)

  r5 <- find_local(test_object)
  sprintf_clean("Does finding normally work? %s", r5)

  r6 <- find_local("this_is_not_a_file")
  sprintf_clean("Does finding a non-existent file work? %s", r6)

  save_local(test_data, "t2.rds")
  save_local(test_data, "t3.rds")

  sprintf_clean("We should have 3 saved files.
Expected [t2.rds, t3.rds, test], Received [%s]", paste(list_local(""), collapse = ", "))
}

local_test()
print_clean()

# --------------
# COMBINED TESTS
# --------------

store_test <- function()
{
  print_clean("Functions Tested: list_store, find_store, save_store, load_store")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

  r1 <- single_line_eval(save_store(test_data, test_object))
  sprintf_clean("Does normal saving work? %s", r1)

  r2 <- single_line_eval(save_store(test_data, NULL))
  sprintf_clean("Does saving to NULL work? %s", r2)

  r3 <- nrow(load_store(test_object)) == nrow(test_data)
  sprintf_clean("Is row number preserved? %s", r3)

  r4 <- single_line_eval(load_store(NULL))
  sprintf_clean("Does loading NULL work? %s", r4)

  r5 <- find_store(test_object)
  sprintf_clean("Does finding normally work? %s", r5)

  r6 <- find_store("this_is_not_a_file")
  sprintf_clean("Does finding a non-existent file work? %s", r6)

  save_store(test_data, "t2.rds")
  save_store(test_data, "t3.rds")

  sprintf_clean("We should have 3 saved files.
Expected [t2.rds, t3.rds, test], Received [%s]", paste(list_store(""), collapse = ", "))
}

disconnect_key()
set_storage_aws(sudo_key(test_keys))
store_test()
print_clean()

disconnect_ref()
set_storage_local(test_root)
store_test()
print_clean()
