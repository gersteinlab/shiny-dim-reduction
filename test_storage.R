# This file tests storage.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("storage.R", encoding="UTF-8")

# keys linked to a test user
tester_keys <- list(
  "id"="AKIAVI2HZGPOEH4RVQIH",
  "secret"="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",
  "bucket"="shiny-app-data-justin-test"
)

test_root <- "C:/Users/Justin Chang/Desktop/ProjectsR/Justin-Tool/test-root"

assign_keys(tester_keys)

# -----
# TESTS
# -----

aws_test_1 <- function()
{
  print_clean("Functions Tested: custom_s3save, custom_s3load")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

  r1 <- save_s3(test_data, test_object)
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
  print_clean("Functions Tested: save_s3, load_s3, find_s3, save_db, load_db")
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

# -------
# RUN ALL
# -------
aws_test_1()
aws_test_2()
