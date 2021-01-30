# This file tests storage.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("installer.R", encoding="UTF-8")
source("storage.R", encoding="UTF-8")

test_keys <- list(
  "id"="AKIAVI2HZGPOEH4RVQIH",
  "secret"="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",
  "bucket"="shiny-app-data-justin-test"
)

test_root <- "C:/Users/Justin Chang/Desktop/ProjectsR/Justin-Tool/test-root"

assign_keys(test_keys)
assign_root(test_root)

library(evaluate)
# checks if a single line evaluation is successful
single_line_eval <- function(single_line, check_warnings = TRUE, check_errors = TRUE)
{
  evaluation <- evaluate::evaluate(quote(single_line))
  no_warnings <- sum("warning" %in% unlist(lapply(evaluation, class))) == 0
  no_errors <- sum("error" %in% unlist(lapply(evaluation, class))) == 0
  (!check_warnings || no_warnings) && (!check_errors || no_errors)
}

# -----
# TESTS
# -----

aws_s3_test_1 <- function()
{
  print_clean("Functions Tested: find_aws_s3, save_aws_s3, load_aws_s3")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

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
}

aws_s3_test_2 <- function(num = 10)
{
  print_clean("Functions Tested: save_aws_s3, find_aws_s3, load_aws_s3")
  test_data <- matrix(1:10000, nrow=100)
  test_object <- "test"

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

# -------
# RUN ALL
# -------
aws_s3_test_1()
aws_s3_test_2()
