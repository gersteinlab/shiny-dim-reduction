# This file tests preprocess.R.
# source("tests/test_preprocess.R")

# -----
# SETUP
# -----

library(testthat)

source("app/preprocess.R")
start_time <- Sys.time()

# -----
# TESTS
# -----

test_that("are_groups() works", {
  are_groups(list()) %>% expect_true()
})


test_that("groups_match_categories() works", {
  groups_match_categories(list(), list()) %>% expect_true()
})
