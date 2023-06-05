# This file tests install.R. Note: must be run from a clean session.
# source("tests/test_install.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

# -----
# TESTS
# -----

utils_test <- function()
{
  cat("--- Testing Utility Functions ---\n\n")

  test_that("all_fin() works", {
    all_fin(NA) %>% expect_false()
    all_fin(NaN) %>% expect_false()
    all_fin(Inf) %>% expect_false()
    all_fin(-Inf) %>% expect_false()
    all_fin("") %>% expect_false()

    all_fin(NULL) %>% expect_true()
    all_fin(-1) %>% expect_true()
    all_fin(-1L) %>% expect_true()
    all_fin(pi) %>% expect_true()

    all_fin(numeric()) %>% expect_true()
    all_fin(1:4) %>% expect_true()
    all_fin(c(1:3, NaN)) %>% expect_false()
    all_fin(LETTERS) %>% expect_false()

    all_fin(matrix(numeric())) %>% expect_true()
    all_fin(matrix(1:4, nrow = 2)) %>% expect_true()
    all_fin(matrix(c(1:3, NaN), nrow = 2)) %>% expect_false()
    all_fin(matrix()) %>% expect_false()
  })

  test_that("is_int() works", {
    is_int(NA) %>% expect_false()
    is_int(NaN) %>% expect_false()
    is_int(Inf) %>% expect_false()
    is_int(-Inf) %>% expect_false()
    is_int("") %>% expect_false()
    is_int(NULL) %>% expect_false()
    is_int(-1) %>% expect_false()
    is_int(pi) %>% expect_false()
    is_int(1:2) %>% expect_false()
    is_int(-1L) %>% expect_true()
  })

  test_that("is_num() works", {
    is_num(NA) %>% expect_false()
    is_num("") %>% expect_false()
    is_num(NULL) %>% expect_false()
    is_num(1:2) %>% expect_false()

    is_num(NaN) %>% expect_true()
    is_num(Inf) %>% expect_true()
    is_num(-Inf) %>% expect_true()
    is_num(-1) %>% expect_true()
    is_num(pi) %>% expect_true()
    is_num(-1L) %>% expect_true()
  })

  test_that("is_str() works", {
    is_str(NULL) %>% expect_false()
    is_str(NA) %>% expect_false()
    is_str(0) %>% expect_false()
    is_str(LETTERS) %>% expect_false()
    is_str("") %>% expect_true()
    is_str("ABC") %>% expect_true()
  })

  test_that("time_diff() works", {
    t1 <- Sys.time()
    Sys.sleep(0.2)
    t2 <- Sys.time()
    Sys.sleep(0.2)
    e1 <- time_diff(t1, t2)
    e2 <- time_diff(t1)

    is.numeric(e1) %>% expect_true()
    is.numeric(e2) %>% expect_true()
    expect_equal(e1, 0.2, tolerance = 0.1)
    expect_equal(e2, 0.4, tolerance = 0.1)
  })

  test_that("vec_str() works", {
    expect_identical(vec_str(numeric()), "")
    expect_identical(vec_str(NA), "NA")
    expect_identical(vec_str(1:5), "1, 2, 3, 4, 5")
    letters5 <- letters[1:3]
    expect_identical(vec_str(letters5), "a, b, c")
    expect_error(vec_str(matrix()))
  })

  test_that("len_n_list() works", {
    for (i in seq_len(100))
      expect_length(len_n_list(i), i)
  })

  test_that("empty_named_list() works", {
    expect_identical(names(empty_named_list(LETTERS)), LETTERS)
  })

  test_that("assign_global() works", {
    test_fun1 <- function() {
      test_var <- 1
    }

    test_fun2 <- function() {
      assign_global("test_var", 1)
    }

    expect_false(exists("test_var"))
    test_fun1()
    expect_false(exists("test_var"))
    test_fun2()
    expect_true(exists("test_var"))
    rm(test_var, envir = .GlobalEnv)
  })
}

source_test <- function()
{
  cat("--- Testing Source Functions ---\n\n")
  setwd("app")

  cat("Setting mode to local:\n")
  Sys.setenv("SHINY_PORT" = "")
  source("install.R")
  cat_f("get_source_loc('find_replace.R'): %s\n", get_source_loc("find_replace.R"))

  cat("Setting mode to cloud:\n")
  Sys.setenv("SHINY_PORT" = 100)
  source("install.R")
  cat_f("get_source_loc('find_replace.R'): %s\n", get_source_loc("find_replace.R"))

  cat("Setting mode to pipeline:\n")
  setwd("..")
  source("app/install.R")
  cat_f("get_source_loc('find_replace.R'): %s\n", get_source_loc("find_replace.R"))
  cat_f("get_source_loc('packaging.R'): %s\n", get_source_loc("packaging.R"))
}

# -------
# RUN ALL
# -------

start_time <- Sys.time()
utils_test()
source_test()
message_f("TESTING TIME (seconds): %.2f", time_diff(start_time))

# manually test installation of packages
# source("app/install.R")
# library(Rtsne)
# remove.packages("Rtsne")
# remove.packages("limma")
# remove.packages("VennDiagram")
