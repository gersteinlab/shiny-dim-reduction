# This file tests install.R.
# Note: must be run from a clean session.
# source("tests/test_install.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

start_time <- Sys.time()

# -----
# TESTS
# -----

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

test_that("all_fun_true() works", {
  all_fun_true(1:10, is_int) %>% expect_true()
  all_fun_true(LETTERS, is_str) %>% expect_true()
  all_fun_true(NULL, is_int) %>% expect_true()
  all_fun_true(list(), is_int) %>% expect_true()
})

test_that("are_safe_names() works", {
  are_safe_names(NULL) %>% expect_true()
  are_safe_names(character()) %>% expect_true()
})

test_that("has_safe_names() works", {
  has_safe_names(NULL) %>% expect_true()
  has_safe_names(setNames(letters, LETTERS)) %>% expect_true()
  has_safe_names(setNames(1:2, c("a", "a"))) %>% expect_false()
})

test_that("are_subsets() works", {
  are_subsets(list(), 13) %>% expect_true()
  are_subsets(list("A" = 1L), 1L) %>% expect_true()
  are_subsets(NULL, 0) %>% expect_false()
  are_subsets(character(), 0) %>% expect_false()
})

test_that("is_metadata() works", {
  is_metadata(data.frame("LETTERS" = LETTERS)) %>% expect_true()
})

test_that("are_colors() works", {
  are_colors(NULL) %>% expect_identical(logical())
  are_colors(NaN) %>% expect_false()
  "#######" %>% are_colors() %>% expect_false()
  "#BCDEFG" %>% are_colors() %>% expect_false()
  "ABCDEF" %>% are_colors() %>% expect_false()
  "ABCDEFF" %>% are_colors() %>% expect_false()
  "ABCDEFFF" %>% are_colors() %>% expect_false()
  "#ABCDEF" %>% are_colors() %>% expect_true()
  "#ABCDEG" %>% are_colors() %>% expect_false()
  "#ABCDEFF" %>% are_colors() %>% expect_false()
  "#ABCDEFFF" %>% are_colors() %>% expect_true()
  "#ABCDEFFG" %>% are_colors() %>% expect_false()
})

test_that("are_axes() works", {
  are_axes(list()) %>% expect_true()
})

test_that("are_categories() works", {
  are_categories(list()) %>% expect_true()
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

  exists("test_var") %>% expect_false()
  test_fun1()
  exists("test_var") %>% expect_false()
  test_fun2()
  exists("test_var") %>% expect_true()
  rm(test_var, envir = .GlobalEnv)
})

test_that("get_app_loc() works", {
  setwd("app")

  # local
  Sys.setenv("SHINY_PORT" = "")
  source("install.R")
  expect_identical(sdr_config$mode, "local")
  expect_null(sdr_config$path)
  expect_identical(get_app_loc("app.R"), "app.R")

  # cloud
  Sys.setenv("SHINY_PORT" = 100)
  source("install.R")
  expect_identical(sdr_config$mode, "cloud")
  expect_null(sdr_config$path)
  for (file in list.files("app"))
  {
    expect_match(get_app_loc(file), file, fixed = TRUE)
  }

  # pipeline
  setwd("..")
  source("app/install.R")
  expect_identical(sdr_config$mode, "pipeline")
  is.character(sdr_config$path) %>% expect_true()
  for (file in list.files("app"))
  {
    a_loc <- file.path("app", file)
    expect_match(get_app_loc(file), a_loc, fixed = TRUE)
  }

  expect_error(get_app_loc("nonexistent.R"))
})

# -------
# CLEANUP
# -------

# tests the printing methods
testing_time <- time_diff(start_time)
cat_f("TESTING TIME (seconds): %.1f\n", testing_time)
message_f("TESTING TIME (seconds): %.1f", testing_time)
stop_f("TESTING TIME (seconds): %.1f", testing_time)

# manually test installation of packages
# source("app/install.R")
# library(Rtsne)
# remove.packages("Rtsne")
# remove.packages("limma")
# remove.packages("VennDiagram")
