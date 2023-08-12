# This file tests storage.R.
# source("tests/test_storage.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

source("app/storage.R")
start_time <- Sys.time()

# you will have to manage these stores yourself
test_local_store <- "~/shiny-dim-reduction"
test_cloud_store <- readRDS("tests/test_cloud_store.rds")

# -----------
# LOCAL TESTS
# -----------

test_that("is_local_store() works", {
  is_local_store(test_local_store) %>% expect_true()
})

test_that("make_local_store() works", {
  expect_identical(test_local_store, make_local_store(test_local_store))
  expect_error(make_local_store(FALSE))
})

set_local_store(test_local_store)

test_that("set_local_store() works", {
  expect_identical(Sys.getenv("LOCAL_STORE"), test_local_store)
})

test_that("local_connected() works", {
  local_connected() %>% expect_true()
})

test_that("list_local() works", {
  ("conventions.R" %in% list_local()) %>% expect_true()
  ("app.R" %in% list_local()) %>% expect_false()
  ("app.R" %in% list_local("app")) %>% expect_true()
  ("red_methods.R" %in% list_local("pipeline")) %>% expect_true()
  list_local(FALSE) %>% expect_error()
})

test_that("find_local() works", {
  find_local("conventions.R") %>% expect_true()
  find_local("app.R") %>% expect_false()
  find_local("app/app.R") %>% expect_true()
  find_local(FALSE) %>% expect_error()
})

test_that("save_local(), load_local(), delete_local() works", {
  data <- matrix(runif(100000), nrow = 100) # 1000 x 100
  file <- "tests/test_data.rds"
  def_data <- matrix(runif(1000), nrow = 10) # 100 x 10

  delete_local(file)
  find_local(file) %>% expect_false()

  # saving
  save_start <- Sys.time()
  save_local(data, file)
  cat_f("LOCAL SAVE TIME: %.1f (sec)\n", time_diff(save_start))

  find_local(file) %>% expect_true()

  # loading
  load_start <- Sys.time()
  data2 <- load_local(file)
  cat_f("LOCAL LOAD TIME: %.1f (sec)\n", time_diff(load_start))

  expect_identical(data, data2)
  delete_local(file)
  find_local(file) %>% expect_false()

  load_local(NULL) %>% expect_error()
  data3 <- load_local("this_is_not_a_file", def_data)
  expect_identical(def_data, data3)
})

# -----------
# CLOUD TESTS
# -----------

test_that("is_cloud_store() works", {
  is_cloud_store(test_cloud_store) %>% expect_true()
})

test_that("make_cloud_store() works", {
  expect_identical(test_cloud_store, do.call(make_cloud_store, test_cloud_store))
  expect_error(make_cloud_store(FALSE, NULL, NaN))
})

set_cloud_store(test_cloud_store)

test_that("set_cloud_store() works", {
  expect_identical(Sys.getenv("AWS_ACCESS_KEY_ID"), test_cloud_store$id)
  expect_identical(Sys.getenv("AWS_SECRET_ACCESS_KEY"), test_cloud_store$secret)
  expect_identical(Sys.getenv("AWS_ACCESS_BUCKET"), test_cloud_store$bucket)
})

test_that("cloud_connected() works", {
  cloud_connected() %>% expect_true()
})

test_that("summarize_bucket() works", {
  if (system2("aws", "--version") == 0)
    summarize_bucket() %>% expect_identical(0L)
})

test_that("list_cloud() works", {
  files <- c("t1.rds", "t2.rds", "t3.rds", "dogs/shiba.rds")
  for (file in files)
    save_cloud(runif(1), file)

  all(files %in% list_cloud()) %>% expect_true()
  expect_identical(list_cloud("dogs"), "shiba.rds")
  expect_identical(list_cloud("cats"), character())
  list_cloud(FALSE) %>% expect_error()
})

test_that("find_cloud() works", {
  find_cloud(FALSE) %>% expect_error()
})

test_that("find_cloud(), save_cloud(), load_cloud(), delete_cloud() works", {
  data <- matrix(runif(100000), nrow = 1000) # 1000 x 100
  file <- "test_data.rds"
  def_data <- matrix(runif(100), nrow = 10) # 100 x 10

  delete_cloud(file)
  find_cloud(file) %>% expect_false()

  # saving
  save_start <- Sys.time()
  save_cloud(data, file)
  cat_f("CLOUD SAVE TIME: %.1f (sec)\n", time_diff(save_start))

  find_cloud(file) %>% expect_true()

  # loading
  load_start <- Sys.time()
  data2 <- load_cloud(file)
  cat_f("CLOUD LOAD TIME: %.1f (sec)\n", time_diff(load_start))

  expect_identical(data, data2)
  delete_cloud(file)
  find_cloud(file) %>% expect_false()

  load_cloud(NULL) %>% expect_error()
  data3 <- load_cloud("this_is_not_a_file", def_data)
  expect_identical(def_data, data3)
})

# --------------
# COMBINED TESTS
# --------------

set_store_mode("local")

test_that("local store_mode works", {
  find_store(FALSE) %>% expect_error()

  data <- matrix(runif(100000), nrow = 1000) # 1000 x 100
  file <- "combined_test_data.rds"
  def_data <- matrix(runif(100), nrow = 10) # 100 x 10

  delete_store(file)
  find_store(file) %>% expect_false()

  # saving
  save_start <- Sys.time()
  save_store(data, file)
  cat_f("STORE SAVE TIME [%s]: %.1f (sec)\n",
        Sys.getenv("SDR_STORE_MODE"), time_diff(save_start))

  find_store(file) %>% expect_true()

  # loading
  load_start <- Sys.time()
  data2 <- load_store(file)
  cat_f("STORE LOAD TIME [%s]: %.1f (sec)\n",
        Sys.getenv("SDR_STORE_MODE"), time_diff(load_start))

  expect_identical(data, data2)
  delete_store(file)
  find_store(file) %>% expect_false()

  load_store(NULL) %>% expect_error()
  data3 <- load_store("this_is_not_a_file", def_data)
  expect_identical(def_data, data3)
})

set_store_mode("cloud")

test_that("cloud store_mode works", {
  find_store(FALSE) %>% expect_error()

  data <- matrix(runif(100000), nrow = 1000) # 1000 x 100
  file <- "combined_test_data.rds"
  def_data <- matrix(runif(100), nrow = 10) # 100 x 10

  delete_store(file)
  find_store(file) %>% expect_false()

  # saving
  save_start <- Sys.time()
  save_store(data, file)
  cat_f("STORE SAVE TIME [%s]: %.1f (sec)\n",
        Sys.getenv("SDR_STORE_MODE"), time_diff(save_start))

  find_store(file) %>% expect_true()

  # loading
  load_start <- Sys.time()
  data2 <- load_store(file)
  cat_f("STORE LOAD TIME [%s]: %.1f (sec)\n",
        Sys.getenv("SDR_STORE_MODE"), time_diff(load_start))

  expect_identical(data, data2)
  delete_store(file)
  find_store(file) %>% expect_false()

  load_store(NULL) %>% expect_error()
  data3 <- load_store("this_is_not_a_file", def_data)
  expect_identical(def_data, data3)
})

load_stores()

# -------
# CLEANUP
# -------

message_f("TESTING TIME (seconds): %.1f", time_diff(start_time))
