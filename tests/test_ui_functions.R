# This file tests ui_functions.R.
# source("tests/test_ui_functions.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

source("app/ui_functions.R")
start_time <- Sys.time()

# -----
# TESTS
# -----

test_that("rep_str() works", {
  expect_identical("DBCDDDDD", rep_str("ABCAAAAA", "A", "D"))
})

test_that("reg_str() works", {
  expect_identical("FFFFF", reg_str("ABCDE", ".", "F"))
})

test_that("str_ind() works", {
  str_ind("ABCD", 1) %>% expect_identical("A")
  str_ind("ABCD", 2) %>% expect_identical("B")
})

test_that("get_opt(), sep_opt() work", {
  opt_names <- c("A", "A ()", "A (B)")
  opt_nums <- c(1L, 2L, 34567L)

  for (i in 1:3)
  {
    a <- opt_names[i]
    b <- opt_nums[i]
    expect_identical(c(a, b), sep_opt(get_opt(a, b)))
  }

  sep_opt("()()(()) (((())))") %>% expect_identical(c("()()(())", "((()))"))
})

# -------
# CLEANUP
# -------

message_f("TESTING TIME (seconds): %.1f", time_diff(start_time))
