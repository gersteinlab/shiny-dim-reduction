# This file tests install.R. Note: must be run from a clean session.

# -----
# SETUP
# -----

source("app/install.R")

# -----
# TESTS
# -----

utils_test <- function()
{
  cat("--- Testing Utility Functions ---\n\n")

  cat_f("is_int(NULL): %s\n", is_int(NULL))
  cat_f("is_int(NA): %s\n", is_int(NA))
  cat_f("is_int(1:2): %s\n", is_int(1:2))
  cat_f("is_int(-1): %s\n", is_int(-1))
  cat_f("is_int(-1L): %s\n\n", is_int(-1L))

  cat_f("is_num(NULL): %s\n", is_num(NULL))
  cat_f("is_num(NA): %s\n", is_num(NA))
  cat_f("is_num(1:2): %s\n", is_num(1:2))
  cat_f("is_num(-1): %s\n", is_num(-1))
  cat_f("is_num(-1.5): %s\n\n", is_num(-1.5))

  cat_f("is_str(NULL): %s\n", is_str(NULL))
  cat_f("is_str(NA): %s\n", is_str(NA))
  cat_f("is_str(0): %s\n", is_str(0))
  cat_f("is_str(LETTERS): %s\n", is_str(LETTERS))
  cat_f("is_str('ABC'): %s\n\n", is_str("ABC"))

  t1 <- Sys.time()
  cat_f("Store current time: t1 = %s\n", t1)
  cat("Sleeping for 1 second ...\n")
  Sys.sleep(1)
  t2 <- Sys.time()
  cat_f("Store current time: t2 = %s\n", t2)
  cat_f("time_diff(t1, t2): %.4f\n\n", time_diff(t1, t2))

  cat_f("vec_str(numeric()): '%s'\n", vec_str(numeric()))
  cat_f("vec_str(NA): '%s'\n", vec_str(NA))
  cat_f("vec_str(1:5): '%s'\n", vec_str(1:5))
  cat_f("vec_str(c('A', 'B')): '%s'\n\n", vec_str(c("A", "B")))

  cat_f("len_n_list(3): %s\n", vec_str(len_n_list(3L)))
  letters_list <- empty_named_list(LETTERS)
  cat_f("length(empty_named_list(LETTERS)): %d\n", length(letters_list))
}

source_test <- function()
{
  cat("--- Functions Tested: get_source_loc ---\n\n")
  cat("Setting mode to app:\n")
  setwd("app")
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
utils_test()
cat("\n")
source_test()

# manually test installation of packages
# remove.packages("Rtsne")
# remove.packages("limma")
# remove.packages("VennDiagram")
# Sys.setenv('SHINY_PORT' = 100)
# source("app/install.R")
