# This file tests install.R. Note: must be run from a clean session.

# -----
# SETUP
# -----

source("install.R")

# -----
# TESTS
# -----

test_assign <- function()
{
  assign_global("test_assign_var", 42)
}

basic_test <- function()
{
  print_clean("*** Testing Basic Functions ***")
  print_clean("This should print cleanly!")
  print_clean()
  sprintf_clean("The current time is: %s", Sys.time())
  start <- my_timer()
  print_clean("Sleeping for 1 second ...")
  Sys.sleep(1)
  sprintf_clean("Now, my_timer(start) yields: %s", my_timer(start))
  print_clean()
  sprintf_clean("The result of len_n_list(3) is: %s", paste(len_n_list(3), collapse = ", "))
  sprintf_clean("The names of empty_named_list(c(\"A\", \"B\", \"C\")) are: %s",
                paste(names(empty_named_list(c("A", "B", "C"))), collapse = ", "))
  sprintf_clean("The names of empty_named_list(\"A\", \"B\", \"C\") are: %s",
                paste(names(empty_named_list("A", "B", "C")), collapse = ", "))

  test_assign()
  sprintf_clean("The result of test_assign() is: %s [expected: 42]",
                test_assign_var)
  rm(test_assign_var, envir = .GlobalEnv)

}

get_source_loc_test <- function()
{
  print_clean("*** Functions Tested: get_source_loc ***")
  print_clean("Setting sdr_from_app to TRUE")
  assign_global("sdr_from_app", TRUE)
  sprintf_clean("get_source_loc(\"find_replace.R\"): %s", get_source_loc("find_replace.R"))
  print_clean("Setting sdr_from_app to FALSE")
  assign_global("sdr_from_app", FALSE)
  sprintf_clean("get_source_loc(\"find_replace.R\"): %s", get_source_loc("find_replace.R"))
}

attempt_install_test <- function()
{
  print_clean("*** INSTALLATION TEST ***")

  print_clean("Removing VennDiagram (make sure it's not attached!) ... ")
  remove.packages("VennDiagram")

  print_clean("Does installation work?")
  source("install.R")
}

# -------
# RUN ALL
# -------
basic_test()
print_clean()
get_source_loc_test()
attempt_install_test()

# manually test if installation can be done on a server
# remove.packages("VennDiagram")
# Sys.setenv('SHINY_PORT' = 100)
# source("install.R")
