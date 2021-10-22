# This file tests install.R.

# -----
# SETUP
# -----

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

# -----
# TESTS
# -----

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
  sprintf_clean("The result of empty_named_list(NULL) is: [%s] [expected: list()]",
                paste(empty_named_list(NULL), collapse = ", "))
}

# -------
# RUN ALL
# -------
basic_test()
