# This file tests find_replace.R.

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

source_sdr("find_replace.R")

# -----
# TESTS
# -----

text_test <- function()
{
  print_clean("*** Functions Tested: rep_str, reg_str ***")
  sprintf_clean("Replace 'A' in 'ABCAAAAA' with 'D': %s", rep_str("ABCAAAAA", "A", "D"))
  sprintf_clean("Replace '.' in 'ABCDE' with 'F': %s", reg_str("ABCDE", ".", "F"))
  print_clean()
  print_clean("*** Functions Tested: add_perc, rem_perc ***")
  sprintf_clean("Add percent to 100: %s", add_perc(100))
  sprintf_clean("Remove percent from 100%%: %s", rem_perc("100%"))
}

# -------
# RUN ALL
# -------
text_test()
