# This file tests text_work.R.

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

source_sdr("text_work.R")

text_test <- function()
{
  print_clean("*** Functions Tested: rep_str, reg_str ***")
  sprintf_clean("Replace 'A' in 'ABCAAAAA' with 'D': %s", rep_str("ABCAAAAA", "A", "D"))
  sprintf_clean("Replace '.' in 'ABCDE' with 'F': %s", reg_str("ABCDE", ".", "F"))
  print_clean()
}
