# This file tests find_replace.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("find_replace.R", encoding="UTF-8")
source("installer.R", encoding="UTF-8")

# -----
# TESTS
# -----

text_test <- function()
{
  print_clean("Functions Tested: rep_str, reg_str")
  sprintf_clean("Replace 'A' in 'ABCAAAAA' with 'D': %s", rep_str("ABCAAAAA", "A", "D"))
  sprintf_clean("Replace '.' in 'ABCDE' with 'F': %s", reg_str("ABCDE", ".", "F"))
  print_clean("Functions Tested: add_perc, rem_perc")
  sprintf_clean("Add percent to 100: %s", add_perc(100))
  sprintf_clean("Remove percent from 100%%: %s", rem_perc("100%"))
}

# -------
# RUN ALL
# -------
text_test()
