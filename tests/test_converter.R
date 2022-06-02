# This file tests converter.R.

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

source_sdr("converter.R")

# -----
# TESTS
# -----

ex_df <- data.frame("A" = 1:100, "B" = 1:100 / 2, "C" = 1:100 / 3)
sprintf_clean("The result of ind_sd_top(ex_df, 2) is %s", paste(ind_sd_top(ex_df, 2), collapse = ", "))
