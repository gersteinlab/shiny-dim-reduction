# This file tests perform_reduction.R.

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

source_sdr("perform_reduction.R")

# -----
# TESTS
# -----

# check if requests are valid
