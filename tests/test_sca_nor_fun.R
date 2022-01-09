# This file tests sca_nor_fun.R.

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

source_sdr("sca_nor_fun.R")

# -----
# TESTS
# -----
