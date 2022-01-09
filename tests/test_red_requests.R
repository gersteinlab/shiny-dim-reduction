# This file tests red_requests.R.
# Warning: test_make_requests.R must be run first!

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

source_sdr("red_requests.R")

# ----------------------
# PERFORM VALID REQUESTS
# ----------------------

val_res <- perform_reduction(val_req, 0)
# val_res <- perform_reduction(val_req, 1)
# val_res <- perform_reduction(val_req, 2)
