# This file tests red_requests.R.

# -----
# SETUP
# -----

source("tests/test_make_requests.R")
source("pipeline/red_requests.R")

# ----------------------
# PERFORM VALID REQUESTS
# ----------------------

system.time({
  test3 <- name_req_key_inters(app_requests[, 1:13])
})

perform_reduction(val_req, 0)
# perform_reduction(val_req, 1)
# perform_reduction(val_req, 2)
