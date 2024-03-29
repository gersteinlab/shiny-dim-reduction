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

set_workflow("exRNA")

cat("perform_reduction(val_req)\n")
print(system.time({
  perform_reduction(val_req)
}))

cat("perform_reduction(val_req, 0)\n")
print(system.time({
  perform_reduction(val_req, 0)
}))

# print(system.time({perform_reduction(val_req, 1)}))
# print(system.time({perform_reduction(val_req, 2)}))
