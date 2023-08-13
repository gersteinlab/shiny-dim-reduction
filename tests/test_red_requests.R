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

# is_done <- file.exists(requests$FILE_LOCATION)

# OPTION 1: keep the most recent time
# requests$TIME_COMPLETED[is_done] <- max(
#   requests$TIME_COMPLETED[is_done],
#   get_time_completed(requests$FILE_LOCATION[is_done])
# )

# OPTION 2: keep the earliest time
# requests$TIME_COMPLETED[is_done] <- min(
#   requests$TIME_COMPLETED[is_done],
#   get_time_completed(requests$FILE_LOCATION[is_done])
# )
