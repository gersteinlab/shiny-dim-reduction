# This file tests workflows.R by creating a demo workflow and inspecting the paths / folders made.

# -----
# SETUP
# -----

source("install.R")
source_sdr("workflows.R")

# -----
# TESTS
# -----

# this test requires user interaction:
# -- set_workflow_root()
# -- -- enter an invalid directory
# -- -- enter 'Q'
# -- -- actually change the workflow root
# -- try quitting
# -- try 'exRNA'

sprintf_clean("Root Location: %s", wf_config$roo_loc)
sprintf_clean("Raw Location: %s", wf_config$raw_loc)
sprintf_clean("Processing Location: %s", wf_config$pro_loc)
sprintf_clean("Combined Location: %s", wf_config$com_loc)
sprintf_clean("Intermediate Location: %s", wf_config$int_loc)
sprintf_clean("Requests Location: %s", wf_config$req_loc)
sprintf_clean("Reference Location: %s", wf_config$ref_loc)
sprintf_clean("App Location: %s", wf_config$app_loc)
sprintf_clean("Dependencies Location: %s", wf_config$dep_loc)
