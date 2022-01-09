# This file tests workflows.R by creating a demo workflow and inspecting the paths / folders made.

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

sprintf_clean("Root Location: %s", roo_loc)
sprintf_clean("Raw Location: %s", raw_loc)
sprintf_clean("Processing Location: %s", pro_loc)
sprintf_clean("Combined Location: %s", com_loc)
sprintf_clean("Intermediate Location: %s", int_loc)
sprintf_clean("Reference Location: %s", ref_loc)
sprintf_clean("App Location: %s", app_loc)
sprintf_clean("Dependencies Location: %s", dep_loc)
