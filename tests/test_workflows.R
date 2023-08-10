# This file tests workflows.R by creating demo workflows and inspecting the paths made.

# -----
# SETUP
# -----

source("pipeline/workflows.R")

test_workflow_locs <- function()
{
  cat_f("Workflow Location: %s\n", get_loc_wf())
  cat_f("-> Table Location: %s\n", get_loc_table())
  cat_f("-> Inter Location: %s\n", get_loc_inter())
  cat_f("-> Store Location: %s\n", get_loc_store())
  cat_f("-> Reque Location: %s\n", get_loc_reque())
}

# -----
# TESTS
# -----

stopifnot(is_wf_config(wf_config))

# create the exRNA workflow folder
# note: if location is not specified and
# no current workflow exists, it will be
# created in the current working directory
upsert_workflow("exRNA")
list_workflows()
test_workflow_locs()

# change the location of the exRNA workflow
upsert_workflow("exRNA", "~/DataR/sdr_workflows/exRNA")
list_workflows()
test_workflow_locs()

# change the location again
upsert_workflow("exRNA", "~/DataR/sdr_workflows")
list_workflows()
test_workflow_locs()

# if no location is specified, it will be
# created in the same folder as the current
# workflow ... note that upsert_workflow
# conveniently sets the current workflow too
upsert_workflow("MNIST")
list_workflows()
test_workflow_locs()

# you can set a workflow as well
set_current_workflow("exRNA")
list_workflows()
test_workflow_locs()

# you can delete workflows that don't exist
unlink_workflow("does_not_exist")
list_workflows()
test_workflow_locs()

# if you make a workflow and delete it,
# the current workflow will go back to the
# non-deleted workflow used most recently
upsert_workflow("temp")
list_workflows()
test_workflow_locs()

unlink_workflow("temp")
list_workflows()
test_workflow_locs()

# save_wf_config()

load_wf_config()
list_workflows()
test_workflow_locs()

