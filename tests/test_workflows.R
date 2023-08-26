# This file tests workflows.R by creating demo workflows and
# inspecting the paths made.

# -----
# SETUP
# -----

source("pipeline/workflows.R")

test_workflow_locs <- function()
{
  cat_f("%s.Workflow: %s\n", get_workflow(), get_loc_wf())
  cat_f("-> %s.Table: %s\n", get_workflow(), get_loc_table())
  cat_f("-> %s.Inter: %s\n", get_workflow(), get_loc_inter())
  cat_f("-> %s.Store: %s\n", get_workflow(), get_loc_store())
  cat_f("-> %s.Reque: %s\n", get_workflow(), get_loc_reque())
  cat_f("-> %s.App-D: %s\n\n", get_workflow(), get_loc_app_d())
}

# --------------
# TEST WF CONFIG
# --------------

stopifnot(is_wf_config(wf_config))

# create the exRNA workflow folder
# note: if location is not specified and
# no current workflow exists, it will be
# created in the current working directory
upsert_workflow("exRNA", getwd())
set_workflow("exRNA")
cat_wf_config()
test_workflow_locs()

# change the location of the exRNA workflow
upsert_workflow("exRNA", "~/DataR/sdr_workflows/exRNA")
cat_wf_config()
test_workflow_locs()

# change the location again
upsert_workflow("exRNA", "~/DataR/sdr_workflows")
cat_wf_config()
test_workflow_locs()

# create another workflow
upsert_workflow("MNIST", "~/DataR/sdr_workflows")
cat_wf_config()
set_workflow("MNIST")
test_workflow_locs()

# you can delete workflows that don't exist
unlink_workflow("does_not_exist")
cat_wf_config()

# you can also make a workflow and delete it
upsert_workflow("temp", "~")
cat_wf_config()

unlink_workflow("temp")
cat_wf_config()

# see what load_wf_config yields
load_wf_config()
cat_wf_config()
test_workflow_locs()

# save_wf_config()

# --------------
# TEST WORKFLOWS
# --------------

set_workflow("exRNA")
test_workflow_locs()

set_workflow("MNIST")
test_workflow_locs()

