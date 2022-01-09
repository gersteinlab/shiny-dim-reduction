# This file stores all functions and constants that should be preserved
# across all files, from data validation to processing to the tool.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}
