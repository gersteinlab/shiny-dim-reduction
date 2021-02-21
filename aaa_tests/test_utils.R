# This file tests utils.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("utils.R", encoding="UTF-8")

# -----
# TESTS
# -----

my_timer_test <- function()
{
  print_clean("Functions Tested: my_timer")
  start <- my_timer()
  print_clean("Sleeping for 2 seconds ...")
  Sys.sleep(2)
  print_clean(sprintf("Time elapsed: %s", my_timer(start)))
}

my_empty_list_test <- function()
{
  print_clean("Functions Tested: my_empty_list")
  target <- vector(mode="list", length=10)
  names(target) <- sprintf("P%s", 1:10)

  print_clean("Test all.equal with a conventionally generated list:")
  print(all.equal(target, my_empty_list(names(target))))

  print_clean("Test NULL as a parameter:")
  print(my_empty_list(NULL))
}

# -------
# RUN ALL
# -------
my_timer_test()
my_empty_list_test()
