# The purpose of this file is to cleanly display output of various types.
# Frequently used for test scripts.
# source("output_clean.R", encoding="UTF-8")

# prints a single line cleanly
print_clean <- function(msg){
  cat(sprintf("%s\n", msg))
}

# prints the result of sprintf cleanly
sprintf_clean <- function(...)
{
  print_clean(sprintf(...))
}

# returns the rounded elapsed system time since 'start'
my_timer <- function(start = 0, num_digits = 4){
  round(as.numeric(Sys.time()) - start, num_digits)
}
