# The purpose of this file is to store functions for find / replace purposes.

require("stringi")

# for each string in "x_stringi", replaces each element of "pattern" with the
# corresponding element in "replacement" and returns an error if
# "pattern" and "replacement" are of different lengths
rep_str <- function(x_stringi, pattern, replacement)
{
  stopifnot(length(pattern) == length(replacement))
  stri_replace_all_fixed(
    x_stringi, pattern = pattern, replacement = replacement, vectorize_all = FALSE)
}

# for each string in "x_stringi" replaces each regex match of each element in
# "pattern" with the corresponding element in "replacement" and returns an
# error if "pattern" and "replacement" are of different lengths
reg_str <- function(x_stringi, pattern, replacement)
{
  stopifnot(length(pattern) == length(replacement))
  stri_replace_all_regex(
    x_stringi, pattern = pattern, replacement = replacement, vectorize_all = FALSE)
}

# adds a percent sign to a string or number
add_perc <- function(str)
{
  sprintf("%s%%", str)
}

# removes a percent sign from a string and converts to a number
rem_perc <- function(str)
{
  as.numeric(rep_str(str, "%", ""))
}
