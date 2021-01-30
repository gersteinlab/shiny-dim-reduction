# The purpose of this file is to store functions for find / replace purposes.

require("stringi")

# fixed pattern replacement in a vector of strings
repStr <- function(x_stringi, pattern, replacement)
{
  stri_replace_all_fixed(
    x_stringi, pattern = pattern, replacement = replacement, vectorize_all = FALSE)
}

# regex pattern replacement in a vector of strings
regStr <- function(x_stringi, pattern, replacement)
{
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
  as.numeric(repStr(str, "%", ""))
}
