# The purpose of this file is to store various UI-related functions for the app.

if (!exists("sdr_config"))
  source("app/install.R")

library(stringi)

# --------------
# FIND / REPLACE
# --------------

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

# --------------
# OPT MANAGEMENT
# --------------

# formats to "opt_name (opt_num)"
get_opt <- function(opt_name, opt_num)
{
  stopifnot(is.character(opt_name), is.integer(opt_num))
  sprintf("%s (%s)", opt_name, opt_num)
}

# gets the ith character of string s
str_ind <- function(s, i)
{
  substring(s, i, i)
}

# separates a single string opt
sep_opt_str <- function(opt_str)
{
  stopifnot(is_str(opt_str))
  n <- nchar(opt_str)

  stopifnot(str_ind(opt_str, n) == ")")
  n_open <- 1

  for (i in (n-1):2)
  {
    c <- str_ind(opt_str, i)
    if (c == ")")
      n_open <- n_open + 1
    if (c == "(")
      n_open <- n_open - 1

    if (n_open == 0)
      return(c(
        substr(opt_str, 1, i - 2),
        substr(opt_str, i + 1, n - 1)
      ))
  }

  stop_f("opt '%s' is not balanced!", opt_str)
}

# Suppose we have a vector of strings of the form "A (B)",
# where A is any string and B is a number. Then
# return a vector containing for each string c("A", "B").
sep_opt <- function(opt_chr)
{
  stopifnot(is.character(opt_chr))
  vapply(opt_chr, sep_opt_str, character(2)) %>% as.character()
}

# syntactic sugar if x is a named integer vector
get_opt_named_int <- function(x)
{
  get_opt(names(x), x)
}

# converts a character vector to opts by frequency
get_opt_chr <- function(x)
{
  stopifnot(is.character(x))
  get_opt_named_int(table(x))
}

# From sep_opt, return all As if ind = 1 or all Bs if ind = 2.
parse_opt <- function(str, ind = 1)
{
  stopifnot(is.character(str))
  result <- sep_opt(str)
  result[(seq(result) + ind) %% 2 == 0]
}

# ---------------
# TEXT MANAGEMENT
# ---------------

# Formats a numeric or character vector for printing
format_print_simple <- function(vec)
{
  if (is.character(vec))
    vec <- sprintf("'%s'", vec)
  if (length(vec) == 1)
    return(vec)
  sprintf("[%s]", vec_str(vec))
}

# Creates a label for the nth component
pc <- function(name = "1")
{
  sprintf("Component %s", name)
}

#' creates a vector of inputs that should be excluded
#' from bookmarking, based on the table's ID
#' @param table_id [string] not checked
#' @return [character]
table_exclude_vector <- function(table_id)
{
  c(
    sprintf("%s_search", table_id),
    sprintf("%s_state", table_id),
    sprintf("%s_cell_clicked", table_id),
    sprintf("%s_search_columns", table_id),
    sprintf("%s_rows_current", table_id),
    sprintf("%s_rows_all", table_id),
    sprintf("%s_rows_selected", table_id),
    sprintf("%s_columns_selected", table_id),
    sprintf("%s_cells_selected", table_id)
  )
}

# -------------
# DATA HANDLING
# -------------

# checks if a value is invalid with respect to a range
# if given an ordered pair, returns whether either value is invalid
range_invalid <- function(value, min, max)
{
  if (length(value) == 2)
    return(range_invalid(value[1], min, max) || range_invalid(value[2], min, max))

  length(value) != 1 || !all_fin(value) || value < min || value > max
}

#' find the smallest positive integer not in v
#'
#' @param vec [vector] not checked
#' @returns [int]
smallest_missing <- function(vec)
{
  small <- 1L
  while (small %in% vec)
    small <- small + 1L
  small
}

median_value <- function(x)
{
  x[median_index(length(x))]
}

# given a vector of values, generate a table for the legend
generate_legend_table <- function(vec)
{
  unique_vals <- unique(vec)

  if (length(unique_vals) < 1)
    return(NULL)

  data.frame("Number" = seq_along(unique_vals), "Value" = unique_vals)
}
