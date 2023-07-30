# The purpose of this file is to store various UI-related functions for the app.

if (!exists("sdr_config"))
  source("app/install.R")

library(stringi)
library(shinycssloaders)
library(shinyWidgets)

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

# formats to "opt_name (opt_num)"
get_opt <- function(opt_name, opt_num)
{
  sprintf("%s (%s)", opt_name, opt_num)
}

# Suppose we have a vector of strings of the form "A (B)",
# where A is any string and B is a number. Then
# return a vector containing for each string c("A", "B").
sep_opt <- function(opt_str)
{
  stopifnot(is.character(opt_str))

  n <- length(opt_str)
  rev_str <- stri_reverse(opt_str) # reverse the string to use stri_split_fixed
  result <- unlist(stri_split_fixed(rev_str,"( ", n = 2)) # split into two parts

  for (i in seq_len(n))
  {
    b <- result[2*i - 1]
    a <- result[2*i]
    result[2*i - 1] <- stri_reverse(a)
    result[2*i] <- stri_reverse(substring(b, 2))
  }

  result
}

# Formats a numeric or character vector for printing
format_print_simple <- function(vec)
{
  if (is.character(vec))
    vec <- sprintf("\"%s\"", vec)
  if (length(vec) > 1)
    return(sprintf("[%s]", paste(vec, collapse = ", ")))
  vec
}

# Creates a label for the nth component
pc <- function(name = "1")
{
  sprintf("Component %s", name)
}

# syntactic sugar if x is a named vector
get_opt_named <- function(x)
{
  get_opt(names(x), x)
}

# converts a character vector to opts
get_opt_chr <- function(x)
{
  stopifnot(is.character(x))
  get_opt_named(table(x))
}

# From sep_opt, return all As if ind = 1 or all Bs if ind = 2.
parse_opt <- function(str, ind = 1)
{
  stopifnot(is.character(str))
  result <- sep_opt(str)
  result[(seq(result) + ind) %% 2 == 0]
}

# -------------------
# INTERFACE FUNCTIONS
# -------------------

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

# adds a spinner to content that may need to be refreshed
my_spin <- function(content)
{
  withSpinner(content, type = 6)
}

# makes a slider for the nth principal component
pc_slider <- function(n, pc_cap)
{
  sliderInput(sprintf("pc%s", n), sprintf("Displayed Component %s", n),
              min=1, max=pc_cap, value=n, step=1, ticks = FALSE)
}

# Creates a selectizeInput panel with only one option allowed.
select_panel <- function(inputId, label, choices = NULL, selected = NULL)
{
  result <- pickerInput(
    inputId = inputId, label = label, choices = choices,
    selected = selected, multiple = FALSE,
    options = list(
      `live-search` = TRUE,
      `live-search-placeholder` = "Search for a phrase ..."
    )
  )

  if (is.list(options))
  {
    for (cg in names(options))
    {
      opt <- options[[cg]]

      if (length(opt) < 2)
      {
        result$children[[2]]$children[[1]] <- HTML(reg_str(
          result$children[[2]]$children[[1]],
          sprintf("<option value=\"%s\">%s</option>", opt, cg),
          sprintf("<optgroup label=\"%s\">
<option value=\"%s\">%s</option>
</optgroup>", cg, opt, opt)
        ))
      }
    }
  }

  result
}

# Creates a group of checked boxes with the given id, name, and inputs
check_panel <- function(inputId, label, choices = NULL, selected = choices)
{
  pickerInput(
    inputId = inputId, label = label,
    choices = choices, selected = selected, multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `selected-text-format` = "count > 1",
      `live-search` = TRUE,
      `live-search-placeholder` = "Search for a phrase ...")
  )
}

# Creates an action button with the given id, name, icon name,
# color, background color, and border color.
action <- function(id, name, icon_name, color, bk, br)
{
  actionButton(
    inputId = id, label = name, icon = icon(icon_name), style =
      sprintf("color: %s; background-color: %s; border-color: %s", color, bk, br))
}

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notification <- function(message, time, form)
{
  if (time > 0)
    showNotification(HTML(message), duration = time, closeButton = TRUE, type = form)
}

# checks if a value is invalid with respect to a range
# if given an ordered pair, returns whether either value is invalid
range_invalid <- function(value, min, max)
{
  if (length(value) == 2)
    return(range_invalid(value[1], min, max) || range_invalid(value[2], min, max))

  length(value) != 1 || is.na(value) || is.nan(value) || value < min || value > max
}

# find the smallest positive integer not in the vector (used for bookmarking)
smallest_missing <- function(vec)
{
  small <- 1
  while (small %in% vec)
    small <- small+1
  small
}

# given a vector of values, generate a table for the legend
generate_legend_table <- function(vec)
{
  unique_vals <- unique(vec)

  if (length(unique_vals) < 1)
    return(NULL)

  data.frame("Number" = seq_along(unique_vals), "Value" = unique_vals)
}
