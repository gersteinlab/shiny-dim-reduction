# The purpose of this file is to store various UI-related functions for the app.

if (!exists("sdr_config"))
  source("app/install.R")

library(shinycssloaders)
library(shinyWidgets)

source_app("text_work.R")

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

# creates a singular select panel that only appears given a certain category
cat_select_panel <- function(cat, id, name, options, chosen)
{
  conditionalPanel(
    condition = sprintf("input.category == '%s'",  cat),
    select_panel(id, name, options, chosen)
  )
}

# creates a check panel of selections for a given category and characteristic
select_check_panel <- function(choices, cat, char)
{
  conditionalPanel(
    sprintf("input.category == '%s' && input.%s == '%s'", cat, id_filter(cat), char),
    check_panel(id_select(cat, char), sprintf("Filter By (%s)", cat), get_opts(choices))
  )
}

# creates a select panel of thresholds for a given category and scale
thre_select_panel <- function(choices, cat, sca)
{
  conditionalPanel(
    condition = sprintf("input.category == '%s' && input.scaling == '%s'", cat, sca),
    select_panel(id_thre(cat, sca), "Threshold", choices, ceiling(length(choices)/2))
  )
}

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notification <- function(message, time, form)
{
  if (time > 0)
    showNotification(HTML(message), duration = time, closeButton = TRUE, type = form)
}

# picks a random option for an input picker that accepts only 1 option
pick_random_input <- function(session, inputId, choices)
{
  updatePickerInput(session, inputId, selected = sample(choices, 1))
}

# checks if a value is invalid with respect to a range
# if given an ordered pair, returns whether either value is invalid
range_invalid <- function(value, min, max)
{
  if (length(value) == 2)
    return(range_invalid(value[1], min, max) || range_invalid(value[2], min, max))

  length(value) != 1 || is.na(value) || is.nan(value) || value < min || value > max
}

# checks if every member of the vector colors is in the vector custom
check_custom_colors <- function(colors, custom)
{
  for (color in colors)
    if (!(color %in% custom))
      return(FALSE)

  TRUE
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
