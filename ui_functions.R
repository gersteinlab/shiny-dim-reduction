# The purpose of this file is to store various UI-related functions for the app.

require("shinycssloaders")
require("shinyWidgets")

# -----------
# COMPUTATION
# -----------

# given a matrix and two ranges f1, f2,
# return the rows where the of number of entries between f1[1], f1[2]
# is between f2[1], f2[2] and omit empty columns
set_f1_f2 <- function(data, f1, f2)
{
  if (class(data) != "matrix" || length(data) < 1 || ncol(data) < 1)
    return(matrix(nrow=0, ncol=0))
  for (j in 1:ncol(data))
    data[,j] <- ifelse(between(data[,j], f1[1], f1[2]), data[,j], NaN)
  valid <- !is.nan(data)
  data[between(rowSums(valid), f2[1], f2[2]), colSums(valid) > 0, drop = FALSE]
}

# given a matrix, returns a data frame where
# all non-NaN entries become 1 and all NaN entries become 0
num_nan_binary <- function(data)
{
  data[!is.nan(data)] <- 1
  data[is.nan(data)] <- 0
  data.frame(data)
}

# returns the first m rows of data unless m is too big
truncate_rows <- function(data, m)
{
  if (m < nrow(data))
    return(data[1:m,,drop=FALSE])
  data
}

# sort the rows of data by their sums in decreasing order
sort_row_sums <- function(data)
{
  data[base::order(rowSums(data),decreasing=T),,drop=FALSE]
}

# checks if a value is invalid with respect to a range
# if given an ordered pair, returns whether either value is invalid
range_invalid <- function(value, min, max)
{
  if (length(value) == 2)
    return(range_invalid(value[1], min, max) || range_invalid(value[2], min, max))

  length(value) != 1 || is.na(value) || is.nan(value) || value < min || value > max
}

# given a list of numeric vectors, returns get_opt(name, length) for each vector
name_num_map <- function(list_num)
{
  mapply(get_opt, names(list_num), lapply(list_num, length), USE.NAMES = FALSE)
}

# checks if every member of the vector colors is in the vector custom
check_custom_colors <- function(colors, custom)
{
  present <- TRUE

  for (color in colors)
    if (!(color %in% custom))
      present <- FALSE

  present
}

# find the smallest positive integer not in the vector
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

  table <- cbind.data.frame(1:length(unique_vals), unique_vals)
  colnames(table) <- c("Number", "Value")

  table
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

# assumes that the non-conditional arguments are lists of arguments, which
# can be concatenated to replicate the behavior of conditionalPanel
expand_cond_panel <- function(condition, ...)
{
  do.call(conditionalPanel, c(condition = condition, ...))
}

# Creates a selectizeInput panel with only one option allowed.
select_panel <- function(id, name, options, chosen)
{
  if (missing(chosen))
    chosen <- 1
  chosen <- min(chosen, length(options))

  pickerInput(
    inputId = id, label = name, choices = options,
    selected = options[chosen], multiple = FALSE,
    options = list(
      `live-search` = TRUE,
      `live-search-placeholder` = "Search for a phrase ..."
    )
  )
}

# Creates a group of checked boxes with the given id, name, and inputs
check_panel <- function(id, name, inputs, indices)
{
  if (missing(indices))
    indices <- 1:length(inputs)

  pickerInput(
    inputId = id, label = name,
    choices = inputs, selected = inputs[indices], multiple = TRUE,
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
    inputId = id, label = name, icon = icon(icon_name), style=
      sprintf("color: %s; background-color: %s; border-color: %s", color, bk, br))
}

# Return the UI for a modal dialog that attempts to authenticate the user
authenticator_modal <- function() {
  modalDialog(
    title = HTML("<b>Authentication</b>"),
    HTML("Need access? Please make a request to
    <a href=\"justin.chang@yale.edu\" target=\"_blank\">
    justin.chang@yale.edu</a>.<br><br>"),
    wellPanel(
      style="background-color: #E0F0FF; border-color: #00356B",
      textInput("username", "Username",
                placeholder="Please enter your username ...", value="guest"),
      textInput("password", "Password (is invisible)",
                placeholder="", value=""),
      action("attempt_login", "Login", "unlock", "#FFFFFF", "#0064C8", "#00356B"),
      actionButton("toggle_password", "Show/Hide Password")
    ),
    footer = tagList()
  )
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
    condition = sprintf("input.category == '%s' && input.scale == '%s'", cat, sca),
    select_panel(id_thre(cat, sca), "Threshold", choices, ceiling(length(choices)/2))
  )
}

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notification <- function(message, time, form)
{
  if (time > 0)
    showNotification(HTML(message), duration = time, closeButton = TRUE, type=form)
}

# picks a random option for an input picker that accepts only 1 option
pick_random_input <- function(session, inputId, choices)
{
  updatePickerInput(session, inputId, selected=sample(choices, 1))
}
