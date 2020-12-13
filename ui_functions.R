# The purpose of this file is to store various UI-related functions for the app.

source("app_functions.R", encoding="UTF-8")

require("shinycssloaders")
require("shinyWidgets")

# ---------------
# INPUT SELECTION
# ---------------

# formats an element (opt) and the number of times it appears in a vector (samples)
get_opt <- function(opt, samples) 
{
  sprintf("%s (%s)", opt, sum(samples %in% opt))
}

# performs get_opt on every unique member of a vector (samples)
get_opts <- function(samples) 
{
  unlist(lapply(unique(samples), function(sample){get_opt(sample, samples)}))
}

# Suppose we have a vector of strings of the form "A (B)", 
# where A and B are strings that do not contain '(' or ')'. 
# Return all As if ind = 1 or all Bs if ind = 2.
parse_opt <- function(str, ind=1)
{
  if (length(str) < 1 || !is.character(str))
    return(NULL)
  strsplit(str, "( \\(|\\))") %>% lapply(function(i){i[ind]}) %>% unlist()
}

# Useful for finding select IDs
get_select <- function(category, character) 
{
  sprintf("selectby_%s_%s", category, character)
}

# Useful for finding thre IDs
get_thre <- function(category, scale) 
{
  sprintf("thre_%s_%s", category, scale)
}

# creates a vector of inputs that should be excluded 
# from bookmarking, based on the table's ID
table_exclude_vector <- function(...)
{
  table_id <- list(...)
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

# -------------------
# INTERFACE FUNCTIONS
# -------------------

# adds a spinner to content that may need to be refreshed
my_spin <- function(content)
{
  content %>% withSpinner(type = 6)
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
check_panel <- function(id, name, inputs)
{
  pickerInput(
    inputId = id, label = name,
    choices = inputs, selected = inputs, multiple = TRUE,
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
    sprintf("input.category == '%s' && input.filterby_%s == '%s'", cat, cat, char),
    check_panel(get_select(cat, char), sprintf("Filter By (%s)", cat), get_opts(choices))
  )
}

# creates a select panel of thresholds for a given category and scale
thre_select_panel <- function(choices, cat, sca)
{
  conditionalPanel(
    condition = sprintf("input.category == '%s' && input.scale == '%s'", cat, sca),
    select_panel(get_thre(cat, sca), "Threshold", choices, ceiling(length(choices)/2))
  )
}

# -------------
# NOTIFICATIONS
# -------------

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notif <- function(message, time, form) 
{
  showNotification(HTML(message), duration = time, closeButton = TRUE, type=form)
}

# prints a message once a plot begins generating.
plot_start <- function(numPlots)
{
  notif(sprintf("Generating Plot #%s:<br>
Please suspend plotting or wait for plotting to
finish before attempting a new configuration.", numPlots), 4, "default")
}

# prints a success message once a plot has been completed.
# note: start is the time when plotting begins, which can be found with Sys.time().
plot_success <- function(delta_time) 
{
  notif(sprintf("Plot generation was successful.<br>
Seconds elapsed: %s", delta_time), 6, "message")
}

# prints a failure message once a plot has been completed.
plot_fail <- function() 
{
  notif("Plot generation failed.<br>
Possible reasons:<br>
(1) invalid configuration<br>
(2) empty dataset", 6, "error")
}