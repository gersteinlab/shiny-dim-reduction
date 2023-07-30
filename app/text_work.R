# The purpose of this file is to store long texts and functions for text manipulation.

if (!exists("sdr_config"))
  source("app/install.R")

# --------------
# FIND / REPLACE
# --------------

library(stringi)

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

# ---------
# FUNCTIONS
# ---------

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

# ----------
# LONG TEXTS
# ----------

dynam_picker_input_ids <- c(
  "rowby",
  "colby",
  "colorby",
  "shapeby",
  "labelby",
  "filterby",
  "selectby",
  "threby"
)

picker_input_ids <- c(
  "sMenu",
  "category",
  "scaling",
  "normalization",
  "embedding",
  "visualize",
  "perplexity",
  "batch_size",
  "palette",
  "console"
)

numeric_input_ids <- c(
  "height",
  "width",
  "text_scale",
  "notif_time",
  "nintersect",
  "bar_frac",
  "set_feat_upse",
  "set_feat_heat",
  "set_feat_dend"
)

numeric_range_input_ids <- c(
  "set_f1",
  "set_f2"
)

tabset_panel_ids <- c(
  "plotPanels"
)

slider_input_ids <- c(
  "pc1",
  "pc2",
  "pc3"
)

default_exclude_vector <- c(
  ".clientValue-default-plotlyCrosstalkOpts",
  "plotly_hover-A",
  "plotly_afterplot-A",
  "plotly_relayout-A",

  "sidebarMenu",
  "sidebarCollapsed",
  "sidebarItemExpanded",

  "start",
  "stop",
  "instructions",
  "citations",
  "randomize",
  "refresh",
  "request_analysis",

  table_exclude_vector("num_data_table"),
  table_exclude_vector("metadata_table"),
  table_exclude_vector("legend_out")
)

my_css_styling <- "
/* Personal notification preferences */
.shiny-notification {
  border-color: #00356B !important;
  opacity: 1 !important;
}

/* Increases text / icon visibility in selectors */
[role=option] > .text, [role=option] > .glyphicon {
  color: #000000 !important;
}

/* Better indicator of selected item */
.dropdown-menu>.active>a,
.dropdown-menu>.active>a:focus,
.dropdown-menu>.active>a:hover {
    background-color: #E0F0FF !important;
}

/* Yale Blue! */
.skin-blue .main-header .logo {
  background-color: #00356B !important;
}

/* Place sidebar toggle on right! */
.sidebar-toggle {
  float: right !important;
}

/* Prevents weird sidebar glitch */
.wrapper {
  height: auto !important;
  position:relative;
  overflow-x:hidden;
  overflow-y:hidden
}

/* Prevents overflow from input pickers */
.inner {
  min-height: 0px !important;
  max-height: 360px !important;
}

/* Prevents misfitting of dropdowns */
.dropdown-menu {
  min-height: 0px !important;
}

/* Wrap text to avoid overflowing selectors */
.dropdown-menu > li > a {
  white-space: normal !important;
}

/* Make password text invisible, but mark the caret */
.my-hidden-text {
  color: rgba(0,0,0,0) !important;
  caret-color: rgba(0,0,0,1) !important;
}

/* Make password text same color as selection */
.my-hidden-text::selection {
  color: #3297FD !important;
  background: #3297FD !important;
}

/* center selectors */
.sidebar-menu .treeview-menu {
  padding-left: 0px !important;
}

/* center title */
#title_out {
  text-align: center !important;
}
"
