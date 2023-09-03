# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

if (!exists("sdr_config"))
  source("app/install.R")

library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)

source_app("plotting.R")
source_app("app_utils.R")

# sets up all storage
source_app("storage.R")
load_all_stores()

source_app("make_requests.R")

get_requests <- function(file)
{
  load_store(file, make_requests())
}

user_req_file <- "Sessions/user_requests.rds"

app_requests <- get_requests("app_requests.rds")
# test the application with no available requests
# app_requests <- make_requests()
stopifnot(are_requests(app_requests))

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

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notification <- function(message, time, form)
{
  if (time > 0)
    showNotification(HTML(message), duration = time, closeButton = TRUE, type = form)
}

# ------------------
# BROWSER PARAMETERS
# ------------------

# The height of a graph by default. Depends on browser interpretation.
graph_height <- 520
# the default barplot/(barplot+matrix) ratio on an upset plot
def_bar_frac <- 0.7
# the default number of set columns on an upset plot
def_set_col_num <- 40
# the maximum number of set columns on an upset plot
max_set_col_num <- 1000000
# the initial number of rows on an upset plot
max_upse <- 5000
# the initial number of rows on a heatmap
max_heat <- 20000
# the initial number of columns on a dendrogram
max_dend <- 200
# should plots respond to user inputs by default?
run_default <- TRUE
# should server-side rendering be used for tables?
table_server_render <- TRUE

# -----------
# BOOKMARKING
# -----------

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
  "console",

  "req_cat",
  "req_row",
  "req_col",
  "req_sca",
  "req_nor",
  "req_emb",
  "req_vis",
  "req_cha",

  "cat_notes"
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
  "set_feat_dend",

  "req_com",
  "req_dim",
  "req_per",
  "req_bat",
  "req_thr"
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
  "req_draft",
  "req_submit",
  "notes",

  "req_aut",

  table_exclude_vector("num_data_table"),
  table_exclude_vector("metadata_table"),
  table_exclude_vector("legend_out")
)

console_ids <- c(
  "address",
  "num_data",
  "metadata",
  "app_requests",
  "user_requests",
  picker_input_ids,
  dynam_picker_input_ids,
  numeric_input_ids,
  numeric_range_input_ids,
  slider_input_ids,
  tabset_panel_ids
)

bookmarkable_ids <- c(
  picker_input_ids,
  sprintf("%s_open", picker_input_ids),
  numeric_input_ids,
  numeric_range_input_ids,
  slider_input_ids,
  tabset_panel_ids
)

# the vector of all inputs to exclude from manual bookmarking
bookmark_exclude_vector <- c(
  default_exclude_vector,
  dynam_picker_input_ids,
  sprintf("%s_open", dynam_picker_input_ids),
  bookmarkable_ids
)

# ----------------------------
# DYNAMIC CHOICES / SELECTIONS
# ----------------------------

# APPLICATION INTERNAL STATE:
# --per category: [[cat]]
#     rowby (one of row subsets)
#     colby (one of col subsets)
#     colorby (one of metadata characteristics)
#     shapeby (one of metadata characteristics)
#     labelby (one of metadata characteristics)
#     filterby (one of safe metadata characteristics)
#     selectby per relevant metadata characteristic [[cha]]
#         character vector of selected options
#     threby per scaling option
#         one of obtained thresholds

#' same as apply(metadata[, rel_meta], 2, get_opt_chr)
#' but it does not drop when rel_meta is of length 1
#'
#' @param metadata [metadata]
#' @param rel_meta [character]
#' @returns [list]
get_opt_metadata <- function(metadata, rel_meta)
{
  opt_metadata <- empty_named_list(rel_meta)

  for (cha in rel_meta)
    opt_metadata[[cha]] <- get_opt_chr(metadata[[cha]])

  opt_metadata
}

# specify choices unique to each row axis
row_axs_names <- get_row_axs_names()
app_row_choices <- empty_named_list(row_axs_names)
for (row_axs in row_axs_names)
{
  row_axis <- get_row_axis(row_axs)
  row_meta <- row_axis$metadata
  rel_meta <- row_axis$rel_meta

  app_row_choices[[row_axs]] <- list(
    "rowby" = get_axis_summary(row_axis) %>%
      unlist() %>% get_opt_named_int(),
    "full_chas" = names(row_meta),
    "safe_chas" = rel_meta,
    "selectby" = get_opt_metadata(row_meta, rel_meta)
  )
}

# specify choices unique to each col axis (colby)
col_axs_names <- get_col_axs_names()
app_col_choices <- empty_named_list(col_axs_names)
for (col_axs in col_axs_names)
{
  app_col_choices[[col_axs]] <- list(
    "colby" = get_col_axis_summary(col_axs) %>%
      unlist() %>% get_opt_named_int()
  )
}

#' wrapper for app_row_choices by cat
#'
#' @param cat [string] not checked
#' @returns [list]
get_app_row_choices <- function(cat)
{
  app_row_choices[[cat_to_row_axs(cat)]]
}

#' wrapper for app_col_choices by cat
#'
#' @param cat [string] not checked
#' @returns [list]
get_app_col_choices <- function(cat)
{
  app_col_choices[[cat_to_col_axs(cat)]]
}

# specify choices unique to each category (threby) and
# the selections for each category
cat_names <- get_cat_names()
app_cat_choices <- empty_named_list(cat_names)
app_cat_selected <- empty_named_list(dynam_picker_input_ids)
for (dynam_id in dynam_picker_input_ids)
  app_cat_selected[[dynam_id]] <- empty_named_list(cat_names)

# useful for thresholds
is_sets <- (app_requests$EMBEDDING == "Sets")
cat_vec <- app_requests$CATEGORIES[is_sets]
sca_vec <- app_requests$SCALING[is_sets]
thr_vec <- app_requests$THRESHOLD[is_sets]

for (cat in cat_names)
{
  row_choices <- get_app_row_choices(cat)
  col_choices <- get_app_col_choices(cat)
  thre_choices <- empty_named_list(sca_options)
  thre_selected <- empty_named_list(sca_options)

  for (sca in sca_options)
  {
    raw_thresholds <- thr_vec[(cat_vec == cat) & (sca_vec == sca)]
    thresholds <- sort(unique(raw_thresholds))
    thre_choices[[sca]] <- thresholds
    thre_selected[[sca]] <- median_value(thresholds)
  }

  app_cat_choices[[cat]] <- list("threby" = thre_choices)
  safe_chas <- row_choices$safe_chas

  # note that choices and selected are accessed differently:
  # choices is [[cat]][[inputId]]
  # selected is [[inputId]][[cat]]
  app_cat_selected$rowby[[cat]] <- row_choices$rowby[1]
  app_cat_selected$colby[[cat]] <- col_choices$colby[1]
  app_cat_selected$colorby[[cat]] <- safe_chas[1]
  app_cat_selected$shapeby[[cat]] <- safe_chas[min(2, length(safe_chas))]
  app_cat_selected$labelby[[cat]] <- safe_chas[1]
  app_cat_selected$filterby[[cat]] <- safe_chas[1]
  app_cat_selected$selectby[[cat]] <- row_choices$selectby
  app_cat_selected$threby[[cat]] <- thre_selected
}

# ---------------
# ASSEMBLE THE UI
# ---------------

output_conditions <- c(
  "visualize_cond",
  "perplexity_cond",
  "set_feat_upse_cond",
  "set_feat_heat_cond",
  "set_feat_dend_cond",
  "nintersect_cond",
  "pc_sliders_cond",
  "pc_slider2_cond",
  "pc_slider3_cond",
  "shape_opts_cond",
  "label_opts_cond"
)

# this is suspicious ... improve later to be like thresholds?
perplexity_types <- setdiff(unique(app_requests$PERPLEXITY), num_d)
pc_cap <- max(c(app_requests$COMPONENT, 3L)) # needs to be at least 3
batch_sizes <- setdiff(unique(app_requests$BATCH_SIZE), num_d)

parameters_menu <- menuItem(
  "Parameters",
  startExpanded = TRUE,
  # icon = icon("table"),
  icon = icon("calculator"),
  select_panel("category", "Category", app_data$groups),
  select_panel("rowby", "Sample Subset"), # DYNAMIC
  select_panel("colby", "Feature Subset"), # DYNAMIC
  select_panel("scaling", "Scaling", sca_options),
  select_panel("normalization", "Normalization"), # DYNAMIC
  select_panel("embedding", "Method of Dimensionality Reduction", emb_options),
  conditionalPanel(
    condition = "output.visualize_cond",
    select_panel("visualize", "Method of Visualization", vis_options)
  ),
  conditionalPanel(
    condition = "input.embedding != 'Sets'",
    conditionalPanel(
      condition = "output.perplexity_cond",
      select_panel("perplexity", "Perplexity", perplexity_types,
                   median_value(perplexity_types))
    ),
    conditionalPanel(
      condition = "input.embedding == 'VAE'",
      select_panel("batch_size", "Batch Size", batch_sizes)
    )
  ),
  conditionalPanel(
    condition = "input.embedding == 'Sets'",
    select_panel("threby", "Threshold")
  )
)

filters_menu <- menuItem(
  "Filters",
  icon = icon("filter"),
  conditionalPanel(
    condition = "input.embedding != 'Sets' && output.pc_sliders_cond",
    pc_slider(1, pc_cap),
    conditionalPanel(
      condition = "output.pc_slider2_cond",
      pc_slider(2, pc_cap)
    ),
    conditionalPanel(
      condition = "output.pc_slider3_cond",
      pc_slider(3, pc_cap)
    )
  ),
  conditionalPanel(
    condition = "input.embedding == 'Sets'",
    numericRangeInput("set_f1", "Fraction of Samples", c(0.5, 1)),
    numericRangeInput("set_f2", "Number of Characteristics", c(1, 60)),
    conditionalPanel(
      condition = "output.set_feat_upse_cond",
      numericInput("set_feat_upse", "Maximum Features",
                   value=max_upse, min=pc_cap, max=2^24)
    ),
    conditionalPanel(
      condition = "output.set_feat_heat_cond",
      numericInput("set_feat_heat", "Maximum Features",
                   value=max_heat, min=pc_cap, max=2^24)
    ),
    conditionalPanel(
      condition = "output.set_feat_dend_cond",
      numericInput("set_feat_dend", "Maximum Features",
                   value=max_dend, min=pc_cap, max=2^24)
    )
  ),
  conditionalPanel(
    condition = "input.embedding != 'Sets' &&
    (input.visualize != 'Summarize' || input.embedding == 'PHATE')",
    select_panel("colorby", "Color By"), # DYNAMIC
    conditionalPanel(
      condition = "output.shape_opts_cond",
      select_panel("shapeby", "Shape By") # DYNAMIC
    ),
    conditionalPanel(
      condition = "output.label_opts_cond",
      select_panel("labelby", "Label By") # DYNAMIC
    )
  ),
  conditionalPanel(
    condition = "output.nintersect_cond",
    numericInput("nintersect", "Number of Columns",
                 value = def_set_col_num, min = 3, max=max_set_col_num),
    numericInput("bar_frac", "Bar Plot Fraction",
                 value = def_bar_frac, min = 0, max = 1),
    numericInput("text_scale", "Text Scale", value = 1, min = 0.01, max = 100)
  ),
  conditionalPanel(
    condition = "input.embedding == 'Sets' ||
    (input.visualize != 'Summarize' || input.embedding == 'PHATE')",
    select_panel("filterby", "Filter By"), # DYNAMIC
    check_panel("selectby", "Selections") # DYNAMIC
  )
)

settings_menu <- menuItem(
  "Settings",
  icon = icon("wrench"),
  check_panel("sMenu", "Settings",
              c("Embed Title", "Embed Legend", "Separate Colors",
                "Boost Graphics", "Uninverted Colors")),
  select_panel("palette", "Color Palette", color_seq_types),
  numericInput("width", "Graph Width", value=NULL, min=1, max=4000),
  numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
  numericInput("notif_time", "Notification Time", value=6),
  check_panel("console", "Console Output", console_ids, NULL)
)

last_updated_time <- "Unknown"
if (nrow(app_requests) > 0)
  last_updated_time <- format(max(app_requests$TIME_COMPLETED), "%b %d, %Y")

last_updated_text <- paste("  <b>Last Updated:</b>", last_updated_time)

button_toolbox <-  box(
  action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
  action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
  action("req_draft", "Request", "user-edit", "#FFF", "#29AB87", "#00356B"),
  action("refresh", "Refresh", "sync", "#FFF", "#29AB87", "#00356B"),
  bookmarkButton(),
  downloadButton("download_num_data", "Numeric Data"),
  downloadButton("download_metadata", "Metadata"),
  action("notes", "Notes", "book", "#FFF", "#9400D3", "#00356B"),
  title = "Controls",
  collapsible = TRUE,
  collapsed = FALSE,
  width = "100%"
)

req_draft_modal <- modalDialog(
  title = HTML("<b>Request Custom Analysis</b>"), easyClose = TRUE,
  select_panel("req_emb", "Desired Embedding", emb_options),
  hr(style = "border-top: 1px solid #000000;"),
  select_panel("req_cat", "Desired Category", cat_names),
  conditionalPanel(
    condition = "input.req_emb != 'Sets'",
    select_panel("req_row", "Desired Sample Subset"), # DYNAMIC
    select_panel("req_col", "Desired Feature Subset") # DYNAMIC
  ),
  select_panel("req_sca", "Desired Scaling", sca_options),
  select_panel("req_nor", "Desired Normalization"), # DYNAMIC
  conditionalPanel(
    condition = "input.req_emb == 'PCA' || input.req_emb == 'VAE' || input.req_emb == 'UMAP'",
    select_panel("req_vis", "Desired Visualization", vis_options)
  ),
  numericInput("req_com", "Desired Component", 10, min = 2, step = 1),
  conditionalPanel(
    condition = "(input.req_emb == 'PCA' || input.req_emb == 'VAE' || input.req_emb == 'UMAP') &&
        input.req_vis == 'tSNE'",
    numericInput("req_dim", "Desired Dimension", 2, min = 2, max = 3, step = 1)
  ),
  conditionalPanel(
    condition = "input.req_emb == 'PHATE' || input.req_vis == 'tSNE' ||
          (input.req_emb == 'UMAP' && input.req_vis != 'Summarize')",
    numericInput("req_per", "Desired Perplexity", 10, min = 1, step = 1)
  ),
  conditionalPanel(
    condition = "input.req_emb == 'VAE'",
    numericInput("req_bat", "Desired Batch Size", 64, min = 1, step = 1)
  ),
  conditionalPanel(
    condition = "input.req_emb == 'Sets'",
    numericInput("req_thr", "Desired Threshold", 0.5, min = 0, max = 1, step = 0.1^4),
    select_panel("req_cha", "Desired Characteristic") # DYNAMIC
  ),
  textInput("req_aut", "Author Name"),
  footer = tagList(
    action("req_submit", "Submit", "cloud", "#FFF", "#0064C8", "#00356B"),
    modalButton("Dismiss")
  )
)

notes_modal <- modalDialog(
  paste(
    "Please visit our
<a href='https://github.com/gersteinlab/shiny-dim-reduction/'
target='_blank' rel='noopener noreferrer'>Github</a> for instructions and
a full list of project contributors / references.
The citations below are in the format requested by their respective creators.
<br><br>
<b>Data Sources</b>
<br>",
    app_data$citations,
    sep = ""
  ) %>% HTML(),
  select_panel("cat_notes", "Category Notes", choices = cat_names),
  textOutput("cat_notes_text"),
  title = HTML("<b>Notes</b>"),
  easyClose = TRUE
)

app_css_data <- tags$head(includeCSS(get_app_loc("app_styling.css")))

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title = app_data$title, titleWidth = "100%"),
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        parameters_menu,
        filters_menu,
        settings_menu,
        div(
          style = "margin: 10px",
          h4(HTML(last_updated_text))
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      app_css_data,
      button_toolbox,
      htmlOutput("title_out"),
      tabBox(
        width="100%",
        id = 'plotPanels',
        tabPanel("Static 2D", uiOutput("ggplot2UI")),
        tabPanel("Interactive 2D", uiOutput("plotly2UI")),
        tabPanel("Interactive 3D", uiOutput("plotly3UI")),
        tabPanel("Boxplot", uiOutput("beeswarmUI")),
        tabPanel("Approved Requests", uiOutput("requestsUI")),
        tabPanel("Pending Requests", uiOutput("pendingRequestsUI")),
        tabPanel("Numeric Data", uiOutput("num_dataUI")),
        tabPanel("Metadata", uiOutput("metadataUI"))
      ),
      uiOutput("legendUI"),
      verbatimTextOutput("console_out")
    )
  )
}

cat_f("DASHBOARD TIME: %.1f (sec)\n", net_time())
