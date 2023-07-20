# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

if (!exists("sdr_config"))
  source("app/install.R")

library(shinydashboard)
library(shinyjs)

source_app("plotting.R")
source_app("ui_functions.R")
source_app("make_requests.R")

app_requests <- load_store(
  "app_requests.rds",
  make_requests()
)
# clean_req_keys(app_requests[, 1:13])

# create default user requests
default_user_requests <- load_store(
  "Sessions/user_requests.rds",
  make_requests()
)
# clean_req_keys(default_user_requests[, 1:13])

# ------------------------
# INSTRUCTIONS / CITATIONS
# ------------------------

citations <- make_citations(app_data$citations)
print_citations <- rem_html_tags(citations)

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
# is the user authenticated by default?
auth_default <- TRUE
# should plots respond to user inputs by default?
run_default <- TRUE
# should server-side rendering be used for tables?
table_server_render <- TRUE

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

# specify choices unique to each row axis
app_row_choices <- empty_named_list(row_axs_names)
for (row_axs in row_axs_names)
{
  row_axis <- row_axes[[row_axs]]
  row_meta <- row_axis$metadata
  rel_meta <- row_axis$rel_meta

  app_row_choices[[row_axs]] <- list(
    "rowby" = get_opt_named(row_subset_lengths[[row_axs]]),
    "full_chas" = names(row_meta),
    "safe_chas" = rel_meta,
    "selectby" = apply(row_meta[, rel_meta], 2, get_opt_chr)
  )
}

# specify choices unique to each col axis (colby)
app_col_choices <- empty_named_list(col_axs_names)
for (col_axs in col_axs_names)
{
  app_col_choices[[col_axs]] <- list(
    "colby" = get_opt_named(col_subset_lengths[[col_axs]])
  )
}

# useful
median_index <- function(x) {
  ceiling(length(x) / 2)
}

median_value <- function(x) {
  x[median_index(x)]
}

# specify choices unique to each category (threby) and
# the selections for each category
app_cat_choices <- empty_named_list(cat_names)
app_cat_selected <- empty_named_list(cat_names)

# useful for thresholds
is_sets <- (app_requests$EMBEDDING == "Sets")
cat_vec <- app_requests$CATEGORIES[is_sets]
sca_vec <- app_requests$SCALING[is_sets]
thr_vec <- app_requests$THRESHOLD[is_sets]

for (cat in cat_names)
{
  cat_row_choices <- app_row_choices[[get_row_axs(cat)]]
  cat_col_choices <- app_col_choices[[get_col_axs(cat)]]
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
  safe_chas <- cat_row_choices$safe_chas

  app_cat_selected[[cat]] <- list(
    "rowby" = cat_row_choices$rowby[1],
    "colby" = cat_col_choices$colby[1],
    "colorby" = safe_chas[1],
    "shapeby" = safe_chas[2],
    "labelby" = safe_chas[1],
    "filterby" = safe_chas[1],
    "selectby" = cat_row_choices$selectby,
    "threby" = thre_selected
  )
}

# -----------
# BOOKMARKING
# -----------

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
  # sprintf("%s_open", picker_input_ids),
  numeric_input_ids,
  numeric_range_input_ids,
  slider_input_ids,
  tabset_panel_ids
)

# the vector of all inputs to exclude from manual bookmarking
bookmark_exclude_vector <- c(
  default_exclude_vector,
  bookmarkable_ids
)

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
  "label_opts_cond")

# ---------------
# ASSEMBLE THE UI
# ---------------

# this is suspicious ... improve later to be like thresholds?
perplexity_types <- setdiff(unique(app_requests$PERPLEXITY), num_d)
pc_cap <- max(app_requests$COMPONENT)
batch_sizes <- setdiff(unique(app_requests$BATCH_SIZE), num_d)

settings_menu <- menuItem(
  "Settings",
  check_panel("sMenu", "Settings",
              c("Embed Title", "Embed Legend", "Separate Colors",
                "Boost Graphics", "Uninverted Colors")),
  select_panel("palette", "Color Palette", color_palettes),
  numericInput("width", "Graph Width", value=NULL, min=1, max=4000),
  numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
  numericInput("notif_time", "Notification Time", value=6),
  check_panel("console", "Console Output", console_ids, NULL)
)

table_1_menu <- menuItem(
  "Table Selection",
  startExpanded = TRUE,
  icon = icon("table"),
  select_panel("category", "Category", groups),
  select_panel("rowby", "Sample Subset"), # DYNAMIC
  select_panel("colby", "Feature Subset"), # DYNAMIC
  select_panel("scaling", "Scaling", sca_options),
  select_panel("normalization", "Normalization", nor_options)
)

analysis_1_menu <- menuItem(
  "Analysis Selection",
  icon = icon("calculator"),
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
      condition = "output.pc_sliders_cond",
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
      condition = "input.embedding == 'VAE'",
      select_panel("batch_size", "Batch Size", batch_sizes)
    )
  ),
  conditionalPanel(
    condition = "input.embedding == 'Sets'",
    select_panel("threby", "Threshold"),
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
  )
)

filters_1_menu <- menuItem(
  "Filter Set Selection",
  icon = icon("filter"),
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
                 value=def_set_col_num, min=3, max=max_set_col_num),
    numericInput("bar_frac", "Bar Plot Fraction", value=def_bar_frac, min=0, max=1),
    numericInput("text_scale", "Text Scale", value=1, min=0.01, max = 100)
  ),
  conditionalPanel(
    condition = "input.embedding == 'Sets' ||
    (input.visualize != 'Summarize' || input.embedding == 'PHATE')",
    select_panel("filterby", "Filter By"), # DYNAMIC
    check_panel("selectby", "Selections") # DYNAMIC
  )
)

last_updated_text <- sprintf(
  "  <b>Last Updated:</b> %s",
  format(max(app_requests$TIME_COMPLETED), "%b %d, %Y")
)

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title = app_data$title, titleWidth="100%"),
    dashboardSidebar(
      width=300,
      sidebarMenu(
        table_1_menu,
        analysis_1_menu,
        filters_1_menu,
        settings_menu,
        div(
          style = "margin: 10px",
          h4(HTML(last_updated_text))
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      button_toolbox(
        title = "Controls",
        action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
        action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
        action("request_analysis", "Request", "user-edit", "#FFF", "#29AB87", "#00356B"),
        bookmarkButton(),
        downloadButton("download_num_data", "Numeric Data"),
        downloadButton("download_metadata", "Metadata")
      ),
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
      button_toolbox(
        title = "Documentation",
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
        downloadButton('downloadInstructions', 'Instructions'),
        downloadButton('downloadCitations', 'Citations')
      ),
      verbatimTextOutput("console_out")
    )
  )
}

cat_f("DASHBOARD TIME: %.1f (sec)\n", net_time())