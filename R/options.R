# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("ui_functions.R")
source_sdr("preprocess.R")

require("shinydashboard")
require("shinyjs")

# ------------------
# BROWSER PARAMETERS
# ------------------

# Only select characteristics with <= num_filters distinct values.
num_filters <- 60
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

# -----------------
# LOAD DEPENDENCIES
# -----------------

# Please see converter.R for an explanation of these dependencies.
get_dependency("order_total", empty_named_list(name_cat))
get_dependency("app_title", "Dimensionality Reduction Tool")
get_dependency("app_citations", "No data citations could be found.")
get_dependency("user_credentials")
get_dependency("custom_color_scales")

# -----
# SETUP
# -----

vis_to_noun <- function(vis)
{
  rep_str(vis, vis_options, vis_nouns)
}

# given a list of numeric vectors, returns get_opt(name, length) for each vector
name_num_map <- function(list_num)
{
  mapply(get_opt, names(list_num), lapply(list_num, length), USE.NAMES = FALSE)
}

# queries the user for a storage type
user_local <- "N"
if (sdr_running_local)
  user_local <- readline(prompt = "
Type 'Y' and press enter to use local storage.
Type anything else and press enter to use AWS storage. ")
set_storage(
  user_local == "Y",
  ifelse(sdr_from_app, "../reference", ref_loc),
  get_dependency("amazon_keys"))

# create bibliography
citations <- rep_str(bibliography, "!!!!!!!!!!", app_citations)

# creates a print version of the instructions / citations
print_instructions <- rem_html_tags(instructions)
print_citations <- rem_html_tags(citations)

# create categories and subsets
init_cat()
init_sub(name_num_map)

# open requests
app_requests <- load_store("requests.rds")

# ----------------
# FIX DEPENDENCIES
# ----------------

# gets the very first file associated with a category, counts the number of rows,
# and creates an empty metadata table for that category
get_empty_cat_meta <- function(cat){
  addr <- requests_to_final(app_requests[which(app_requests$CATEGORIES == cat)[1],])
  if (!find_store(addr))
    return(data.frame("Unknown" = "Unknown"))
  data.frame("Unknown" = rep("Unknown", nrow(load_store(addr))))
}

# fills in order_total in the event of missing portions
for (cat in name_cat)
  if (is.null(order_total[[cat]]))
    order_total[[cat]] <- get_empty_cat_meta(cat)

# ------------------------------
# GENERATE OPTIONS AND BOOKMARKS
# ------------------------------

# all bookmarking IDs for selections and thresholds
select_ids <- NULL
thre_ids <- NULL
selected_chars <- empty_named_list(name_cat)

# empty lists for option boxes, to be presented to the user
row_sub_opts <- vector(mode = "list", length = num_cat)
col_sub_opts <- vector(mode = "list", length = num_cat)
color_opts <- vector(mode = "list", length = num_cat)
shape_opts <- vector(mode = "list", length = num_cat)
label_opts <- vector(mode = "list", length = num_cat)
filter_opts <- vector(mode = "list", length = num_cat)
select_opts <- vector(mode = "list", length = num_filters*num_cat) # initially oversize
thre_opts <- vector(mode = "list", length = 2*num_cat)

# create the option boxes
for (cn in 1:num_cat)
{
  cat <- name_cat[cn]
  order_gen <- order_total[[cat]]
  cols_unique_gen <- apply(order_gen, 2, function(i) length(unique(i)))
  order_names <- colnames(order_gen)

  # row subsets
  row_subsets <- sub_row_groups[[cat]]
  row_sub_opts[[cn]] <- cat_select_panel(
    cat, id_row(cat), sprintf("Sample Subset (%s)", cat), row_subsets, 1)

  # col subsets
  col_subsets <- sub_col_groups[[cat]]
  col_sub_opts[[cn]] <- cat_select_panel(
    cat, id_col(cat), sprintf("Feature Subset (%s)", cat), col_subsets, 1)

  # characteristics
  chars <- order_names[between(cols_unique_gen, 2, num_filters)]
  if (length(chars) < 1)
    chars <- "Unknown"
  selected_chars[[cat]] <- chars

  # filters
  filter_opts[[cn]] <- cat_select_panel(
    cat, id_filter(cat), sprintf("Current Filter (%s)", cat), chars, 1)

  # colors
  color_opts[[cn]] <- cat_select_panel(
    cat, id_color(cat), sprintf("Color By (%s)", cat), chars, 1)

  # shapes
  shape_opts[[cn]] <- cat_select_panel(
    cat, id_shape(cat), sprintf("Shape By (%s)", cat), chars, 2)

  # labels
  label_opts[[cn]] <- cat_select_panel(
    cat, id_label(cat), sprintf("Label By (%s)", cat), chars, 1)

  # selections
  for (char in chars)
  {
    select_ids <- c(select_ids, id_select(cat, char))
    select_opts[[length(select_ids)]] <- select_check_panel(order_gen[[char]], cat, char)
  }

  # # thresholds
  # for (sca in sca_options)
  # {
  #   thre_ids <- c(thre_ids, id_thre(cat, sca))
  #   thre_opts[[length(thre_ids)]] <- thre_select_panel(thre_seqs[[sca]][[cat]], cat, sca)
  # }
}

# truncate select_opts
select_opts <- select_opts[1:length(select_ids)]

# -----------
# BOOKMARKING
# -----------

picker_input_ids <- c(
  default_picker_input_ids,
  id_row(name_cat),
  id_col(name_cat),
  id_color(name_cat),
  id_shape(name_cat),
  id_label(name_cat),
  id_filter(name_cat),
  select_ids,
  thre_ids
)

bookmarkable_ids <- c(
  picker_input_ids,
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

perplexity_types <- setdiff(unique(app_requests$PERPLEXITY), num_d())
pc_cap <- max(app_requests$COMPONENT)
batch_sizes <- setdiff(unique(app_requests$BATCH_SIZE), num_d())

# ---------------
# ASSEMBLE THE UI
# ---------------

settings_menu <- menuItem(
  "Settings",
  check_panel("sMenu", "Settings", c("Embed Title", "Embed Legend", "Separate Colors",
                                     "Boost Graphics", "Uninverted Colors")),
  select_panel("palette", "Color Palette", color_palettes),
  numericInput("width", "Graph Width", value=NULL, min=1, max=4000),
  numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
  numericInput("notif_time", "Notification Time", value=6),
  check_panel("console", "Console Output", bookmarkable_ids, NULL)
)

category_sel <- select_panel("category", "Category", cat_groups)
category_sel$children[[2]]$children[[1]] <- HTML(reg_str(
  category_sel$children[[2]]$children[[1]],
  "<option value=\"RNA_binding_proteins\">RNA Binding Proteins</option>",
  "<optgroup label=\"RNA Binding Proteins\">
<option value=\"RNA_binding_proteins\">RNA Binding Proteins</option>
</optgroup>"
))

table_1_menu <- menuItem(
  "Table Selection",
  startExpanded = TRUE,
  icon = icon("table"),
  category_sel,
  row_sub_opts,
  col_sub_opts,
  select_panel("scale", "Scale", sca_options),
  select_panel("normalize", "Normalization", nor_options)
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
                   ceiling(length(perplexity_types)/2))
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
  expand_cond_panel(
    condition = "input.embedding == 'Sets'",
    # thre_opts,
    list(
      numericRangeInput("set_f1", "Fraction of Samples", c(0.5,1)),
      numericRangeInput("set_f2", "Number of Characteristics", c(1,num_filters)),
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
)

filters_1_menu <- menuItem(
  "Filter Set Selection",
  icon = icon("filter"),
  conditionalPanel(
    condition = "input.embedding != 'Sets' &&
    (input.visualize != 'Summarize' || input.embedding == 'PHATE')",
    expand_cond_panel(
      condition = "true",
      color_opts
    ),
    expand_cond_panel(
      condition = "output.shape_opts_cond",
      shape_opts
    ),
    expand_cond_panel(
      condition = "output.label_opts_cond",
      label_opts
    )
  ),
  conditionalPanel(
    condition = "output.nintersect_cond",
    numericInput("nintersect", "Number of Columns",
                 value=def_set_col_num, min=3, max=max_set_col_num),
    numericInput("bar_frac", "Bar Plot Fraction", value=def_bar_frac, min=0, max=1),
    numericInput("text_scale", "Text Scale", value=1, min=0.01, max = 100)
  ),
  expand_cond_panel(
    condition = "input.embedding == 'Sets' ||
    (input.visualize != 'Summarize' || input.embedding == 'PHATE')",
    filter_opts,
    select_opts
  )
)

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title=app_title, titleWidth="100%"),
    dashboardSidebar(
      width=300,
      sidebarMenu(
        table_1_menu,
        analysis_1_menu,
        filters_1_menu,
        settings_menu
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      button_toolbox(
        title = "Controls",
        action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
        action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
        bookmarkButton(),
        downloadButton("download_num_data", "Numeric Data"),
        downloadButton("download_metadata", "Metadata"),
        action("randomize", "Randomize", "connectdevelop",
               "#FFF", "#29AB87", "#00356B")
      ),
      htmlOutput("title_out"),
      tabBox(
        width="100%",
        id = 'plotPanels',
        tabPanel("Requests", uiOutput("requestsUI")),
        tabPanel("Static 2D", uiOutput("ggplot2UI")),
        tabPanel("Interactive 2D", uiOutput("plotly2UI")),
        tabPanel("Interactive 3D", uiOutput("plotly3UI")),
        tabPanel("Boxplot", uiOutput("beeswarmUI")),
        tabPanel("Numeric Data", uiOutput("num_dataUI")),
        tabPanel("Metadata", uiOutput("metadataUI"))
      ),
      DTOutput("legend_out", width="100%") %>% my_spin(),
      button_toolbox(
        title = "Documentation",
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
        downloadButton('downloadInstructions', 'Instructions'),
        downloadButton('downloadCitations', 'Citations'),
        action("request_analysis", "Request Analysis", "user-edit", "#FFF", "#29AB87", "#00356B")
      ),
      verbatimTextOutput("console_out")
    )
  )
}
