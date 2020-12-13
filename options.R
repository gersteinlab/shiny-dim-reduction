# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

source("interface.R", encoding="UTF-8")

require("shinydashboard")
require("shinyjs")

# create categories and subsets
init_cat()
init_sub(name_num_map)

# ----------------
# FIX DEPENDENCIES
# ----------------

# gets the very first file associated with a category, counts the number of rows,
# and creates an empty metadata table for that category
getEmptyCatMeta <- function(cat){
  addr <- make_aws_name(
    cat, "Total", sca_options[1], nor_options[1], 
    rem_perc(fea_options[1]), emb_options[1], vis_options[1], 2, 1)
  data.frame("Unknown" = rep("Unknown", nrow(load_db(addr))))
}

# fills in order_total in the event of missing portions
for (cat in name_cat)
  if (is.null(order_total[[cat]]))
    order_total[[cat]] <- getEmptyCatMeta(cat)

# assigns thresholds (if NULL, return a useful placeholder)
assign_thre <- function(thre)
{
  if (is.null(thre))
    return((0:10/10) %>% format(nsmall=4))
  return(thre %>% format(nsmall=4))
}

# thresholds
thre_seqs <- rep(list(my_empty_list(name_cat)), 2)
names(thre_seqs) <- sca_options
for (sca in sca_options)
  for (cat in name_cat)
    thre_seqs[[sca]][[cat]] <- assign_thre(thresholds[[sca]][[cat]])

# ------------------------------
# GENERATE OPTIONS AND BOOKMARKS
# ------------------------------

# all bookmarking IDs for selections and thresholds
select_ids <- NULL
thre_ids <- NULL

# empty lists for option boxes, to be presented to the user
sub_opts <- vector(mode = "list", length = num_cat)
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
  
  # subsets
  subsets <- sub_groups[[cat]]
  sub_opts[[cn]] <- cat_select_panel(
    cat, sprintf("subsetby_%s", cat), sprintf("Feature Subset (%s)", cat), subsets, 1)
  
  # characteristics
  chars <- order_names[between(cols_unique_gen, 2, num_filters)]
  if (length(chars) < 1)
    chars <- "Unknown"
  
  # filters
  filter_opts[[cn]] <- cat_select_panel(
    cat, sprintf("filterby_%s", cat), sprintf("Current Filter (%s)", cat), chars, 1)
  
  # colors
  color_opts[[cn]] <- cat_select_panel(
    cat, sprintf("colorby_%s", cat), sprintf("Color By (%s)", cat), chars, 1)
  
  # shapes
  shape_opts[[cn]] <- cat_select_panel(
    cat, sprintf("shapeby_%s", cat), sprintf("Shape By (%s)", cat), chars, 2)
  
  # labels
  label_opts[[cn]] <- cat_select_panel(
    cat, sprintf("labelby_%s", cat), sprintf("Label By (%s)", cat), chars, 1)
  
  # selections
  for (char in chars)
  {
    select_ids <- c(select_ids, get_select(cat, char))
    select_opts[[length(select_ids)]] <- select_check_panel(order_gen[[char]], cat, char)
  }
  
  # thresholds
  for (sca in sca_options)
  {
    thre_ids <- c(thre_ids, get_thre(cat, sca))
    thre_opts[[length(thre_ids)]] <- thre_select_panel(thre_seqs[[sca]][[cat]], cat, sca)
  }
}

# truncate select_opts
select_opts <- select_opts[1:length(select_ids)]

# -----------
# BOOKMARKING
# -----------

picker_input_ids <- c(
  "sMenu",
  "category",
  "scale",
  "normalize",
  "features",
  "embedding",
  "visualize",
  "perplexity",
  "palette",
  sprintf("subsetby_%s", name_cat),
  sprintf("colorby_%s", name_cat),
  sprintf("shapeby_%s", name_cat),
  sprintf("labelby_%s", name_cat),
  sprintf("filterby_%s", name_cat),
  select_ids, 
  thre_ids
)

numeric_input_ids <- c(
  "height", 
  "nintersect", 
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

# the vector of all inputs to exclude from manual bookmarking
bookmark_exclude_vector <- c(
  ".clientValue-default-plotlyCrosstalkOpts",
  "plotly_hover-A",
  "plotly_afterplot-A",
  "plotly_relayout-A",
  
  "username", 
  "password", 
  "toggle_password", 
  "attempt_login",
  
  "sidebarMenu",
  "sidebarCollapsed",
  "sidebarItemExpanded",
  
  "start", 
  "stop", 
  "instructions", 
  "citations", 
  
  table_exclude_vector(
    "num_data_table", 
    "metadata_table", 
    "legend_out"
  ),
  
  picker_input_ids,
  numeric_input_ids,
  numeric_range_input_ids,
  slider_input_ids,
  tabset_panel_ids
)

# ---------------
# ASSEMBLE THE UI
# ---------------

dataSelectionMenu <- menuItem(
  startExpanded=TRUE,
  "Data Selection",
  select_panel("category", "Category", cat_groups),
  sub_opts,
  select_panel("embedding", "Method of Dimensionality Reduction", emb_options),
  conditionalPanel(
    repJs("[EMB] == 'PCA' || [EMB] == 'VAE' || [EMB] == 'UMAP'"),
    select_panel("visualize", "Method of Visualization", vis_options)
  ),
  select_panel("scale", "Scale", sca_options),
  conditionalPanel(
    condition = "input.embedding != 'Sets'",
    select_panel("normalize", "Normalization", nor_options),
    select_panel("features", "Percentage of Features Used", fea_options),
    conditionalPanel(
      repJs("[EMB] == 'PHATE' || [VIS] == 'tSNE' || ([EMB] == 'UMAP' && [VIS] != '[SUM]')"),
      select_panel("perplexity", "Perplexity", perplexity_types, 
                   ceiling(length(perplexity_types)/2))
    )
  ),
  expand_cond_panel(
    condition = "input.embedding == 'Sets'", 
    thre_opts, 
    list(
      numericRangeInput("set_f1", "Fraction of Samples", c(0.5,1)),
      numericRangeInput("set_f2", "Number of Characteristics", c(1,num_filters)),
      conditionalPanel(
        repJs("[PAN] == '[PAN1]'"),
        numericInput("set_feat_upse", "Maximum Features", 
                     value=max_upse, min=pc_cap, max=2^24)
      ),
      conditionalPanel(
        repJs("[PAN] == '[PAN2]'"),
        numericInput("set_feat_heat", "Maximum Features", 
                     value=max_heat, min=pc_cap, max=2^24)
      ),
      conditionalPanel(
        repJs("[PAN] == '[PAN3]'"),
        numericInput("set_feat_dend", "Maximum Features", 
                     value=max_dend, min=pc_cap, max=2^24)
      )
    )
  )
)

settingsMenu <- menuItem(
  "Settings",
  check_panel("sMenu", "Settings", my_settings),
  select_panel("palette", "Color Palette", pal_options),
  numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
  conditionalPanel(
    repJs("[PAN] == '[PAN1]' && [EMB] == 'Sets'"),
    numericInput("nintersect", "Number of Columns", value=40, min=3, max=2^num_filters)
  ),
  conditionalPanel(
    repJs("[VIS] == 'Explore' && ([EMB] == 'PCA' || [EMB] == 'VAE' || [EMB] == 'UMAP')"),
    pc_slider(1, pc_cap),
    conditionalPanel(
      repJs("[PAN] == '[PAN1]' || [PAN] == '[PAN2]' || [PAN] == '[PAN3]'"), 
      pc_slider(2, pc_cap)
    ),
    conditionalPanel(
      condition = repJs("[PAN] == '[PAN3]'"),
      pc_slider(3, pc_cap)
    )
  )
)

filtersMenu <- menuItem(
  "Filters",
  conditionalPanel(
    condition = "input.embedding != 'Sets'",
    expand_cond_panel(
      condition = "input.visualize != 'Summarize' || input.embedding == 'PHATE'",
      color_opts
    ),
    expand_cond_panel(
      repJs("[PAN] == '[PAN1]' && ([EMB] == 'PHATE' || [VIS] != '[SUM')"),
      shape_opts
    ),
    expand_cond_panel(
      repJs("[PAN] == '[PAN2]' && [PAN] == '[PAN3]' && 
      ([EMB] == 'PHATE' || [VIS] != 'SUM')"),
      label_opts
    )
  ),
  expand_cond_panel(
    repJs("[VIS] != '[SUM]' || [EMB] == 'Sets' || [EMB] == 'PHATE'"),
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
        dataSelectionMenu,
        settingsMenu,
        filtersMenu
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      box(
        title = "Controls",
        collapsible=TRUE, collapsed=FALSE, width="100%",
        action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
        action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
        bookmarkButton(),
        downloadButton('downloadData', 'Numeric Data'),
        downloadButton('downloadMetadata', 'Metadata')
      ),
      uiOutput("plainTitleUI"),
      tabBox(
        width="100%",
        id = 'plotPanels',
        tabPanel(pan_options[1], uiOutput("ggplot2UI")),
        tabPanel(pan_options[2], uiOutput("plotly2UI")),
        tabPanel(pan_options[3], uiOutput("plotly3UI")),
        tabPanel(pan_options[4], uiOutput("beeswarmUI")),
        tabPanel("Numeric Data", id="num_data",
                 DTOutput("num_data_table", width="100%") %>% my_spin()),
        tabPanel("Metadata", id="metadata",
                 DTOutput("metadata_table", width="100%") %>% my_spin())
      ),
      box(
        title = "Documentation",
        collapsible=TRUE, collapsed=FALSE, width="100%",
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
        downloadButton('downloadInstructions', 'Instructions'),
        downloadButton('downloadCitations', 'Citations')
      ),
      div(id="legend_out_spin", DTOutput("legend_out", width="100%") %>% my_spin())
    )
  )
}