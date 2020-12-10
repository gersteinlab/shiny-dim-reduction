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

# the outlines used to decode lists of lists
outline <- my_empty_list(name_cat)
bookmark_char <- my_empty_list(name_cat)
bookmark_thre <- my_empty_list(name_cat)

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
  
  # filters
  filters <- order_names[between(cols_unique_gen, 2, num_filters)]
  if (length(filters) < 1)
    filters <- "Unknown"
  filter_opts[[cn]] <- cat_select_panel(
    cat, sprintf("filterby_%s", cat), sprintf("Current Filter (%s)", cat), filters, 1)
  
  # colors
  color_opts[[cn]] <- cat_select_panel(
    cat, sprintf("colorby_%s", cat), sprintf("Color By (%s)", cat), filters, 1)
  
  # shapes
  shape_opts[[cn]] <- cat_select_panel(
    cat, sprintf("shapeby_%s", cat), sprintf("Shape By (%s)", cat), filters, 2)
  
  # labels
  label_opts[[cn]] <- cat_select_panel(
    cat, sprintf("labelby_%s", cat), sprintf("Label By (%s)", cat), filters, 1)
  
  # selections
  outline[[cat]] <- my_empty_list(filters)
  for (char in filters)
  {
    select_ids <- c(select_ids, get_select(cat, char))
    opt <- get_opt(order_gen[[char]])
    outline[[cat]][[char]] <- opt
    select_opts[[length(select_ids)]] <- select_check_panel(opt, cat, char)
  }
  bookmark_char[[cat]] <- filters
  
  # thresholds
  for (sca in sca_options)
  {
    thre_ids <- c(thre_ids, get_thre(cat, sca))
    thre_opts[[length(thre_ids)]] <- thre_select_panel(thre_seqs[[sca]][[cat]], cat, sca)
  }
  bookmark_thre[[cat]] <- sca_options
}

# truncate select_opts
select_opts <- select_opts[1:length(select_ids)]

# the vector of all inputs to exclude from manual bookmarking
bookmark_exclude_vector <- c(
  table_exclude_vector(c(
    "num_data_table", "metadata_table", "legend_out"
  )),
  
  ".clientValue-default-plotlyCrosstalkOpts",
  "plotly_hover-A",
  "plotly_afterplot-A",
  "plotly_relayout-A",
  
  "sidebar_menu",
  "sidebarCollapsed",
  "sidebarItemExpanded",
  
  sprintf("subsetby_%s", name_cat),
  sprintf("colorby_%s", name_cat), 
  sprintf("shapeby_%s", name_cat), 
  sprintf("labelby_%s", name_cat), 
  sprintf("filterby_%s", name_cat), 
  select_ids, thre_ids,
  
  "start", "stop", "toggle", "central_nav", "instructions", "citations", 
  "sMenu", "height", "category", "scale", "normalize", "features", "embedding", 
  "visualize", "perplexity", "set_feat_upse", "set_feat_heat", "set_feat_dend",
  "palette", "plotPanels", "username", "password", "toggle_password",
  "attempt_login", "set_f1", "set_f2", "nintersect", "pc1", "pc2", "pc3"
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
    condition = "
input.embedding == 'PCA' || input.embedding == 'VAE' || input.embedding == 'UMAP'",
    select_panel("visualize", "Method of Visualization", vis_options)
  ),
  select_panel("scale", "Scale", sca_options),
  conditionalPanel(
    condition = "input.embedding != 'Sets'",
    select_panel("normalize", "Normalization", nor_options),
    select_panel("features", "Percentage of Features Used", fea_options)
  ),
  conditionalPanel(
    condition = "input.embedding != 'Sets' &&
(input.embedding == 'PHATE' || input.visualize == 'tSNE' ||
(input.embedding == 'UMAP' && input.visualize != 'Summarize'))",
    select_panel("perplexity", "Perplexity", perplexity_types, 
                 ceiling(length(perplexity_types)/2))
  ),
  expand_cond_panel(
    condition = "input.embedding == 'Sets'", 
    thre_opts, 
    list(
      numericRangeInput("set_f1", "Fraction of Samples", c(0.5,1)),
      numericRangeInput("set_f2", "Number of Characteristics", c(1,num_filters)),
      conditionalPanel(
        condition = "input.plotPanels == 'ggplot2'",
        numericInput("set_feat_upse", "Maximum Features", 
                     value=max_upse, min=pc_cap, max=2^24)
      ),
      conditionalPanel(
        condition = "input.plotPanels == 'plotly2'",
        numericInput("set_feat_heat", "Maximum Features", 
                     value=max_heat, min=pc_cap, max=2^24)
      ),
      conditionalPanel(
        condition = "input.plotPanels == 'plotly3'",
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
    condition = "input.plotPanels == 'ggplot2' && input.embedding == 'Sets'",
    numericInput("nintersect", "Number of Intersections", value=40, min=3, max=2^num_filters)
  ),
  conditionalPanel(
    condition = "input.visualize == 'Explore' && (input.embedding == 'PCA' ||
input.embedding == 'VAE' || input.embedding == 'UMAP')",
    pc_slider(1, pc_cap),
    conditionalPanel(
      condition = "input.plotPanels == 'ggplot2' ||
    input.plotPanels == 'plotly2' || input.plotPanels == 'plotly3'",
      pc_slider(2, pc_cap)
    ),
    conditionalPanel(
      condition = "input.plotPanels == 'plotly3'",
      pc_slider(3, pc_cap)
    )
  )
)

filtersMenu <- menuItem(
  "Filters",
  expand_cond_panel(
    condition = "input.embedding != 'Sets' && (input.embedding == 'PHATE' ||
      input.visualize != 'Summarize')",
    color_opts
  ),
  expand_cond_panel(
    condition = "input.embedding != 'Sets' && input.plotPanels == 'ggplot2' &&
  (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
    shape_opts
  ),
  expand_cond_panel(
    condition = "input.embedding != 'Sets' && input.plotPanels != 'boxplot' &&
        input.plotPanels != 'ggplot2' &&
  (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
    label_opts
  ),
  expand_cond_panel(
    condition = "input.visualize != 'Summarize' ||
      input.embedding == 'Sets' || input.embedding == 'PHATE'",
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
        id = "sidebar_menu",
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
        tabPanel("ggplot2", uiOutput("ggplot2UI")),
        tabPanel("plotly2", uiOutput("plotly2UI")),
        tabPanel("plotly3", uiOutput("plotly3UI")),
        tabPanel("boxplot", uiOutput("beeswarmUI")),
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