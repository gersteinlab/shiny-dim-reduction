# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

source("interface.R", encoding="UTF-8")

require("shinydashboard")
require("shinyjs")

# ----------------
# FIX DEPENDENCIES
# ----------------

# order_total
for (cat in name_cat)
{
  if (is.null(order_total[[cat]]))
  {
    addr <- make_aws_name(
      cat, "Total", sca_options[1], nor_options[1], 
      rem_perc(fea_options[1]), emb_options[1], vis_options[1], 2, 1)
  
    data <- load_db(addr, aws_bucket)
    order_total[[cat]] <- data.frame("Unknown" = rep("Unknown", nrow(data)))
  }
}

# thresholds
thre_seqs <- rep(list(my_empty_list(name_cat)), 2)
names(thre_seqs) <- sca_options
for (sca in sca_options)
{
  for (cat in name_cat)
  {
    if (is.null(thresholds[[sca]][[cat]]))
      thre_temp <- 0:10/10
    else
      thre_temp <- seq(thre[1], thre[2], (thre[2]-thre[1])/10)
    
    thre_seqs[[sca]][[cat]] <- thre_temp %>% round(4) %>% format(nsmall=4)
  }
}

# ----------------
# GENERATE OUTLINE
# ----------------

sub_groups <- my_empty_list(name_cat)

for (cat in name_cat)
  sub_groups[[cat]] <- sprintf("Total (%s)", categories[[cat]])

for (dec_group in decorations)
{
  subset <- dec_group$Subsets[-1]
  mapping <- mapply(
    function(a,b){
      sprintf("%s (%s)", b, length(a))
    }, subset, names(subset)
  ) 
  names(mapping) <- NULL
    
  for (good_cat in dec_group$Categories)
    sub_groups[[good_cat]] <- c(sub_groups[[good_cat]], mapping) 
}

# an outline of characteristics
outline <- my_empty_list(name_cat)
# all the IDs that will be compressed for the URL, made from filters
select_ids <- NULL
# create the outline
cols_unique <- my_empty_list(name_cat)

# create the outline
for (cat in name_cat)
{
  order_gen <- order_total[[cat]]
  
  # number of factor levels per characteristic
  cols_unique[[cat]] <- apply(order_gen, 2, function(i) length(unique(i)))
  
  # characteristics with a factor number in the range 2 <= x <= num_filters
  filterable <- colnames(order_gen)[between(cols_unique[[cat]], 2, num_filters)]
  
  if (length(filterable) < 1)
    filterable <- "Unknown"
  
  outline[[cat]] <- my_empty_list(filterable)
  for (filchar in filterable)
  {
    opt <- get_opt(order_gen[[filchar]])
    outline[[cat]][[filchar]] <- opt
    select_ids <- c(select_ids, get_select(cat, filchar))
  }
  
}

# the option boxes that will be presented to the user
color_opts <- vector(mode = "list", length = num_cat)
shape_opts <- vector(mode = "list", length = num_cat)
label_opts <- vector(mode = "list", length = num_cat)
filter_opts <- vector(mode = "list", length = num_cat)
select_opts <- vector(mode = "list", length = length(select_ids))
thre_opts <- vector(mode = "list", length = 2*num_cat)

# create the option boxes
filt_ind <- 0
thre_ind <- 0
for (cn in 1:num_cat)
{
  order_gen <- order_total[[cn]]
  cat <- name_cat[cn]
  cols_unique_gen <- cols_unique[[cat]]
  order_names <- colnames(order_gen)
  
  # colors
  colors <- order_names[between(cols_unique_gen, 2, num_colors)]
  if (length(colors) < 1) 
    colors <- "Unknown"
  color_opts[[cn]] <- list("1"=cat, "2"=colors)
  
  # shapes
  shapes <- order_names[between(cols_unique_gen, 2, num_shapes)]
  if (length(shapes) < 1) 
    shapes <- "Unknown"
  shape_opts[[cn]] <- list("1"=cat, "2"=shapes)
  
  # labels
  labels <- order_names[between(cols_unique_gen, 2, num_labels)]
  if (length(labels) < 1) 
    labels<- "Unknown"
  label_opts[[cn]] <- list("1"=cat, "2"=labels)
  
  # filters
  filter_opts[[cn]] <- list(
    "1"=cat,
    "2"=names(outline[[cat]])
  )
  
  # selections
  for (char in names(outline[[cat]]))
  {
    filt_ind <- filt_ind + 1
    
    select_opts[[filt_ind]] <- conditionalPanel(
      condition=sprintf(
        "input.category == '%s' && input.filterby_%s == '%s'", cat, cat, char),
      check_panel(get_select(cat, char), sprintf("Filter By (%s)", cat),
                  outline[[cat]][[char]])
    )
  }
  
  # thresholds
  for (sca in sca_options)
  {
    thre_ind <- thre_ind + 1
    thre_opts[[thre_ind]] <- list("1"=cat, "2"=sca)
  }
}

# -----------------
# BOOKMARK OUTLINES
# -----------------

# all the IDs that will be compressed for the URL, made from thresholds
thre_ids <- NULL
for (sca in sca_options)
  thre_ids <- c(thre_ids, get_thre(name_cat, sca))

bookmark_cat <- my_empty_list(name_cat)
bookmark_thre <- my_empty_list(name_cat)

for (cat in name_cat)
{
  bookmark_cat[[cat]] <- names(outline[[cat]])
  bookmark_thre[[cat]] <- sca_options
}

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
  "attempt_login", "set_f1", "set_f2", "pc1", "pc2", "pc3"
)

# ---------------
# ASSEMBLE THE UI
# ---------------

# Creates the UI for subsets.
sub_panels_ui <- function(name_cat, sub_groups){
  sub_panels <- my_empty_list(name_cat)
  for (cat in name_cat)
  {
    subsets_temp <- sub_groups[[cat]]
    names(subsets_temp) <- NULL
    sub_panels[[cat]] <- conditionalPanel(
      condition = sprintf("input.category == '%s'",  cat),
      select_panel(
        sprintf("subsetby_%s", cat), sprintf("Feature Subset (%s)", cat),
        subsets_temp))
  }
  sub_panels
}

# Creates the UI for colors.
color_panels_ui <- function(colors){
  lapply(colors, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'", x[[1]]),
      select_panel(
        sprintf("colorby_%s", x[[1]]), sprintf("Color By (%s)", x[[1]]),
        x[[2]], 1))
  })
}

# Creates the UI for shapes.
shape_panels_ui <- function(shapes){
  lapply(shapes, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'",  x[[1]]),
      select_panel(
        sprintf("shapeby_%s", x[[1]]), sprintf("Shape By (%s)", x[[1]]),
        x[[2]], 2))
  })
}

# Creates the UI for labels.
label_panels_ui <- function(labels){
  lapply(labels, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'",  x[[1]]),
      select_panel(
        sprintf("labelby_%s", x[[1]]), sprintf("Label By (%s)", x[[1]]),
        x[[2]], 1))
  })
}

# Creates the UI for filter panels.
filter_panels_ui <- function(filters){
  lapply(filters, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'",  x[[1]]),
      select_panel(
        sprintf("filterby_%s", x[[1]]), sprintf("Current Filter (%s)", x[[1]]),
        x[[2]]))
  })
}

# Creates the UI for threshold panels.
thre_panels_ui <- function(thres){
  lapply(thres, function(x){
    thre_seq <- thre_seqs[[x[[2]]]][[x[[1]]]]
    mid <- ceiling(length(thre_seq)/2)
    
    conditionalPanel(
      condition = sprintf("input.category == '%s' && input.scale == '%s'", x[[1]], x[[2]]),
      select_panel(get_thre(x[[1]], x[[2]]), "Threshold", thre_seq, mid)
    )
  })
}

dataSelectionMenu <- menuItem(
  startExpanded=TRUE,
  "Data Selection",
  select_panel("category", "Category", cat_groups),
  sub_panels_ui(name_cat, sub_groups),
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
  do.call(conditionalPanel, c(
    condition = "input.embedding == 'Sets'", 
    thre_panels_ui(thre_opts), list(
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
  ))
)

settingsMenu <- menuItem(
  "Settings",
  check_panel("sMenu", "Settings", my_settings),
  select_panel("palette", "Color Palette", pal_options),
  numericInput("height", "Graph Height", value=graph_height, min=1, max=4000),
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
  do.call(conditionalPanel, c(
    condition = "input.embedding != 'Sets' && (input.embedding == 'PHATE' ||
      input.visualize != 'Summarize')",
    color_panels_ui(color_opts)
  )),
  do.call(conditionalPanel, c(
    condition = "input.embedding != 'Sets' && input.plotPanels == 'ggplot2' &&
  (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
    shape_panels_ui(shape_opts)
  )),
  do.call(conditionalPanel, c(
    condition = "input.embedding != 'Sets' && input.plotPanels != 'beeswarm' &&
        input.plotPanels != 'ggplot2' &&
  (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
    label_panels_ui(label_opts)
  )),
  do.call(conditionalPanel, c(
    condition = "input.visualize != 'Summarize' ||
      input.embedding == 'Sets' || input.embedding == 'PHATE'",
    filter_panels_ui(filter_opts),
    select_opts
  ))
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