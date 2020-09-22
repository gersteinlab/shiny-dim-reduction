# The purpose of this file is to generate options for each category
# and to use these options in assembling the app's user interface.

# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

# ----------------
# GENERATE OUTLINE
# ----------------

# create categories
cat_groups <- lapply(categories_full, function(x){names(x)})
name_cat <- unlist(cat_groups)
num_cat <- length(name_cat)
categories <- unlist(categories_full, recursive=FALSE)
names(categories) <- name_cat
names(name_cat) <- NULL

# create category subsets panel
sub_groups <- my_empty_list(name_cat)
for (cat in name_cat)
  sub_groups[[cat]] <- sprintf("Total (%s)", categories[[cat]])

for (dec_group in decorations)
{
  if (length(dec_group$Categories) > 0)
    subset <- dec_group$Subsets[-1]
  
  for (good_cat in dec_group$Categories)
  {
    mapping <- mapply(
      function(a,b){
        sprintf("%s (%s)", b, length(a))
      }, subset, names(subset)
    ) %>% sort_by_names()
    names(mapping) <- NULL
    sub_groups[[good_cat]] <- c(sub_groups[[good_cat]], mapping) 
  }
  
  rm(subset)
}

# an outline of characteristics
outline <- my_empty_list(name_cat)
# all the IDs that will be compressed for the URL, made from the outline
select_ids <- NULL
# create the outline
cols_unique <- my_empty_list(name_cat)
# count max number of characteristics
max_cat_num <- 0

# create the outline
for (cat in name_cat)
{
  order_gen <- order_total[[cat]]
  
  # number of factor levels per characteristic
  cols_unique[[cat]] <- apply(order_gen, 2, function(i) length(unique(i)))
  
  # characteristics with a factor number in the range 2 <= x <= num_filters
  filterable <- colnames(order_gen)[between(cols_unique[[cat]], 2, num_filters)]
  
  outline[[cat]] <- my_empty_list(filterable)
  for (filchar in filterable)
  {
    opt <- get_opt(order_gen[[filchar]])
    outline[[cat]][[filchar]] <- opt
    select_ids <- c(select_ids, get_select(cat, filchar))
    max_cat_num <- max(length(opt), max_cat_num)
  }
    
}
rm(filchar, filterable, order_gen)

# -----------------
# BOOKMARK OUTLINES
# -----------------

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
  "height",
  "sidebarItemExpanded",
  
  sprintf("subsetby_%s", name_cat),
  sprintf("colorby_%s", name_cat), 
  sprintf("shapeby_%s", name_cat), 
  sprintf("labelby_%s", name_cat), 
  sprintf("filterby_%s", name_cat), 
  select_ids, 
  get_thre(name_cat, "Logarithmic"), get_thre(name_cat, "Linear"), 
  
  "start", "stop", "toggle", "central_nav", "instructions", "citations", 
  "sMenu", "category", "scale", "normalize", "features", "embedding", 
  "visualize", "perplexity", "upsetpref", "dendrogram", 
  "palette", "plotPanels", "username", "password", "toggle_password",
  "attempt_login", "set_f1", "set_f2", "pc1", "pc2", "pc3"
)

# ---------------
# ASSEMBLE THE UI
# ---------------

# only do assembly if you don't have a saved version

# perplexity_ui <- function(p_types){
#   if (length(p_types) < 1)
#     return(NULL)
#   
#   conditionalPanel(
#     condition = "input.embedding != 'Sets' && 
#   (input.embedding == 'PHATE' || input.visualize == 'tSNE' ||
#   (input.embedding == 'UMAP' && input.visualize != 'Summarize'))",
#     select_panel("perplexity", "Perplexity", p_types, ceiling(length(p_types)/2))
#   )
# }
# 
# sets_ui <- function(thre_opts, max){
#   min_max <- num_filters
#   for (i in 0:4)
#   {
#     for (j in 0:4)
#     {
#       target <- 2^i * 5*j
#       if (target >= max && target < min_max)
#         min_max <- target
#     }
#   }
#   
#   do.call(conditionalPanel, c(
#     condition = "input.embedding == 'Sets'", thre_opts, list(
#       sliderInput(
#         "set_f1", "Fraction of Samples", 
#         min=0, max=1, value=c(0.5,1), step=0.01,
#         ticks = FALSE, dragRange=FALSE),
#       sliderInput(
#         "set_f2", "Fraction of Characteristics", 
#         min=0, max=1, value=c(0,1), step=1.0/min_max,
#         ticks = FALSE, dragRange=FALSE)
#     )
#   ))
# }
# 
# sub_panels_ui <- function(cat_groups, sub_groups){
#   name_cat_temp <- unlist(cat_groups)
#   sub_panels <- my_empty_list(name_cat_temp)
#   for (cat in name_cat_temp)
#   {
#     subsets_temp <- sub_groups[[cat]]
#     names(subsets_temp) <- NULL
#     sub_panels[[cat]] <- conditionalPanel(
#       condition = sprintf("input.category == '%s'",  cat),
#       select_panel(
#         sprintf("subsetby_%s", cat), sprintf("Feature Subset (%s)", cat), 
#         subsets_temp))
#   }
#   sub_panels
# }
# 
# color_panels_ui <- function(colors){
#   lapply(colors, function(x){
#     conditionalPanel(
#       condition = sprintf("input.category == '%s'", x[[1]]),
#       select_panel(
#         sprintf("colorby_%s", x[[1]]), sprintf("Color By (%s)", x[[1]]), 
#         x[[2]], 1))
#   })
# }
# 
# shape_panels_ui <- function(shapes){
#   lapply(shapes, function(x){
#     conditionalPanel(
#       condition = sprintf("input.category == '%s'",  x[[1]]),
#       select_panel(
#         sprintf("shapeby_%s", x[[1]]), sprintf("Shape By (%s)", x[[1]]), 
#         x[[2]], 2))
#   })
# }
# 
# label_panels_ui <- function(labels){
#   lapply(labels, function(x){
#     conditionalPanel(
#       condition = sprintf("input.category == '%s'",  x[[1]]),
#       select_panel(
#         sprintf("labelby_%s", x[[1]]), sprintf("Label By (%s)", x[[1]]), 
#         x[[2]], 1))
#   })
# }
# 
# filter_panels_ui <- function(filters){
#   lapply(filters, function(x){
#     conditionalPanel(
#       condition = sprintf("input.category == '%s'",  x[[1]]),
#       select_panel(
#         sprintf("filterby_%s", x[[1]]), sprintf("Current Filter (%s)", x[[1]]), 
#         x[[2]]))
#   })
# }
# 
# thre_panels_ui <- function(thres){
#   if (is.null(thres))
#     return(NULL)
#   
#   lapply(thres, function(x){
#     conditionalPanel(
#       condition = sprintf("input.category == '%s' && input.scale == '%s'", 
#                           x[[1]], x[[2]]),
#       sliderInput(
#         get_thre(x[[1]], x[[2]]), "Threshold", min=x[[3]], max=x[[4]], 
#         value=(x[[3]]+x[[4]])/2, step=(x[[4]]-x[[3]])/10, round=-4,
#         ticks = FALSE)
#     )
#   })
# }
# 
# # the option boxes that will be presented to the user
# color_opts <- vector(mode = "list", length = num_cat)
# shape_opts <- vector(mode = "list", length = num_cat)
# label_opts <- vector(mode = "list", length = num_cat)
# filter_opts <- vector(mode = "list", length = num_cat)
# select_opts <- vector(mode = "list", length = length(select_ids))
# thre_opts <- vector(mode = "list", length = 2*num_cat)
# 
# # create the option boxes
# gen_index <- 0
# for (cn in 1:num_cat)
# {
#   order_gen <- order_total[[cn]]
#   cat <- name_cat[cn]
#   cols_unique_gen <- cols_unique[[cat]]
#   order_names <- colnames(order_gen)
#   
#   # colors
#   color_opts[[cn]] <- list(
#     "1"=cat, 
#     "2"=order_names[between(cols_unique_gen, 2, num_colors)]
#   )
#   
#   # shapes
#   shape_opts[[cn]] <- list(
#     "1"=cat, 
#     "2"=order_names[between(cols_unique_gen, 2, num_shapes)]
#   )
#   
#   # labels
#   label_opts[[cn]] <- list(
#     "1"=cat, 
#     "2"=order_names[between(cols_unique_gen, 2, num_labels)]
#   )
#   
#   # filters
#   filter_opts[[cn]] <- list(
#     "1"=cat, 
#     "2"=names(outline[[cat]])
#   )
#   
#   # selections
#   for (char in names(outline[[cat]]))
#   {
#     gen_index <- gen_index + 1
#     
#     select_opts[[gen_index]] <- conditionalPanel(
#       condition=sprintf(
#         "input.category == '%s' && input.filterby_%s == '%s'", cat, cat, char),
#       check_panel(get_select(cat, char), sprintf("Filter By (%s)", cat), 
#                   outline[[cat]][[char]])
#     )
#   }
#   
#   # thresholds
#   if (!is.null(thresholds))
#   {
#     for (sn in 1:2)
#     {
#       sca <- sca_options[sn]
#       thre_temp <- thresholds[[sca]][[cat]]
#       
#       thre_opts[[2*cn-2+sn]] <- list(
#         "1"=cat, 
#         "2"=sca, 
#         "3"=round(thre_temp[1], 4), 
#         "4"=round(thre_temp[2], 4)
#       )
#     }
#   }
#   else
#   {
#     thre_opts <- NULL
#   }
# }
# 
# overall_ui <- dashboardPage(
#   skin="blue",
#   dashboardHeader(title=app_title, titleWidth="100%"),
#   dashboardSidebar(
#     width=300,
#     sidebarMenu(
#       id = "sidebar_menu",
#       menuItem(
#         startExpanded=TRUE,
#         "Data Selection",
#         select_panel("category", "Category", cat_groups),
#         sub_panels_ui(cat_groups, sub_groups),
#         select_panel("embedding", "Method of Dimensionality Reduction", emb_options),
#         conditionalPanel(
#           condition = "
# input.embedding == 'PCA' || input.embedding == 'VAE' || input.embedding == 'UMAP'", 
#           select_panel("visualize", "Method of Visualization", vis_options)
#         ),
#         conditionalPanel(
#           condition = "input.embedding == 'Sets'",
#           conditionalPanel(
#             condition = "input.plotPanels == 'ggplot2'",
#             select_panel("upsetpref", "Method of Visualization", ups_options)),
#           conditionalPanel(
#             condition = "input.plotPanels == 'plotly2' || input.plotPanels == 'plotly3'",
#             select_panel("dendrogram", "Method of Visualization", den_options))
#         )
#       ),
#       menuItem(
#         "Parameters",
#         select_panel("scale", "Scale", sca_options),
#         conditionalPanel(
#           condition = "input.embedding != 'Sets'",
#           select_panel("normalize", "Normalization", nor_options),
#           select_panel("features", "Percentage of Features Used", fea_options)
#         ),
#         conditionalPanel(
#           condition = "input.visualize == 'Summarize' && (input.embedding == 'PCA' ||
#   input.embedding == 'VAE' || input.embedding == 'UMAP')",
#           "No data selection can be performed under these conditions.
#           Please switch to a non-summary plot."
#         ),
#         conditionalPanel(
#           condition = "input.visualize == 'Explore' && (input.embedding == 'PCA' ||
#   input.embedding == 'VAE' || input.embedding == 'UMAP')",
#           pc_slider(1, pc_cap),
#           conditionalPanel(
#             condition = "input.plotPanels == 'ggplot2' || 
#       input.plotPanels == 'plotly2' || input.plotPanels == 'plotly3'",
#             pc_slider(2, pc_cap)
#           ),
#           conditionalPanel(
#             condition = "input.plotPanels == 'plotly3'",
#             pc_slider(3, pc_cap)
#           )
#         ),
#         perplexity_ui(perplexity_types),
#         sets_ui(thre_panels_ui(thre_opts), max_cat_num)
#       ),
#       menuItem(
#         "Settings", 
#         check_panel("sMenu", "Settings", my_settings),
#         select_panel("palette", "Color Palette", pal_options),
#         numericInput("height", "Graph Height", value=graph_height, min=1, max=4000)
#       ),
#       menuItem(
#         "Filters",
#         do.call(conditionalPanel, c(
#           condition = "input.embedding != 'Sets' && (input.embedding == 'PHATE' || 
#         input.visualize != 'Summarize')",
#           color_panels_ui(color_opts)
#         )),
#         do.call(conditionalPanel, c(
#           condition = "input.embedding != 'Sets' && input.plotPanels == 'ggplot2' &&
#     (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
#           shape_panels_ui(shape_opts)
#         )),
#         do.call(conditionalPanel, c(
#           condition = "input.embedding != 'Sets' && input.plotPanels != 'beeswarm' &&
#           input.plotPanels != 'ggplot2' &&
#     (input.embedding == 'PHATE' || input.visualize != 'Summarize')",
#           label_panels_ui(label_opts)
#         )),
#         do.call(conditionalPanel, c(
#           condition = "input.visualize != 'Summarize' || 
#         input.embedding == 'Sets' || input.embedding == 'PHATE'",
#           filter_panels_ui(filter_opts), 
#           select_opts
#         ))
#       )
#     )
#   ),
#   dashboardBody(
#     shinyjs::useShinyjs(),
#     tags$head(tags$style(my_css_styling)),
#     box(
#       title = "Controls",
#       collapsible = TRUE, collapsed=TRUE, width="100%",
#       action("start", "Start Plotting", "chart-bar", "#FFF", "#0064C8", "#00356B"),
#       action("stop", "Stop Plotting", "ban", "#FFF", "#C90016", "#00356B"),
#       bookmarkButton(),
#       downloadButton('downloadData', 'Numeric Data'),
#       downloadButton('downloadMetadata', 'Metadata')
#     ),
#     tabBox(
#       width="100%",
#       id = 'plotPanels',
#       tabPanel("ggplot2", uiOutput("ggplot2UI")),
#       tabPanel("plotly2", uiOutput("plotly2UI")),
#       tabPanel("plotly3", uiOutput("plotly3UI")),
#       tabPanel("beeswarm", uiOutput("beeswarmUI")),
#       tabPanel("Numeric Data", id="num_data", 
#                DTOutput("num_data_table", width="100%") %>% my_spin()),
#       tabPanel("Metadata", id="metadata", 
#                DTOutput("metadata_table", width="100%") %>% my_spin())
#     ),
#     box(
#       title = "Documentation",
#       collapsible = TRUE, collapsed=TRUE, width="100%",
#       action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
#       action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
#       downloadButton('downloadInstructions', 'Instructions'),
#       downloadButton('downloadCitations', 'Citations')
#     ),
#     uiOutput("plainTitleUI"),
#     div(id="legend_out_spin", DTOutput("legend_out", width="100%") %>% my_spin()),
#     hr(),
#     "Developed by Justin Chang at the Gerstein Lab, 
#       under the mentorship of Joel Rozowsky."
#   )
# )

# saves the UI
# setwd(dep_loc); saveRDS(complete_ui, "complete_ui.rds")