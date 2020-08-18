# The purpose of this file is to load dependencies and use them to generate
# selection options for the app.

# -----------------
# LOAD DEPENDENCIES
# -----------------

dir <- "dependencies"

# ordered from most to least important (ex: app_title can be missing and
# not significantly affect functionality)
categories_full <- get_from_dir(
  "categories_full.rds", 
  NULL, dir
)

# a bit of exposition regarding categories_full:
# first, break up into groups (ex: cCREs, Expression, Proteomics)
# second, break up into categories (ex: H3K27ac, H3K9me3, Methylation)
# note that all categories MUST be unique, even if in different groups
# also, the value of categories_full$cCREs$H3K27ac must be the number of
# features originally in that dataset before dimensionality reduction

order_total <- get_from_dir(
  "order_total.rds",
  NULL, dir
)

amazon_keys <- get_from_dir(
  "amazon_keys.rds",
  NULL, dir
)

perplexity_types <- get_from_dir(
  "perplexity_types.rds", 
  NULL, dir
)

thresholds <- get_from_dir(
  "thresholds.rds",
  NULL, dir
)

pc_cap <- get_from_dir(
  "pc_cap.rds",
  3, dir
)

app_title <- get_from_dir(
  "app_title.rds", 
  "Dimensionality Reduction Tool", dir)

app_citations <- get_from_dir(
  "app_citations.rds", 
  "No data citations could be found.", dir)

citations <- bibliography(app_citations)

custom_color_scales <- get_from_dir(
  "custom_color_scales.rds",
  NULL, dir
)

decorations <- get_from_dir(
  "decorations.rds",
  NULL, dir
)

rm(dir)

# a bit of exposition regarding decorations:
# first of all, each decoration in the list has a name
# and then two entries:
# (i) a vector of categories to which the decoration applies
# (ii) a list containing (a) a reference character vector and 
# (b) indices of that reference vector that constitute subsets

# sets parameters after getting keys for Amazon AWS
Sys.setenv("AWS_ACCESS_KEY_ID" = amazon_keys[1],
           "AWS_SECRET_ACCESS_KEY" = amazon_keys[2])
aws_bucket <- amazon_keys[3]

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

# create the outline
cols_unique <- my_empty_list(name_cat)
num_filchars <- 0
for (cat in name_cat)
{
  order_gen <- order_total[[cat]]
  
  # number of factor levels per characteristic
  cols_unique[[cat]] <- apply(order_gen, 2, function(i) length(unique(i)))
  
  # characteristics with a factor number in the range 2 <= x <= num_filters
  filterable <- colnames(order_gen)[between(cols_unique[[cat]], 2, num_filters)]
  
  outline[[cat]] <- my_empty_list(filterable)
  num_filchars <- num_filchars + length(filterable)
  for (filchar in filterable)
    outline[[cat]][[filchar]] <- get_opt(order_gen[[filchar]])
}
rm(filchar, filterable, order_gen)

# ----------------
# GENERATE OPTIONS
# ----------------

# the option boxes that will be presented to the user
color_opts <- vector(mode = "list", length = num_cat)
shape_opts <- vector(mode = "list", length = num_cat)
filter_opts <- vector(mode = "list", length = num_cat)
select_opts <- vector(mode = "list", length = num_filchars)
thre_opts <- vector(mode = "list", length = 2*num_cat)

# count max number of characteristics
max_cat_num <- 0
# all the IDs that will be compressed for the URL, made from the outline
select_ids <- rep("", num_filchars)

# create the option boxes
gen_index <- 0
for (cn in 1:num_cat)
{
  order_gen <- order_total[[cn]]
  cat <- name_cat[cn]
  cols_unique_gen <- cols_unique[[cat]]
  order_names <- colnames(order_gen)
  
  # colors
  color_opts[[cn]] <- list(
    "1"=cat, 
    "2"=order_names[between(cols_unique_gen, 2, num_colors)]
  )
  
  # shapes
  shape_opts[[cn]] <- list(
    "1"=cat, 
    "2"=order_names[between(cols_unique_gen, 2, num_shapes)]
  )
  
  # filters
  filter_opts[[cn]] <- list(
    "1"=cat, 
    "2"=names(outline[[cat]])
  )
  
  # selections
  for (char in names(outline[[cat]]))
  {
    gen_index <- gen_index + 1
    opts <- outline[[cat]][[char]]
    sele <- get_select(cat, char)
    max_cat_num <- max(length(opts), max_cat_num)
    select_ids[gen_index] <- sele
    
    select_opts[[gen_index]] <- conditionalPanel(
      condition=sprintf(
        "input.category == '%s' && input.filterby_%s == '%s'", cat, cat, char),
      check_panel(sele, sprintf("Filter By (%s)", cat), opts)
    )
  }
  
  # thresholds
  if (!is.null(thresholds))
  {
    for (sn in 1:2)
    {
      sca <- sca_options[sn]
      thre_temp <- thresholds[[sca]][[cat]]
      
      thre_opts[[2*cn-2+sn]] <- list(
        "1"=cat, 
        "2"=sca, 
        "3"=round(thre_temp[1], 4), 
        "4"=round(thre_temp[2], 4)
      )
    }
  }
  else
  {
    thre_opts <- NULL
  }
}
rm(cn, sn)

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
  "metadata_table_search",
  "metadata_table_state",
  "metadata_table_cell_clicked",
  "metadata_table_search_columns",
  "metadata_table_rows_current",
  "metadata_table_rows_all",
  "num_data_table_search",
  "num_data_table_state",
  "num_data_table_cell_clicked",
  "num_data_table_search_columns",
  "num_data_table_rows_current",
  "num_data_table_rows_all",
  ".clientValue-default-plotlyCrosstalkOpts",
  "plotly_hover-A",
  "plotly_afterplot-A",
  "plotly_relayout-A",
  
  sprintf("subsetby_%s", name_cat),
  sprintf("colorby_%s", name_cat), 
  sprintf("shapeby_%s", name_cat), 
  sprintf("filterby_%s", name_cat), 
  select_ids, 
  get_thre(name_cat, "Logarithmic"), get_thre(name_cat, "Linear"), 
  
  "start", "stop", "toggle", "central_nav", "instructions", "citations", 
  "sMenu", "category", "scale", "normalize", "features", "embedding", 
  "visualize", "perplexity", "upsetpref", "dendrogram", 
  "palette", "plotPanels",  
  "set_f1", "set_f2", "pc1", "pc2", "pc3"
)