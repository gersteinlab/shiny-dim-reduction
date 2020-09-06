# The purpose of this file is to store browser parameters, long text
# strings, and non-generalizable user interfaces for the main app. 
# None of these should depend on other global variables!
# Note: Most interface numbers are rounded to 4 decimal places.
# This likely will not change in the near future.

# ------------------
# BROWSER PARAMETERS
# ------------------

# Only allow "Color By" on a characteristic with <= num_colors distinct values.
num_colors <- 60
# Only allow "Shape By" on a characteristic with <= num_shapes distinct values.
num_shapes <- 60
# Only allow filtering on a characteristic with <= num_filters distinct values.
num_filters <- 60
# The height of a graph by default. Depends on browser interpretation.
graph_height <- 600
# the maximum size of a matrix (prevent memory overflow)
max_points <- 16777216 # 2^24
# the maximum number of points on a heatmap
max_heatma <- 1048576 # 2^20
# the maximum number of columns on a dendrogram
max_dendro <- 512 # 2^9

# plot panel options
pan_options <- c("ggplot2", "plotly2", "plotly3", "beeswarm")
# palette options
pal_options <- list(
  "Base"=c("Custom", "Rainbow", "Heat", "Terrain", "Topography", "CM"),
  "Viridis"=c("Viridis", "Magma", "Plasma", "Inferno", "Cividis")
)
# dendrogram options
den_options <- c("Variance", "Correlation")
# upset plot options
ups_options <- c("Frequency", "Degree")
# settings options
my_settings <- c("Embed Title", "Embed Legend", "Notifications", 
                 "Limit Memory Use", "Uninverted Colors")

# ----------
# LONG TEXTS
# ----------

bibliography <- function(app_citations){sprintf(
  "Developed at Gerstein Lab from 2019-2020.
<br><br>
Work conducted under the mentorship of Joel Rozowsky and Mark Gerstein. 
The citations below are in the format requested by their respective creators.
<br><br>
<b>Data Sources</b>
<br>
%s
<br><br>
<b>R Packages</b>
<br>
<u>Rtsne:</u> Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor 
Embedding using a Barnes-Hut Implementation, URL: 
<a href=\"https://github.com/jkrijthe/Rtsne\" target=\"_blank\">
https://github.com/jkrijthe/Rtsne</a>
<br>
<u>Keras:</u> Keras, (2018), GitHub repository, 
https://github.com/charlespwd/project-title
<a href=\"https://github.com/keras-team/keras\" target=\"_blank\">
https://github.com/keras-team/keras</a>
<br>
<u>UMAP:</u> McInnes et al., (2018). UMAP: Uniform Manifold Approximation and 
Projection. Journal of Open Source Software, 3(29), 861, 
<a href=\"https://doi.org/10.21105/joss.00861\" target=\"_blank\">
https://doi.org/10.21105/joss.00861</a>
<br>
<u>PHATE:</u> Moon, K.R., van Dijk, D., Wang, Z. et al. 
Visualizing structure and transitions in high-dimensional biological data. 
Nat Biotechnol 37, 1482-1492 (2019). 
<a href=\"https://doi.org/10.1038/s41587-019-0336-3\" target=\"_blank\">
https://doi.org/10.1038/s41587-019-0336-3</a>
<br>
<u>UpSetR:</u> Jake R Conway, Alexander Lex, Nils Gehlenborg UpSetR: An R Package 
for the Visualization of Intersecting Sets and their Properties doi: 
<a href=\"https://doi.org/10.1093/bioinformatics/btx364\" target=\"_blank\">
https://doi.org/10.1093/bioinformatics/btx364</a>
<br>
<u>heatmaply:</u> Galili, Tal, O'Callaghan, Alan, Sidi, Jonathan, Sievert, 
Carson (2017). \"heatmaply: an R package for creating interactive cluster heatmaps 
for online publishing.\" Bioinformatics. doi:
<a href=\"http://dx.doi.org/10.1093/bioinformatics/btx657\" target=\"_blank\">
http://dx.doi.org/10.1093/bioinformatics/btx657</a>
<br>
<u>Cividis:</u> Nu%sez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow. 
\"Optimizing colormaps with consideration for color vision deficiency to enable 
accurate interpretation of scientific data.\" PloS one 13.7 (2018): e0199239.
<br><br>
<b>Further Reading</b>
<br>
<u>Optimizing tSNE:</u> Wattenberg, et al., \"How to Use t-SNE Effectively\",
Distill, 2016. <a href=\"http://doi.org/10.23915/distill.00002\" target=\"_blank\">
http://doi.org/10.23915/distill.00002</a>
<br>
<u>James Diao's ERCC Plotting Tool:</u> 
<a href=\"https://github.com/jamesdiao/ERCC-Plotting-Tool\" target=\"_blank\">
https://github.com/jamesdiao/ERCC-Plotting-Tool</a>", app_citations, intToUtf8(0x00F1))}

instructions <- 
  "Welcome! Please feel free to explore this dimensionality reduction tool.
Developed at Gerstein Lab from 2019-2020.
<br><br>
In Shiny, values are reactive and observe their dependencies. If one of their 
dependencies is invalidated - meaning one of the inputs has changed - then they 
recalculate their values. Once a reactive value is recalculated, the functions 
that depend on it will be invalidated, causing their own downstream recalculation. 
This approach allows the app to be highly responsive, without constantly performing 
upstream calculations. Users can therefore freely change parameters as they go. 
Non-visible outputs are also not activated, which saves drawing time.
<br><br>
Note that the settings in 'Graphing' are applied in reverse order. As an example,
'Color Palette' is applied after 'Method of Dimensionality Reduction', which is
applied after normalization, which is applied after scaling.
<br><br>
If you want to save plotly output as an image, use the camera icon.
To save all other output, right click the image and 'Save image as ...'
<br><br>
<b>Glossary of Terms</b>
<ul>
<li><u>Settings Menu:</u> 
If 'Embed Title' is checked, then the title of the plot will be included within the 
plot graphic. Otherwise, it will be displayed as plaintext below the plot. Embedded 
titles are not supported for UpSetR. If 'Show Legend' is  checked, then the plot will 
contain a legend. Otherwise, no legend will be included. If 'Notifications' is 
checked, then relevant notifications will appear in the bottom-right corner over time. 
Otherwise, no notifications will appear. If 'Limit Memory Use' is checked, then 
lightweight functions will be used to improve the stability of this tool. This is 
necessary for the online version of this tool, which is hosted on a server with very 
limited memory. For the offline executable, disabling this setting will increase 
performance. If 'Uninverted Colors' is checked, then color scales will be displayed in 
the default order. Otherwise, color scales will be reversed.
</li>
<li><u>Color Palette:</u>
For plotting, this tool supports 10 color scales. The color scales inherent to R are 
'Custom', 'Rainbow', 'Heat', 'Terrain', and 'Topography'. The color scales inherent 
to Viridis are 'Viridis', 'Magma', 'Plasma', 'Inferno', and 'Cividis'. Cividis 
'enables nearly-identical visual-data interpretation' for color-deficient vision, 
'is perceptually uniform in hue and brightness, and increases in brightness linearly'.
</li>
<li><u>Method of Visualization:</u> 
Explore: Plots combinations of principal components. 
Summarize: Generates a filter-free summary, with best-fit lines in ggplot2 and plotly3.
tSNE: Flattens all components into a t-distributed Stochastic Neighbor Embedding.
Frequency: Sorts the columns of the UpSetR plot by frequencies of factor subsets.
Degree: Sorts the columns of the UpSetR plot by degrees of factor subsets.
Variance: Sorts columns of heatmap by variance.
Correlation: Performs correlation clustering, with dendrograms in plotly3.
</li>
<li><u>Method of Dimensionality Reduction:</u> 
PCA: Principal Component Analysis.
VAE: Variational Auto-Encoder.
UMAP: Uniform Manifold Approximation and Projection.
PHATE: Potential of Heat diffusion for Affinity-based Transition Embedding.
Sets: Uses a user-calculated threshold to analyze characteristic intersections.
</li>
<li><u>Percentage of Features Used:</u> 
Let x be the fraction of extraneous features used.
Let t be the total number of features. 
Let c be the number of components in the final reduction.
Then the first {c + x * (t - c)} features, in decreasing order by variance,
will be used to form the model.
Due to hardware limitations, t is capped at 24000 for VAE.
</li>
<li><u>Normalization:</u> 
Raw data is globally normalized - features retain their relative magnitudes, the 
minimum of the whole matrix is 0, and the maximum of the whole matrix is 1. 
Normalized data is locally normalized - the minimum of each feature is 0 and the 
maximum of each feature is 1. Normalization generally benefits neural networks, 
so it is suggested for VAE. On the other hand, normalization is usually detrimental 
to PCA, as it removes the relative magnitudes of all features.
</li>
<li><u>Scale:</u> 
Logarithmic data underwent a transformation of f(x) = log2(x+1).
</li>
<li><u>Feature Subset:</u> 
Choose a subset of features for use in further analysis.
</li>
<li><u>Category:</u> 
A category is a set of samples with conserved features. Since categories do not 
necessarily share common metadata characteristics, distinct inputs exist for each 
category in subsetting, coloring, labeling, filtering, selecting, and thresholding.
</li>
<li><u>Start Plotting:</u> 
Activates plotting and causes plots to update instantaneously. 
</li>
<li><u>Stop Plotting:</u> 
Stops plotting and allows settings to be updated without waiting.
</li>
<li><u>Bookmark:</u> 
Creates a URL that replicates this session.
</li>
<li><u>Numeric Data:</u> 
Downloads the numeric data used to produce the current plot.
</li>
<li><u>Metadata:</u> 
Downloads the metadata used to produce the current plot.
</li>
<li><u>Displayed Components:</u> 
Displayed Component 1 denotes the component, after dimensionality reduction, 
that will be shown on the x-axis. Displayed Component 2 denotes the component, 
after dimensionality reduction, that will be shown on the y-axis. For plotly3, 
Displayed Component 3 denotes the component, after dimensionality reduction, 
that will be shown on the z-axis. Note that components can equal each other.
</li>
<li><u>Color By, Shape By, Label By:</u>
What category should points on the graph be colored / shaped / labeled by?
(Note: depends on the category selected.)
</li>
<li><u>Current Filter, Filter By:</u> 
'Current Filter' lists all available categories for which filters can be applied. 
Filters for all categories are applied concurrently in an AND operation. In other 
words, only points in the intersection of all filters will be plotted. Filters consist 
of including / excluding factors of a selected metadata characteristic.
(Note: depends on the category selected.)
</li>
<li><u>Perplexity:</u>
Let a gene's expression range be normalized to (0,1) after the scaling transformation. 
Then a gene will be considered present in a categorical characteristic if it is 
expressed above this threshold. The bounds of this slider were selected to ensure a 
broad range of expression patterns without overflowing memory.
</li>
<li><u>Threshold:</u> 
Perplexity is a hyperparameter used in dimensionality reduction methods that create 
clusters. It is analogous to the expected number of neighbors for any data point.
</li>
<li><u>Fraction of Samples:</u> 
Suppose S samples belong to a characteristic. 
Suppose a gene is expressed at the threshold level in G of those samples. 
Then this slider determines the acceptable values of G/S to be displayed.
</li>
<li><u>Fraction of Characteristics:</u> 
This slider determines the percentage of possible sets that must contain a gene
for the gene to be displayed. A gene is contained if and only if it is present
in an appropriate fraction of samples. As an example, housekeeping genes (present 
in all characteristics) - will be removed by setting this value to less than 1.
</li></ul>"

no_autofill <- 
  "document.getElementById('password').setAttribute('autocomplete','new-password')"

my_css_styling <- HTML("
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

/* Everyone's favorite color - Yale Blue! */
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
")

# ----------
# INTERFACES
# ----------

perplexity_ui <- function(p_types){
  if (length(p_types) < 1)
    return(NULL)
  
  conditionalPanel(
    condition = "input.embedding != 'Sets' && 
    (input.embedding == 'PHATE' || input.visualize == 'tSNE' ||
    (input.embedding == 'UMAP' && input.visualize != 'Summarize'))",
    select_panel("perplexity", "Perplexity", p_types, ceiling(length(p_types)/2))
  )
}

sets_ui <- function(thre_opts, max){
  min_max <- num_filters
  for (i in 0:4)
  {
    for (j in 0:4)
    {
      target <- 2^i * 5*j
      if (target >= max && target < min_max)
        min_max <- target
    }
  }
  
  do.call(conditionalPanel, c(
    condition = "input.embedding == 'Sets'", thre_opts, list(
      sliderInput(
        "set_f1", "Fraction of Samples", 
        min=0, max=1, value=c(0.5,1), step=0.01,
        ticks = FALSE, dragRange=FALSE),
      sliderInput(
        "set_f2", "Fraction of Characteristics", 
        min=0, max=1, value=c(0,1), step=1.0/min_max,
        ticks = FALSE, dragRange=FALSE)
    )
  ))
}

sub_panels_ui <- function(cat_groups, sub_groups){
  name_cat_temp <- unlist(cat_groups)
  sub_panels <- my_empty_list(name_cat_temp)
  for (cat in name_cat_temp)
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

color_panels_ui <- function(colors){
  lapply(colors, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'", x[[1]]),
      select_panel(
        sprintf("colorby_%s", x[[1]]), sprintf("Color By (%s)", x[[1]]), 
        x[[2]]))
  })
}

shape_panels_ui <- function(shapes){
  lapply(shapes, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'",  x[[1]]),
      select_panel(
        sprintf("shapeby_%s", x[[1]]), sprintf("Label By (%s)", x[[1]]), 
        x[[2]], 2))
  })
}

filter_panels_ui <- function(filters){
  lapply(filters, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s'",  x[[1]]),
      select_panel(
        sprintf("filterby_%s", x[[1]]), sprintf("Current Filter (%s)", x[[1]]), 
        x[[2]]))
  })
}

thre_panels_ui <- function(thres){
  if (is.null(thres))
    return(NULL)
  
  lapply(thres, function(x){
    conditionalPanel(
      condition = sprintf("input.category == '%s' && input.scale == '%s'", 
                          x[[1]], x[[2]]),
      sliderInput(
        get_thre(x[[1]], x[[2]]), "Threshold", min=x[[3]], max=x[[4]], 
        value=(x[[3]]+x[[4]])/2, step=(x[[4]]-x[[3]])/10, round=-4,
        ticks = FALSE)
    )
  })
}