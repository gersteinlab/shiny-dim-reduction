# The purpose of this file is to store libraries and functions
# for server-side logic. None of these should depend on global variables!
# Each section is sorted from most algorithmic to least algorithmic,
# though intended purpose may create a more reasonable ordering.

source("outline.R", encoding="UTF-8")

require("viridis")
require("ggplot2")
require("plotly")
require("UpSetR")
require("VennDiagram")
require("beeswarm")
require("heatmaply")
require("DT")

# -----------
# COMPUTATION
# -----------

# given a matrix and two ranges f1, f2,
# return the rows where the of number of entries between f1[1], f1[2]
# is between f2[1], f2[2] and omit empty columns
set_f1_f2 <- function(data, f1, f2)
{
  if (class(data) != "matrix" || length(data) < 1 || ncol(data) < 1)
    return(matrix(nrow=0, ncol=0))
  for (j in 1:ncol(data))
    data[,j] <- ifelse(between(data[,j], f1[1], f1[2]), data[,j], NaN)
  valid <- !is.nan(data)
  data[between(rowSums(valid), f2[1], f2[2]), colSums(valid) > 0, drop = FALSE]
}

# given a matrix, returns a data frame where
# all non-NaN entries become 1 and all NaN entries become 0
num_nan_binary <- function(data)
{
  data[!is.nan(data)] <- 1
  data[is.nan(data)] <- 0
  data.frame(data)
}

# checks if a value is invalid with respect to a range
# if given an ordered pair, returns whether either value is invalid
range_invalid <- function(value, min, max)
{
  if (length(value) == 2)
    return(range_invalid(value[1], min, max) || range_invalid(value[2], min, max))
  
  length(value) != 1 || is.na(value) || is.nan(value) || value < min || value > max
}

# given a list of numeric vectors, returns get_opt(name, length) for each vector
name_num_map <- function(list_num)
{
  mapply(get_opt, names(list_num), lapply(list_num, length), USE.NAMES = FALSE) 
}

# calculates the number of truncated features given pc_cap, 
# the inputted feature fraction (feat), and the total number of features.
calc_feat <- function(pc_cap, feat, total)
{
  pc_cap + ceiling(feat * (total - pc_cap))
}

# Useful function for changing "PC 1" to "prcomp 1", getting length, etc.
pc <- function(name) 
{
  if (missing(name))
    name <- "1"
  sprintf("Component %s", name)
}

# ----------------------
# GENERAL GRAPHS / TOOLS
# ----------------------

make_transparent <- function(color, alpha = 0.5)
{
  sprintf("%s44", substr(color, start=1, stop=7))
}

# Generates a color sequence of length n_colors with the given type.
# "Base"=c("Rainbow", "Heat", "Terrain", "Topography", "CM"),
# "Viridis"=c("Viridis", "Magma", "Plasma", "Inferno", "Cividis")
color_seq <- function(n_colors, color_type = "Rainbow", reverse = FALSE) 
{
  # returns bulldog blue if only one color is needed
  if (n_colors < 2)
    return("#00356B")
  
  if (color_type == "Heat")
    return(heat.colors(n_colors, rev=reverse))
  if (color_type == "Terrain")
    return(terrain.colors(n_colors, rev=reverse))
  if (color_type == "Topography")
    return(topo.colors(n_colors, rev=reverse))
  if (color_type == "CM")
    return(cm.colors(n_colors, rev=reverse))
  if (color_type == "Viridis")
    return(viridis::viridis(n_colors, direction = ifelse(reverse, -1, 1)))
  if (color_type == "Magma")
    return(viridis::magma(n_colors, direction = ifelse(reverse, -1, 1)))
  if (color_type == "Plasma")
    return(plasma(n_colors, direction = ifelse(reverse, -1, 1)))
  if (color_type == "Inferno")
    return(inferno(n_colors, direction = ifelse(reverse, -1, 1)))
  if (color_type == "Cividis")
    return(cividis(n_colors, direction = ifelse(reverse, -1, 1)))
  
  # rainbow, the default (also returned to cover for custom color scales)
  if (reverse)
    return(hcl(n_colors:1 * (360/(n_colors+1))-15, 160, 60))
  else
    return(hcl(1:n_colors * (360/(n_colors+1))-15, 160, 60))
}

# my personal favorite ordering of the 25 default R plot shapes
ggplot2_shapes_unique <- c(15:25,0:2,5:6,3:4,8,7,9:14)
# Generates a shape sequence of length n for ggplot2.
ggplot2_shape_seq <- function(n) 
{
  my_seq <- 0:(n-1) %% length(ggplot2_shapes_unique)
  ggplot2_shapes_unique[my_seq+1]
}

# Displays a default graph, which is accepted by ggplot2 and plotly.
ggplot2_null <- function()
{
  df <- data.frame()
  ggplot(df) + geom_point() + 
    ggtitle("This plot cannot be displayed.") +
    xlim(0, 1) + ylim(0, 1)
}

# Beeswarm / box plot, rel stands for relation
boxplot_beeswarm <- function(data, rel, xlab, ylab, names, 
                             box_colors, bee_colors, title)
{
  boxplot(rel, data=data, xlab=xlab, ylab=ylab, 
          names=names, col=box_colors, outline = FALSE, main=title)
  
  beeswarm(rel, data=data, xlab=xlab, ylab=ylab,
           labels=names, col=bee_colors, corral="random",
           main=title, pch=16, add=TRUE) # filled circles
}

# -------------------
# SCATTER PLOT GRAPHS
# -------------------

# Plots all data points at (x,y) ...
# ... with appropriate colors in cq, symbols in sq.
# The graph will have features (x_axis, y_axis, title), with legend
# determining whether a legend will be displayed.
ggplot2_2d <- function(x, y, x_axis, y_axis, 
                       color, symbol, cq, sq, title, legend) 
{
  df <- data.frame(dx = x, dy = y, Color = color, Shape = symbol)
  ggplot(df, aes(x = dx, y = dy, color = Color, shape = Shape)) + 
    ggtitle(title) + xlab(x_axis) + ylab(y_axis) + 
    scale_color_manual(values = cq) +
    scale_shape_manual(values = ggplot2_shape_seq(sq)) +
    theme(plot.title=element_text(size=22,face="bold"), 
          axis.title.x=element_text(size=16, margin = margin(t = 10)), 
          axis.title.y=element_text(size=16, margin = margin(r = 10)), 
          axis.text=element_text(size=12),
          legend.title=element_text(size=16), 
          legend.text=element_text(size=10),
          legend.position=ifelse(legend, "right", "none"),
          plot.margin = margin(0.5, 0, 0, 0, "cm"),
          panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                          colour = "gray")) + 
    geom_hline(aes(yintercept = 0), size = 0.5) +
    geom_vline(aes(xintercept = 0), size = 0.5) +
    geom_point(size = 2.4) + 
    guides(color = guide_legend(order = 1))
}

# Plots all data points at (x,y) ...
# ... with appropriate colors in colors in c_seq, text.
# The graph will have features (x_axis, y_axis, title), with legend
# determining whether a legend will be displayed.
plotly_2d <- function(x, y, x_axis, y_axis, mode, 
                      color, text, c_seq, title, legend) 
{
  plot_ly(x = x, y = y, mode = mode,
          color = color, colors = c_seq,  
          text = text, 
          marker = list(size = 6, symbol = 'circle'),
          hovertemplate = paste(
            "<b>%{text}</b>",
            "<br>(%{x:.4f}, %{y:.4f})",
            "<extra></extra>"), type="scatter") %>% layout(
              title = title,
              xaxis = list(title=x_axis),
              yaxis = list(title=y_axis),
              showlegend = legend)
}

# Plots all data points at (x,y,z) ...
# ... with appropriate colors in c_seq, text.
# The graph will have features (x_axis, y_axis, z_axis, title), with legend
# determining whether a legend will be displayed.
plotly_3d <- function(x, y, z, x_axis, y_axis, z_axis,
                      color, text, c_seq, title, legend) 
{
  plot_ly(x = x, y = y, z = z, mode = "markers",  
          color = color, colors = c_seq,
          text = text, 
          marker = list(size = 4, symbol = 'circle'),
          hovertemplate = paste(
            "<b>%{text}</b>",
            "<br>(%{x:.4f}, %{y:.4f}, %{z:.4f})",
            "<extra></extra>"), type="scatter3d") %>% layout(
            title = title,
            scene = list(xaxis = list(title = x_axis),
                         yaxis = list(title = y_axis),
                         zaxis = list(title = z_axis)),
            showlegend = legend)
}

# -----------------------
# SET INTERSECTION GRAPHS
# -----------------------

# Creates an UpSetR plot with the desired aesthetic.
upset_custom <- function(data, legend, nintersects) 
{
  upset(data, nsets = ncol(data), nintersects = nintersects, 
        sets.x.label = "Features Per Factor Level", 
        mainbar.y.label = "Features Per Factor Subset", 
        show.numbers = ifelse(legend, "yes", "no"),
        order.by = "freq", decreasing = T, mb.ratio = c(0.7, 0.3), text.scale = 1,
        line.size = 0.1, point.size = 2.1, shade.alpha = 0.4, matrix.dot.alpha = 0.5, 
        matrix.color = "royalblue4", main.bar.color = "royalblue4",
        sets.bar.color = "royalblue4", shade.color = "lightskyblue")
}

# Draws a single set venn diagram, where data is a one-column matrix / data frame
venn1_custom <- function(data, legend) 
{
  draw.single.venn(
    nrow(data), category = ifelse(legend, colnames(data)[1], ""), 
    lwd = 2, lty = "solid", cex = 1,
    fill = "#0064c8", alpha = 0.5, 
    
    ind = TRUE, margin = 0.025, cat.just = list(c(0.5, 0.5)),
    cat.default.pos = "outer", cat.prompts = FALSE, 
    cat.pos = 0, cat.dist = 0.025, cat.cex = 1, 
    rotation.degree = 0, rotation.centre = c(0.5, 0.5), 
    col = "black", label.col = "black", cat.col = "black",
    fontface = "plain", fontfamily = "serif", 
    cat.fontface = "plain", cat.fontfamily = "serif")
}

# creates a variance-based heatmap for sets on plotly
plotly_heatmap_variance <- function(binary, colors, title, legend, smooth)
{
  rows <- sprintf("X:%s", substring(rownames(binary), 0, 50))
  cols <- sprintf("Y:%s", substring(colnames(binary), 0, 50))
  rownames(binary) <- NULL
  colnames(binary) <- NULL
  
  plot_ly(x = rows, y = cols, z = t(binary), 
          type="heatmap", 
          zsmooth = ifelse(smooth, "best", "false"), 
          colors=colors) %>% layout(
            title = title, 
            showlegend = legend)
}

# creates a correlation-based heatmap for sets on plotly
plotly_heatmap_dendrogram <- function(binary, colors, title, legend, dend)
{
  binary[is.na(binary)] <- 0
  heatmaply(
    t(binary),
    main=title, 
    hide_colorbar=!legend, 
    colors=colors, 
    plot_method = "plotly",
    colorbar_len = ifelse(dend, 0.8, 1),
    showticklabels = c(FALSE, TRUE),
    dendrogram = ifelse(dend, "both", "none")) 
}

# ----------
# DATATABLES
# ----------

empty_df <- matrix(nrow=0, ncol=1) %>% data.frame()
colnames(empty_df) <- "Unknown"

# Creates a datatable from a data frame
my_datatable <- function(df)
{
  if (class(df) != "data.frame" || ncol(df) < 1)
    df <- empty_df
  
  datatable(
    df, editable=FALSE, escape=TRUE, filter="top", 
    selection="none", options=list(
      scrollX=TRUE,
      scrollY=TRUE,
      autoWidth=FALSE
    )
  )
}