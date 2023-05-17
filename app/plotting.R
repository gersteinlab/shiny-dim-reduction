# The purpose of this file is to store libraries and functions
# for server-side logic. None of these should depend on global variables!
# Each section is sorted from most algorithmic to least algorithmic,
# though intended purpose may create a more reasonable ordering.

if (!exists("sdr_config"))
  source("install.R")

library(viridis)
library(ggplot2)
library(plotly)
library(UpSetR)
library(VennDiagram)
library(beeswarm)
library(heatmaply)
library(DT)

# ----------------------
# GENERAL GRAPHS / TOOLS
# ----------------------

# adds a transparency of alpha = 0.5 to a color
make_transparent <- function(color)
{
  sprintf("%s44", substr(color, start = 1, stop = 7))
}

# bulldog blue
single_color_seq <- "#00356B"
# adds harvard crimson
double_color_seq <- function(reverse = FALSE)
{
  if (reverse)
    return(c(single_color_seq, "#C90016"))
  c("#C90016", single_color_seq)
}

# lists all color palette options
color_palettes <- list(
  "Custom" = c("Custom", "Grayscale", "None"),
  "Base" = c("Rainbow", "Heat", "Terrain", "Topography", "CM"),
  "Viridis" = c("Viridis", "Magma", "Plasma", "Inferno", "Cividis")
)

# Generates a color sequence of length n_colors with the given type
# note: all palettes except for "Custom" are accepted
color_seq <- function(n_colors, color_type = "Rainbow", reverse = FALSE)
{
  if (color_type == "None")
    return(rep("#000000", n_colors))

  # returns single color seq if only one color needed
  if (n_colors < 2)
    return(single_color_seq)

  if (color_type == "Grayscale")
    return(grey.colors(n_colors, rev = reverse))
  if (color_type == "Heat")
    return(heat.colors(n_colors, rev = reverse))
  if (color_type == "Terrain")
    return(terrain.colors(n_colors, rev = reverse))
  if (color_type == "Topography")
    return(topo.colors(n_colors, rev = reverse))
  if (color_type == "CM")
    return(cm.colors(n_colors, rev = reverse))
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
  if (color_type == "Rainbow")
  {
    c_seq <- hcl(seq_len(n_colors) * (360/(n_colors+1))-15, 160, 60)
    if (reverse)
      return(rev(c_seq))
    return(c_seq)
  }
  stop(sprintf("%s is not a supported color_type.", color_type))
}

# get remainder of a but put it in the range 1:b
mod_from_one <- function(a, b)
{
  (a - 1) %% b + 1
}

# my personal favorite ordering of the 25 default R plot shapes
ggplot2_shapes_unique <- c(15:25, 0:2, 5:6, 3:4, 8, 7, 9:14)
# Generates a shape sequence of length n for ggplot2.
ggplot2_shape_seq <- function(n)
{
  ggplot2_shapes_unique[mod_from_one(seq_len(n), length(ggplot2_shapes_unique))]
}

# Displays a default graph, which is accepted by ggplot2 and plotly.
ggplot2_null <- function()
{
  df <- data.frame()
  ggplot(df) + geom_point() +
    ggtitle("This plot cannot be displayed.") +
    xlim(0, 1) + ylim(0, 1)
}

# Boxplot + Beeswarm, with partially transparent boxplot
# data: a data frame where each row represents a dot,
# column 1 stores the label, and column 2 stores the y-coord
boxplot_beeswarm <- function(data, colors, title = "", legend = TRUE)
{
  xlab <- colnames(data)[1]
  ylab <- colnames(data)[2]
  rel <- get(ylab) ~ get(xlab)
  x_bins <- unique(data[,1])

  if (!legend)
    x_bins <- 1:length(x_bins)

  boxplot(rel, data = data, xlab = xlab, ylab = ylab, names = x_bins,
          col=make_transparent(colors), outline = FALSE, main = title) # transparency

  beeswarm(rel, data = data, xlab = xlab, ylab = ylab, labels = x_bins,
           col = colors, corral = "random", main = title, pch = 16, add = TRUE) # filled circles
}

# -------------------
# SCATTER PLOT GRAPHS
# -------------------

# Plots all data points at (x,y) ...
# ... with appropriate colors in cq, symbols in sq.
# The graph will have features (x_axis, y_axis, title), with legend
# determining whether a legend will be displayed.
ggplot2_2d <- function(x, y, color = NULL, shape = NULL,
                       color_seq = NULL, mode = NULL, legend = TRUE,
                       title = "", x_axis = "", y_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(shape) < 1)
    shape <- color

  if (length(color_seq) < 1)
    color_seq <- color_seq(length(unique(color)))

  df <- data.frame("x" = as.numeric(x), "y" = as.numeric(y),
                   "Color" = as.character(color), "Shape" = as.character(shape))

  shape_seq <- ggplot2_shape_seq(length(unique(shape)))

  theme <- theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 10),
    legend.position = ifelse(legend, "right", "none"),
    plot.margin = margin(0.5, 0, 0, 0, "cm"),
    panel.background = element_blank(),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "gray"))

  plot <- ggplot(df, aes(x = x, y = y, color = Color, shape = Shape)) +
    theme + ggtitle(title) + xlab(x_axis) + ylab(y_axis) +
    scale_color_manual(values = color_seq) +
    scale_shape_manual(values = shape_seq) +
    geom_hline(aes(yintercept = 0), size = 0.5) +
    geom_vline(aes(xintercept = 0), size = 0.5) +
    geom_point(size = 2.4) +
    guides(color = guide_legend(order = 1))

  if (is.null(mode))
    return(plot)
  if (mode == "log")
    return(plot + geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x))))
  return(plot + geom_line())
}

# Plots all data points at (x,y) ...
# ... with appropriate colors in colors in c_seq, text.
# The graph will have features (x_axis, y_axis, title), with legend
# determining whether a legend will be displayed.
plotly_2d <- function(x, y, color = NULL, text = NULL,
                      color_seq = NULL, lines = FALSE, legend = TRUE,
                      title = "", x_axis = "", y_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(text) < 1)
    text <- color

  if (length(color_seq) < 1)
    color_seq <- color_seq(length(unique(color)))

  plot <- plot_ly(x = as.numeric(x), y = as.numeric(y),
                  color = as.character(color), text = as.character(text),
                  colors = color_seq, mode = ifelse(lines, "lines+markers", "markers"),
                  marker = list(size = 6, symbol = 'circle'),
                  hovertemplate = paste(
                    "<b>%{text}</b>",
                    "<br>(%{x:.4f}, %{y:.4f})",
                    "<extra></extra>"), type="scatter")
  layout(plot,
         title = title,
         xaxis = list(title = x_axis),
         yaxis = list(title = y_axis),
         showlegend = legend)
}

# Plots all data points at (x,y,z) ...
# ... with appropriate colors in c_seq, text.
# The graph will have features (x_axis, y_axis, z_axis, title), with legend
# determining whether a legend will be displayed.
plotly_3d <- function(x, y, z, color = NULL, text = NULL,
                      color_seq = NULL, legend = TRUE,
                      title = "", x_axis = "", y_axis = "", z_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(text) < 1)
    text <- color

  if (length(color_seq) < 1)
    color_seq <- color_seq(length(unique(color)))

  plot <- plot_ly(x = as.numeric(x), y = as.numeric(y), z = as.numeric(z),
                  color = as.character(color), text = as.character(text),
                  colors = color_seq, mode = "markers",
                  marker = list(size = 4, symbol = 'circle'),
                  hovertemplate = paste(
                    "<b>%{text}</b>",
                    "<br>(%{x:.4f}, %{y:.4f}, %{z:.4f})",
                    "<extra></extra>"), type="scatter3d")
  layout(plot,
         title = title,
         scene = list(xaxis = list(title = x_axis),
                      yaxis = list(title = y_axis),
                      zaxis = list(title = z_axis)),
         showlegend = legend)
}

# -----------------------
# SET INTERSECTION GRAPHS
# -----------------------

# checks if data is a matrix or data frame
is_mat_df <- function(data)
{
  any(class(data) %in% c("matrix", "data.frame"))
}

# given a matrix and two ranges f1, f2,
# return the rows where the of number of entries between f1[1], f1[2]
# is between f2[1], f2[2] and omit empty columns
set_f1_f2 <- function(data, f1, f2)
{
  if (!is_mat_df(data) || length(data) < 1 || ncol(data) < 1)
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

# returns the first m rows of data unless m is too big
truncate_rows <- function(data, m = .Machine$double.xmax)
{
  if (m < nrow(data))
    return(data[1:m,,drop=FALSE])
  data
}

# sort the rows of data by their sums in decreasing order
sort_row_sums <- function(data)
{
  data[base::order(rowSums(data),decreasing=T),,drop=FALSE]
}

# Creates an UpSetR plot with nintersects columns, the provided height ratio of the
# bar plot to the whole upset, and whether feature subsets should be sorted by size.
upset_custom <- function(data, nintersects, ratio, keep_order, text_scale = 1,
                         solid="royalblue4", shade="lightskyblue")
{
  if (ncol(data) < 2 || nrow(data) < 8)
    return(NULL)

  colnames(data) <- gsub(" ", "_", colnames(data))

  upset(data, sets = rev(colnames(data)), nintersects = nintersects,
        sets.x.label = "Features Per Factor Level",
        mainbar.y.label = "Features Per Factor Subset",
        order.by = "freq", mb.ratio = c(ratio, 1-ratio), keep.order=keep_order,
        line.size = 0.1, shade.alpha = 0.25, text.scale = text_scale,
        matrix.color = solid, main.bar.color = solid,
        sets.bar.color = solid, shade.color = shade)
}

# Draws a single set venn diagram, where data is a one-column matrix / data frame
venn1_custom <- function(data, legend = TRUE)
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
plotly_heatmap_variance <- function(binary, colors = NULL,
                                    title = "", legend = TRUE, smooth = FALSE)
{
  if (length(colors) < 1)
    colors <- color_seq(5, "Inferno")

  rows <- substring(rownames(binary), 0, 50)
  cols <- substring(colnames(binary), 0, 50)
  rownames(binary) <- NULL
  colnames(binary) <- NULL

  plot <- plot_ly(x = rows, y = cols, z = t(binary),
                  type="heatmap",
                  zsmooth = ifelse(smooth, "best", "false"),
                  colors=colors)
  layout(plot,
         title = title,
         showlegend = legend)
}

# creates a correlation-based heatmap for sets on plotly
plotly_heatmap_dendrogram <- function(binary, colors = NULL,
                                      title = "", legend = TRUE, dend = TRUE)
{
  if (length(colors) < 1)
    colors <- color_seq(5, "Inferno")

  if (nrow(binary) < 1 || ncol(binary) < 1)
    return(NULL)

  binary[is.na(binary)] <- 0
  heatmaply(
    t(binary),
    main = title,
    hide_colorbar = !legend,
    colors = colors,
    plot_method = "plotly",
    colorbar_len = ifelse(dend, 0.8, 1),
    showticklabels = c(FALSE, TRUE),
    dendrogram = ifelse(dend, "both", "none"))
}

# ----------
# DATATABLES
# ----------

can_be_numeric <- function(vec)
{
  sum(is.na(suppressWarnings(as.character(vec)))) == sum(is.na(suppressWarnings(as.numeric(vec))))
}

# Creates a datatable from a data frame
my_datatable <- function(df = empty_df)
{
  if (class(df) != "data.frame" || ncol(df) < 1)
    df <- empty_df

  for (col in colnames(df))
  {
    if (!can_be_numeric(df[[col]]))
    {
      if (length(unique(df[[col]])) <= num_filters)
        df[[col]] <- as.factor(df[[col]])
      else
        df[[col]] <- as.character(df[[col]])
    }
  }

  datatable(
    df, editable = FALSE, escape = TRUE, filter = "top", selection = "none",
    options = list(scrollX = TRUE, scrollY = TRUE, autoWidth = FALSE)
  )
}

# -----------
# SPECIALIZED
# -----------

ggplot2_pca_sum <- function(data, pc_cap, legend = TRUE, title = "")
{
  pca_var_metadata <- rep("Cumulative Variance", pc_cap)
  ggplot2_2d(
    data[,"Components"], data[,"Variance"],
    pca_var_metadata, pca_var_metadata,
    single_color_seq, "log", legend,
    title, "Number of Components", "Variance Captured"
  )
}

plotly_pca_sum <- function(data, pc_cap, lines = TRUE, legend = TRUE, title = "")
{
  pca_var_metadata <- rep("Cumulative Variance", pc_cap)
  plotly_2d(
    data[,"Components"], data[,"Variance"],
    pca_var_metadata, sprintf("%s: %s", "Variance", pca_var_metadata),
    single_color_seq, lines, legend,
    title, "Number of Components", "Variance Captured"
  )
}

ggplot2_vae_sum <- function(data, reverse = FALSE, legend = TRUE, title = "")
{
  ggplot2_2d(
    data[,"Training Iterations"], data[,"Loss Value"],
    data[,"Loss Type"], data[,"Loss Type"],
    double_color_seq(reverse), "log", legend,
    title, "Number of Training Iterations", "Loss Function Output"
  )
}

plotly_vae_sum <- function(data, lines = TRUE, reverse = FALSE, legend = TRUE, title = "")
{
  plotly_2d(
    data[,"Training Iterations"], data[,"Loss Value"],
    data[,"Loss Type"], sprintf("%s: %s", "Loss Type", data[,"Loss Type"]),
    double_color_seq(reverse), lines, legend,
    title, "Number of Training Iterations", "Loss Function Output"
  )
}

# makes a matrix that summarizes nearest neighbors for heatmap use
knn_label_matrix <- function(knn_indices, labels)
{
  # validate knn_indices is a matrix of indices
  stopifnot(all.equal(class(matrix()), class(knn_indices)))
  stopifnot(sum(knn_indices > 0) == nrow(knn_indices) * ncol(knn_indices))

  n <- nrow(knn_indices)
  k <- ncol(knn_indices)

  uni_labels <- unique(labels)
  m <- length(uni_labels)
  heatmat <- matrix(0, nrow = m, ncol = m)
  rownames(heatmat) <- uni_labels
  colnames(heatmat) <- uni_labels

  for (i in 1:n)
  {
    point_type <- labels[i]
    for (j in 1:k)
    {
      nn_type <- labels[knn_indices[i,j]]
      heatmat[point_type, nn_type] <- heatmat[point_type, nn_type] + 1
    }
  }

  for (i in 1:m)
  {
    heatmat[i,] <- heatmat[i,] / sum(heatmat[i,])
  }

  heatmat
}

first_over_total <- function(row){row[1] / sum(row)}

ggplot2_umap_sum <- function(label_mat, color_seq = NULL, title = "")
{
  if (length(color_seq) < 1)
    color_seq <- color_seq(nrow(label_mat))

  res <- data.frame("Sample_Group" = rownames(label_mat),
                    "Self_Neighbors" = apply(label_mat, 1, first_over_total))
  ggplot(res, aes(y = Self_Neighbors, x = Sample_Group, fill = Sample_Group)) +
    geom_bar(stat = "identity") + coord_flip() +
    scale_fill_manual(values = color_seq) + ggtitle(title) + theme(
      plot.title = element_text(size = 22, face = "bold"),
      axis.title.x = element_text(size = 16, margin = margin(t = 10)),
      axis.title.y = element_text(size = 16, margin = margin(r = 10)),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
      panel.background = element_blank(),
      panel.grid.major = element_line(size = 0.1, linetype = 'solid', colour = "gray"))
}

plotly_umap_sum <- function(label_mat, paint, title = "", legend = TRUE, three = TRUE, boost = FALSE)
{
  if (three)
    return(plotly_heatmap_dendrogram(
      label_mat, paint, title = title, legend = legend, dend = boost
    ))
  plotly_heatmap_variance(
    label_mat, paint, title = title, legend = legend, smooth = boost
  )
}