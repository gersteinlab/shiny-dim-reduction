# The purpose of this file is to store libraries and functions
# for server-side logic. None of these should depend on global variables!
# Each section is sorted from most algorithmic to least algorithmic,
# though intended purpose may create a more reasonable ordering.

if (!exists("sdr_config"))
  source("app/install.R")

library(viridis)
library(ggplot2)
library(plotly)
library(UpSetR)
library(VennDiagram)
library(beeswarm)
library(heatmaply)
library(DT)

# ---------------
# COLOR SEQUENCES
# ---------------

# the default color sequence (color_seq) for this project
sdr_color_seq <- c("#00356B", "#C90016")

# lists all color sequence options
color_seq_types <- list(
  "Custom" = c("Custom", "Grayscale", "None"),
  "Base" = c("Rainbow", "Heat", "Terrain", "Topography", "CM"),
  "Viridis" = c("Viridis", "Magma", "Plasma", "Inferno", "Cividis")
)

#' sets color_seq transparency to alpha = 0.25
#'
#' @param color_seq [color_seq] not checked
#' @returns [color_seq]
make_transparent <- function(color_seq)
{
  sprintf("%s40", substr(color_seq, start = 1, stop = 7))
}

#' cycles a color_seq forward
#'
#' @param color_seq [color_seq] not checked
#' @param i [integer] not checked
#' @returns [color_seq]
cycle_color_seq <- function(color_seq, i = 1L)
{
  n <- length(color_seq)
  i_seq <- seq_len(n) + (i %% n)
  rep(color_seq, 2)[i_seq]
}

#' reverses a color_seq
#'
#' @param color_seq [color_seq] not checked
#' @param reverse [boolean] not checked
#' @returns [color_seq]
rev_color_seq <- function(color_seq, reverse = TRUE)
{
  if (!reverse)
    return(color_seq)
  rev(color_seq)
}

#' alternates a color_seq
#'
#' @param color_seq [color_seq] not checked
#' @param alternate [boolean] not checked
#' @returns [color_seq]
alt_color_seq <- function(color_seq, alternate = TRUE)
{
  if (!alternate)
    return(color_seq)
  n <- length(color_seq)
  cycle_seq <- cycle_color_seq(color_seq, median_index(n))
  rbind(color_seq, cycle_seq)[seq_len(n)]
}

#' Generates a color_seq of length n and type color_type
#'
#' @param [int]
#' @param color_type [string]
#' @returns [color_seq]
make_color_seq <- function(n, color_type = "Rainbow")
{
  stopifnot(is_int(n), is_str(color_type))
  if (color_type == "None")
    return(rep("#000000", n))
  if (n < 3L)
    return(rep(sdr_color_seq, length.out = n))

  if (color_type == "Grayscale")
    return(grey.colors(n))
  if (color_type == "Heat")
    return(heat.colors(n))
  if (color_type == "Terrain")
    return(terrain.colors(n))
  if (color_type == "Topography")
    return(topo.colors(n))
  if (color_type == "CM")
    return(cm.colors(n))
  if (color_type == "Viridis")
    return(viridis::viridis(n))
  if (color_type == "Magma")
    return(viridis::magma(n))
  if (color_type == "Plasma")
    return(plasma(n))
  if (color_type == "Inferno")
    return(inferno(n))
  if (color_type == "Cividis")
    return(cividis(n))
  if (color_type == "Rainbow")
    return(hcl(360 * 1:n / (n + 1) - 15, 160, 60))
  stop_f("unsupported color_type: %s", color_type)
}

# ------------------
# BOXPLOT + BEESWARM
# ------------------

#' boxplot + beeswarm with partially transparent boxplot
#'
#' @param data [data.frame] with two
#'    columns (labels / values), not checked
#' @param color_seq [color_seq, NULL] not checked
#' @param title [string] not checked
#' @param legend [boolean] not checked
#' @returns [object] plot
boxplot_beeswarm <- function(data, color_seq = NULL, title = "", legend = TRUE)
{
  xlab <- colnames(data)[1]
  ylab <- colnames(data)[2]
  rel <- get(ylab) ~ get(xlab)
  x_bins <- unique(data[, 1])

  if (is.null(color_seq))
    color_seq <- make_color_seq(length(x_bins))

  if (!legend)
    x_bins <- seq_along(x_bins)

  graphics::boxplot(
    rel, data = data, xlab = xlab, ylab = ylab, names = x_bins,
    col = make_transparent(color_seq), outline = FALSE, main = title
  ) # transparency

  beeswarm::beeswarm(
    rel, data = data, xlab = xlab, ylab = ylab, labels = x_bins,
    col = color_seq, corral = "random", main = title, pch = 16, add = TRUE
  ) # filled circles
}

# -------------------
# SCATTER PLOT GRAPHS
# -------------------

# ggplot2 common theme
ggplot2_theme <- theme(
  plot.title = element_text(size = 22, face = "bold"),
  axis.title.x = element_text(size = 16, margin = margin(t = 10)),
  axis.title.y = element_text(size = 16, margin = margin(r = 10)),
  axis.text = element_text(size = 12),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 10),
  plot.margin = margin(0.5, 0, 0, 0, "cm"),
  panel.background = element_blank(),
  panel.grid.major = element_line(
    size = 0.1, linetype = 'solid', colour = "gray")
)

# a default plot accepted by ggplot2 and plotly
default_plot <- ggplot(data.frame()) + ggplot2_theme +
  geom_point() + ggtitle("This plot cannot be displayed.") +
  xlim(0, 1) + ylim(0, 1)

#' Generates a shape sequence of length n for ggplot2
#'
#' @param n [int] not checked
#' @returns [integer]
ggplot2_shape_seq <- function(n)
{
  # my favorite ordering of the 26 default R plot shapes
  c(15L:25L, 0L, 1L, 2L, 5L, 6L, 3L, 4L, 8L, 7L, 9L:14L) %>%
    rep(length.out = n)
}

#' Plots 2D data points with colors, shapes, axes, title, and legend.
#'
#' @param x [numeric]
#' @param y [numeric]
#' @param color [character, NULL]
#' @param shape [character, NULL]
#' @param color_seq [color_seq, NULL] not checked
#' @param legend [boolean] whether a legend is displayed
#' @param title [string] not checked
#' @param x_axis [string] not checked
#' @param y_axis [string] not checked
#' @returns [object] plot
ggplot2_2d <- function(x, y, color = NULL, shape = NULL,
                       color_seq = NULL, legend = TRUE,
                       title = "", x_axis = "", y_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(shape) < 1)
    shape <- color

  if (length(color_seq) < 1)
    color_seq <- make_color_seq(num_unique(color))

  df <- data.frame("xx" = x, "yy" = y, "Color" = color, "Shape" = shape)
  shape_seq <- num_unique(shape) %>% ggplot2_shape_seq()
  theme <- ggplot2_theme
  theme[["legend.position"]] <- ifelse(legend, "right", "none")

  ggplot(df, aes(x = xx, y = yy, color = Color, shape = Shape)) +
    theme + ggtitle(title) + xlab(x_axis) + ylab(y_axis) +
    scale_color_manual(values = color_seq) +
    scale_shape_manual(values = shape_seq) +
    geom_hline(aes(yintercept = 0), size = 0.5) +
    geom_vline(aes(xintercept = 0), size = 0.5) +
    geom_point(size = 2.4) +
    guides(color = guide_legend(order = 1))
}

#' Plots 2D data points with colors, text, axes, title, and legend.
#'
#' @param x [numeric]
#' @param y [numeric]
#' @param color [character, NULL]
#' @param text [character, NULL]
#' @param color_seq [color_seq, NULL] not checked
#' @param lines [boolean] whether to connect points by lines
#' @param legend [boolean] whether a legend is displayed
#' @param title [string] not checked
#' @param x_axis [string] not checked
#' @param y_axis [string] not checked
#' @returns [object] plot
plotly_2d <- function(x, y, color = NULL, text = NULL,
                      color_seq = NULL, lines = FALSE, legend = TRUE,
                      title = "", x_axis = "", y_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(text) < 1)
    text <- color

  if (length(color_seq) < 1)
    color_seq <- make_color_seq(num_unique(color))

  ht_text <- "<b>%{text}</b><br>(%{x:.4f}, %{y:.4f})<extra></extra>"
  plotly::plot_ly(
    x = x, y = y, color = color, text = text, colors = color_seq,
    mode = ifelse(lines, "lines+markers", "markers"),
    marker = list(size = 6, symbol = 'circle'),
    hovertemplate = ht_text, type = "scatter"
  ) %>% plotly::layout(
    title = title,
    xaxis = list("title" = x_axis),
    yaxis = list("title" = y_axis),
    showlegend = legend
  )
}

#' Plots 3D data points with colors, text, axes, title, and legend.
#'
#' @param x [numeric]
#' @param y [numeric]
#' @param z [numeric]
#' @param color [character, NULL]
#' @param text [character, NULL]
#' @param color_seq [color_seq, NULL] not checked
#' @param legend [boolean] whether a legend is displayed
#' @param title [string] not checked
#' @param x_axis [string] not checked
#' @param y_axis [string] not checked
#' @param z_axis [string] not checked
#' @returns [object] plot
plotly_3d <- function(x, y, z, color = NULL, text = NULL,
                      color_seq = NULL, legend = TRUE,
                      title = "", x_axis = "", y_axis = "", z_axis = "")
{
  if (length(color) < 1)
    color <- rep("Unknown", length(x))

  if (length(text) < 1)
    text <- color

  if (length(color_seq) < 1)
    color_seq <- make_color_seq(num_unique(color))

  ht_text <- "<b>%{text}</b><br>(%{x:.4f}, %{y:.4f}, %{z:.4f})<extra></extra>"
  plotly::plot_ly(
    x = x, y = y, z = z, color = color, text = text, colors = color_seq,
    mode = "markers",
    marker = list(size = 4, symbol = 'circle'),
    hovertemplate = ht_text,
    type = "scatter3d"
  ) %>% plotly::layout(
    title = title,
    scene = list(
      "xaxis" = list("title" = x_axis),
      "yaxis" = list("title" = y_axis),
      "zaxis" = list("title" = z_axis)
    ),
    showlegend = legend
  )
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
  nan_map <- is.nan(data)
  data[!nan_map] <- 1
  data[nan_map] <- 0
  data.frame(data)
}

# returns the first m rows of data unless m is too big
truncate_rows <- function(data, m = .Machine$double.xmax)
{
  if (m >= nrow(data))
    return(data)
  data[seq_len(m), , drop = FALSE]
}

# sort the rows of data by their sums in decreasing order
sort_row_sums <- function(data)
{
  data[base::order(rowSums(data), decreasing = TRUE), , drop = FALSE]
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

#' Draws a Venn diagram with one circle
#'
#' @param area [int] not checked
#' @param category [string] the name of the circle
#' @returns [object] plot
venn1_custom <- function(area, category = "")
{
  VennDiagram::draw.single.venn(
    area, category = category,
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
    colors <- make_color_seq(5L, "Inferno")

  rows <- substring(rownames(binary), 0, 50)
  cols <- substring(colnames(binary), 0, 50)
  rownames(binary) <- NULL
  colnames(binary) <- NULL

  plot_ly(
    x = rows, y = cols, z = t(binary),
    type = "heatmap",
    zsmooth = ifelse(smooth, "best", "false"),
    colors = colors
  ) %>% layout(
    title = title,
    showlegend = legend
  )
}

# creates a correlation-based heatmap for sets on plotly
plotly_heatmap_dendrogram <- function(binary, colors = NULL,
                                      title = "", legend = TRUE, dend = TRUE)
{
  if (length(colors) < 1)
    colors <- make_color_seq(5L, "Inferno")

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
  sum(is.na(suppressWarnings(as.character(vec)))) ==
    sum(is.na(suppressWarnings(as.numeric(vec))))
}

empty_df <- data.frame("Unknown" = numeric())

# Creates a datatable from a data frame
my_datatable <- function(df = empty_df, levels_nmax = 60)
{
  if (class(df) != "data.frame" || ncol(df) < 1)
    df <- empty_df

  for (col in colnames(df))
  {
    if (!can_be_numeric(df[[col]]))
    {
      if (num_unique(df[[col]]) <= levels_nmax)
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
    data[, "Components"], data[, "Variance"],
    pca_var_metadata, pca_var_metadata,
    sdr_color_seq[1], legend, title,
    "Number of Components", "Variance Captured"
  ) + geom_line()
}

plotly_pca_sum <- function(data, pc_cap, lines = TRUE, legend = TRUE, title = "")
{
  pca_var_metadata <- rep("Cumulative Variance", pc_cap)
  plotly_2d(
    data[,"Components"], data[,"Variance"],
    pca_var_metadata, sprintf("Variance: %s", pca_var_metadata),
    sdr_color_seq[1], lines, legend,
    title, "Number of Components", "Variance Captured"
  )
}

ggplot2_vae_sum <- function(data, reverse = FALSE, legend = TRUE, title = "")
{
  ggplot2_2d(
    data[,"Training Iterations"], data[,"Loss Value"],
    data[,"Loss Type"], data[,"Loss Type"],
    rev_color_seq(sdr_color_seq, reverse), legend, title,
    "Number of Training Iterations", "Loss Function Output"
  ) + geom_smooth(
    se = FALSE, method = "gam", formula = y ~ s(log(x))
  )
}

plotly_vae_sum <- function(data, lines = TRUE, reverse = FALSE, legend = TRUE, title = "")
{
  plotly_2d(
    data[,"Training Iterations"], data[,"Loss Value"],
    data[,"Loss Type"], sprintf("%s: %s", "Loss Type", data[,"Loss Type"]),
    rev_color_seq(sdr_color_seq, reverse), lines, legend,
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
    color_seq <- make_color_seq(nrow(label_mat))

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

cat_f("PLOTTING SETUP TIME: %.1f (sec)\n", net_time())
