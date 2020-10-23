# The purpose of this file is to store libraries and functions
# for the main app. None of these should depend on global variables!
# Each section is sorted from most algorithmic to least algorithmic,
# though intended purpose may create a more reasonable ordering.

require("shinydashboard")
require("shinyjs")
require("shinycssloaders")
require("shinyWidgets")
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

# Converts a matrix into a binary matrix: 1 if lower <= x <= upper, 0 otherwise.
frac_convert <- function(data, lower, upper)
{
  for (j in 1:ncol(data))
    data[,j] <- between(data[,j], lower, upper)
  storage.mode(data) <- "numeric"
  data[rowSums(data) > 0, colSums(data) > 0, drop = FALSE]
}

# filters a binary matrix by the number of entries in each column
rowSum_filter_bin <- function(data, lower, upper)
{
  lower <- ceiling(lower*ncol(data))
  upper <- floor(upper*ncol(data))
  data[between(rowSums(data), lower, upper), , drop = FALSE]
}

# bounds a matrix for Sets heatmap
frac_bound <- function(data, lower, upper)
{
  for (j in 1:ncol(data))
    data[,j] <- ifelse(between(data[,j], lower, upper), data[,j], NaN)
  data[rowSums(!is.nan(data)) > 0, colSums(!is.nan(data)) > 0, drop = FALSE]
}

# filters a binary matrix by the number of non-NaN entries in each column
rowSum_filter_dat <- function(data, lower, upper)
{
  lower <- ceiling(lower*ncol(data))
  upper <- floor(upper*ncol(data))
  data[between(rowSums(!is.nan(data)), lower, upper), , drop = FALSE]
}

# Gets the option set for a group of samples
get_opt <- function(samples) {
  lapply(unique(samples), function(x){make_opt(x, sum(x == samples))})
}

# Formats an option for app selection
make_opt <- function(a,b) {
  sprintf("%s (%s)", a, b)
}

# Suppose a checkbox created by get_opt() returns the string "Sputum (100)". 
# This function parses that string to "Sputum".
parse_opt <- function(checkbox)
{
  if (is.null(checkbox)) 
    return(NULL)
  checkbox %>% strsplit(" \\(") %>% lapply(function(i){i[1]}) %>% unlist()
}

# calculates the number of truncated features given pc_cap, 
# the inputted feature fraction (feat), and the total number of features.
calc_feat <- function(pc_cap, feat, total)
{
  pc_cap + ceiling(feat * (total - pc_cap))
}

# retrieves a subset
get_my_subset <- function(decor, cat, sub)
{
  for (dec_group in decor)
  {
    if (cat %in% dec_group$Categories)
    {
      ref <- dec_group$Subsets$Reference
      ind <- dec_group$Subsets[[sub]]
      return(ref[ind])
    }
  }
  
  return(NULL)
}

# Useful function for changing "PC 1" to "prcomp 1", getting length, etc.
pc <- function(name) 
{
  if (missing(name))
    name <- "1"
  sprintf("Component %s", name)
}

# sorts a list by names
sort_by_names <- function(target)
{
  target[base::order(names(target))]
}

# --------------
# INPUT ENCODING
# --------------

# Useful for finding select IDs
get_select <- function(category, character) 
{
  sprintf("selectby_%s_%s", category, character)
}

# Useful for finding thre IDs
get_thre <- function(category, scale) 
{
  sprintf("thre_%s_%s", category, scale)
}

# To make short bookmarks, we must use URL-safe characters.
# These characters are:
# safe_URL_separators <- ".-_~"
# safe_URL_numbers <- "0123456789"
# safe_URL_LETTERS <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
# safe_URL_letters <- "abcdefghijklmnopqrstuvwxyz"

# Bookmarking:
# i) A single variable "data" should completely capture the state of the app
# ii) data should be encoded and presented in onBookmark
# iii) data should be decoded and used to update input widgets in onRestored
# iv) all variables should be converted into (arrays of) numerics, which should be
#     collapsed into a vector of strings and then converted into one string.
# v) The following separators should be used, from highest level to lowest level.
sep_chars <- c("~", "_", "-")

# chars <- sample(sample(c(as.character(0:9), LETTERS[1:22])))
# reserved: lowercase letters, W, X, Y, Z
chars <- c("6","J","F","K","2","R","M","O",
           "5","A","9","U","7","0","B","1",
           "V","T","D","P","8","H","S","I",
           "4","3","Q","L","N","G","E","C")

# converts an array of indices into a base 32 string
indices_fifstr <- function(indices)
{
  if (length(indices) < 1)
    return("*")
  fif <- rep(0, length=ceiling(max(indices)/5))
  for (i in indices)
  {
    fifi <- floor((i-1)/5)+1
    modulo <- (i-1) %% 5
    fif[fifi] <- fif[fifi] + 2^modulo
  }
  
  cipher <- fif
  
  for (i in 1:length(fif))
    cipher[i] <- chars[fif[i]+1]
  
  paste(rev(cipher), collapse = '')
}

# converts a base 32 string into an array of indices
fifstr_indices <- function(fifstr)
{
  if (fifstr == "*")
    return(integer(0))
  
  cipher <- rev(strsplit(fifstr, ""))[[1]]
  fif <- rep(0, length(cipher))
  for (i in 1:length(cipher))
    fif[i] <- which(chars == cipher[i])-1
  
  bin <- rep(0, 5*length(fif))
  for (i in 1:length(fif))
  {
    temp <- fif[i]
    bin[5*i-4] <- floor(temp / 16)
    temp <- temp %% 16
    bin[5*i-3] <- floor(temp / 8)
    temp <- temp %% 8
    bin[5*i-2] <- floor(temp / 4)
    temp <- temp %% 4
    bin[5*i-1] <- floor(temp / 2)
    temp <- temp %% 2
    bin[5*i] <- floor(temp)
  }
  length(bin)-which(bin > 0)+1
}

# encodes a vector of numbers (length < 2 ok)
encode_num <- function(data)
{
  if (length(data) < 1 || class(data) != "numeric")
    return(0)
  paste(data*10000, collapse=sep_chars[3])
}

# decodes a vector of numbers (length < 2 ok)
decode_num <- function(data)
{
  strsplit(data, sep_chars[3])[[1]] %>% as.numeric() / 10000
}

# encodes a list of lists into a string with two separators
encode_lol <- function(lol)
{
  for (item in names(lol))
    lol[[item]] <- paste(unlist(lol[[item]]), collapse=sep_chars[3])
  
  paste(unlist(lol), collapse=sep_chars[2])
}

# decodes a list of lists from a string with two separators
decode_lol <- function(list_string, outline)
{
  lol <- as.list(strsplit(list_string, sep_chars[2])[[1]])
  names(lol) <- names(outline)
  
  for (item in names(lol))
  {
    inner <- as.list(strsplit(lol[[item]], sep_chars[3])[[1]])
    names(inner) <- outline[[item]]
    lol[[item]] <- inner
  }
  
  lol
}

# creates a vector of inputs that should be excluded 
# from bookmarking, based on the table's ID
table_exclude_vector <- function(table_id)
{
  c(
    sprintf("%s_search", table_id),
    sprintf("%s_state", table_id),
    sprintf("%s_cell_clicked", table_id),
    sprintf("%s_search_columns", table_id),
    sprintf("%s_rows_current", table_id),
    sprintf("%s_rows_all", table_id),
    sprintf("%s_rows_selected", table_id),
    sprintf("%s_columns_selected", table_id),
    sprintf("%s_cells_selected", table_id)
  )
}

# ----------------------
# GENERAL GRAPHS / TOOLS
# ----------------------

# Generates a color sequence of length n_colors with the given type.
# "Base"=c("Rainbow", "Heat", "Terrain", "Topography", "CM"),
# "Viridis"=c("Viridis", "Magma", "Plasma", "Inferno", "Cividis")
color_seq <- function(n_colors, color_type, keep) 
{
  rev_opt <- !keep
  
  if (n_colors < 2)
    return("#00356B")
  
  if (color_type == "Rainbow" || color_type == "Custom")
  {
    if (rev_opt)
      return(hcl(n_colors:1 * (360/(n_colors+1))-15, 160, 60))
    else
      return(hcl(1:n_colors * (360/(n_colors+1))-15, 160, 60))
  }
  if (color_type == "Heat")
    return(heat.colors(n_colors, rev=rev_opt))
  if (color_type == "Terrain")
    return(terrain.colors(n_colors, rev=rev_opt))
  if (color_type == "Topography")
    return(topo.colors(n_colors, rev=rev_opt))
  if (color_type == "CM")
    return(cm.colors(n_colors, rev=rev_opt))
  if (color_type == "Viridis")
    return(viridis::viridis(n_colors, direction = ifelse(rev_opt, -1, 1)))
  if (color_type == "Magma")
    return(viridis::magma(n_colors, direction = ifelse(rev_opt, -1, 1)))
  if (color_type == "Plasma")
    return(plasma(n_colors, direction = ifelse(rev_opt, -1, 1)))
  if (color_type == "Inferno")
    return(inferno(n_colors, direction = ifelse(rev_opt, -1, 1)))
  if (color_type == "Cividis")
    return(cividis(n_colors, direction = ifelse(rev_opt, -1, 1)))
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
                             box_colors, bee_colors)
{
  boxplot(rel, data=data, xlab=xlab, ylab=ylab,
          names=names, col=box_colors,
          outline = FALSE, main='boxplot + beeswarm')
  
  beeswarm(rel, data=data, xlab=xlab, ylab=ylab,
           labels=names, col=bee_colors, corral="random",
           main= 'beeswarm + bxplot', pch=16, add=TRUE) # filled circles
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
upset_custom <- function(data, legend, ordering) 
{
  upset(data, nsets = ncol(data), nintersects = 50, 
        sets.x.label = "Features Per Factor Level", 
        mainbar.y.label = "Features Per Factor Subset", 
        show.numbers = ifelse(legend, "yes", "no"),
        order.by = ordering, decreasing = T, mb.ratio = c(0.7, 0.3), text.scale = 1,
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

# Draws a two set venn diagram, where data is a two-column matrix/data frame
# Note: there is a glitch with this package ... only the first title is displayed
venn2_custom <- function(data, legend) 
{
  draw.pairwise.venn(
    sum(data[,1]), sum(data[,2]), sum(data[,1]*data[,2]), 
    category = ifelse(legend, colnames(data), rep("", 2)),
    lwd = rep(2, 2), lty = rep("solid", 2), cex = rep(1, 3),  
    fill = c("#0064C8", "#C9788E"), alpha = rep(0.5, 2), 
    
    euler.d = TRUE, scaled = TRUE, inverted = FALSE, ext.text = FALSE, 
    sep.dist = 0.1, offset = 0, 
    
    # unsure at the moment
    cex.prop = NULL, print.mode = "raw", sigdigs = 3, 
    
    ind = TRUE, margin = 0.025, cat.just = rep(list(c(0.5, 0.5)), 2), 
    cat.default.pos = "outer", cat.prompts = FALSE, 
    cat.pos = c(0, 180), cat.dist = rep(0.025, 2), cat.cex = rep(1, 2), 
    rotation.degree = 0, rotation.centre = c(0.5, 0.5),
    col = rep("black", 2), label.col = rep("black", 3), cat.col = rep("black", 2),
    fontface = rep("plain", 3), fontfamily = rep("serif", 3), 
    cat.fontface = rep("plain", 2), cat.fontfamily = rep("serif", 2))
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

# ------------
# USER WIDGETS
# ------------

# Return the UI for a modal dialog that attempts to authenticate the user
authenticator_modal <- function() {
  modalDialog(
    title = HTML("<b>Authentication</b>"),
    HTML("Need access? Please make a request to 
    <a href=\"justin.chang@yale.edu\" target=\"_blank\">
    justin.chang@yale.edu</a>.<br><br>"),
    wellPanel(
      style="background-color: #E0F0FF; border-color: #00356B",
      textInput("username", "Username", 
                placeholder="Please enter your username ...", value="guest"),
      textInput("password", "Password (is invisible)", 
                placeholder="", value=""),
      action("attempt_login", "Login", "unlock", "#FFFFFF", "#0064C8", "#00356B"),
      actionButton("toggle_password", "Show/Hide Password")
    ),
    footer = tagList()
  )
}

# adds a spinner to content that may need to be refreshed
my_spin <- function(content)
{
  content %>% withSpinner(type = 6)
}

# shows a notification (form can be default, message, warning, error)
# in general: warnings and errors are self-explanatory, defaults are used
# to begin actions, and messages are used to return results
notif <- function(message, time, form) 
{
  showNotification(HTML(message), duration = time, closeButton = TRUE, type=form)
}

# prints a message once a plot begins generating.
plot_start <- function(numPlots)
{
  notif(sprintf("Generating Plot #%s:<br>
Please suspend plotting or wait for plotting to
finish before attempting a new configuration.", numPlots), 4, "default")
}

# prints a success message once a plot has been completed.
# note: start is the time when plotting begins, which can be found with Sys.time().
plot_success <- function(delta_time) 
{
  notif(sprintf("Plot generation was successful.<br>
Seconds elapsed: %s", delta_time), 6, "message")
}

# prints a message that indicates a truncated matrix
truncated_msg <- function()
{
  notif("Warning: This matrix is too large to plot.<br>
         A truncated version will be presented.", 6, "warning")
}

# prints a failure message once a plot has been completed.
plot_fail <- function() 
{
  notif("Plot generation failed.<br>
Possible reasons:<br>
(1) invalid configuration<br>
(2) empty dataset", 6, "error")
}