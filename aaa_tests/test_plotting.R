# This file tests app_functions.R.

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("plotting.R", encoding="UTF-8")
source("installer.R", encoding="UTF-8")

# displays a color sequence as a rectangular palette
display_palette <- function(color_seq, title)
{
  nums <- 1:length(color_seq)
  image(nums, 1, as.matrix(nums), col=color_seq, axes=FALSE, xlab=title, ylab="")
}

# -----
# TESTS
# -----

test_color_seq <- function()
{
  print_clean("Functions tested: display_palette, color_seq")
  display_palette(single_color_seq, "Yale Blue")
  display_palette(double_color_seq(), "Yale Blue + Harvard Crimson")

  for (color in c("Rainbow", "Heat", "Terrain", "Topography", "CM",
                  "Viridis", "Magma", "Plasma", "Inferno", "Cividis"))
  {
    display_palette(
      color_seq(1000, color_type=color, reverse=FALSE), sprintf("%s Normal", color))
    display_palette(
      color_seq(1000, color_type=color, reverse=TRUE), sprintf("%s Reverse", color))
  }

  display_palette("#000000", "End of Presentation")
}

test_ggplot2_null <- function()
{
  print_clean("Functions tested: ggplot2_null")
  ggplot2_null()
}

test_boxplot_beeswarm <- function(r10 = 100)
{
  print_clean("Functions tested: boxplot_beeswarm")
  data <- data.frame(matrix(nrow = 10*r10, ncol=2))
  data[,1] <- sprintf("c%s", rep(1:10, r10))
  data[,2] <- sample(seq(0, 1, by=0.01), 10*r10, replace=TRUE)^2
  colnames(data) <- c("X_coord", "Y_coord")
  cs <- color_seq(10)

  boxplot_beeswarm(data, cs, "Test with Embedded Legend", TRUE)
  boxplot_beeswarm(data, cs, "Test with External Legend", FALSE)
}

test_ggplot2_2d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  print_clean("Functions tested: ggplot2_2d")
  ggplot2_2d(1:(10*n), 1:(10*n), types, types, color_seq(10))
}

test_plotly_2d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  print_clean("Functions tested: plotly_2d")
  plotly_2d(1:(10*n), 1:(10*n), types, types, color_seq(10))
}

test_plotly_3d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  print_clean("Functions tested: plotly_2d")
  plotly_3d(1:(10*n), 1:(10*n), 1:(10*n), types, types, color_seq(10))
}

test_upset_custom <- function(row=10000, col=4)
{
  print_clean("Functions tested: upset_custom")

  # example data
  data <- data.frame(matrix(sample(0:1, row*col, replace=TRUE), nrow=row, ncol=col))
  rownames(data) <- sprintf("Row_%s", 1:row)
  colnames(data) <- sprintf("Col_%s", 1:col)
  assign("upset_custom_data", data, envir=.GlobalEnv)

  # compressed with number of occurrences
  comp <- data[!duplicated(data), ]
  occurrences <- rep(0, nrow(comp))

  for (i in 1:nrow(comp))
  {
    ref <- as.numeric(comp[i, ])
    for (j in 1:nrow(data))
    {
      if (isTRUE(all.equal(ref, as.numeric(data[j, ]))))
        occurrences[i] <- occurrences[i]+1
    }
  }

  comp[["Occurrences"]] <- occurrences
  comp <- comp[order(rowSums(comp[,1:col])),]
  View(comp)
}

# -------
# RUN ALL
# -------
test_color_seq()
test_ggplot2_null()
test_boxplot_beeswarm()
test_ggplot2_2d()
test_plotly_2d()
test_plotly_3d()
test_upset_custom()
upset_custom(upset_custom_data, 4, 0.7, TRUE)
upset_custom(upset_custom_data, 8, 0.5, TRUE)
upset_custom(upset_custom_data, 16, 0.3, TRUE)
