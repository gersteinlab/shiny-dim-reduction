# This file tests plotting.R.

# -----
# SETUP
# -----

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("plotting.R")

library(dplyr)

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

get_set_data <- function(row=10000, col=4)
{
  test <- seq(0, 1, 0.1)
  data <- matrix(sample(test, row*col, replace=TRUE), nrow=row, ncol=col)
  rownames(data) <- sprintf("Row_%s", 1:row)
  colnames(data) <- sprintf("Col_%s", 1:col)
  data
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

upset_custom_data <- get_set_data()

upset_custom_data %>% truncate_rows(100) %>% set_f1_f2(c(0.3, 1), c(0, 3)) %>%
  num_nan_binary() %>% upset_custom(4, 0.7, TRUE)

upset_custom_data %>% truncate_rows(1000) %>% set_f1_f2(c(0.4, 1), c(0, 4)) %>%
  num_nan_binary() %>% upset_custom(16, 0.5, TRUE)

upset_custom_data %>% truncate_rows() %>% set_f1_f2(c(0.5, 1), c(0, 4)) %>%
  num_nan_binary() %>% upset_custom(64, 0.3, TRUE)

inferno <- color_seq(5, "Inferno", TRUE)

upset_custom_data %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_variance(inferno)

upset_custom_data %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_variance(inferno, smooth = FALSE)

upset_custom_data %>% truncate_rows(500) %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_dendrogram(inferno)
