# This file tests plotting.R.
# source("tests/test_plotting.R")

# -----
# SETUP
# -----

library(testthat)

source("app/plotting.R")

# displays a color sequence as a rectangular palette
display_palette <- function(color_seq, title = "")
{
  stopifnot(is_color_seq(color_seq))
  nums <- seq_along(color_seq)
  image(nums, 1, as.matrix(nums), col = color_seq,
        axes = FALSE, xlab = title, ylab = "")
}

# -----
# TESTS
# -----

test_that("make_transparent() works", {
  c("#ABCDEF", "#ABCDEF00", "#ABCDEFZZZZZ",
    "#FF0000", "#00FF00", "#0000FF") %>%
    make_transparent() %>% is_color_seq() %>% expect_true()
})

test_that("make_color_seq() works", {
  sdr_color_seq %>% is_color_seq() %>% expect_true()

  expect_identical(make_color_seq(0L), character())
  expect_identical(make_color_seq(1L), sdr_color_seq[1])
  expect_identical(make_color_seq(2L), sdr_color_seq)
})

test_that("ggplot2_shape_seq() works", {
  ggplot2_shape_seq(1000) %>% is.integer() %>% expect_true()
})

show_color_seqs <- function()
{
  cat("Showing color sequences ...\n")
  display_palette(sdr_color_seq[1], "Yale Blue")
  display_palette(sdr_color_seq, "Yale Blue + Harvard Crimson")

  non_custom_types <- setdiff(unlist(color_seq_types), "Custom")
  for (color_type in non_custom_types)
  {
    color_seq <- make_color_seq(1000L, color_type)
    display_palette(color_seq, sprintf("%s Normal", color_type))

    color_rev <- rev_color_seq(color_seq)
    display_palette(color_rev, sprintf("%s Reverse", color_type))
  }

  display_palette("#000000", "End of Presentation")
}

color_seq10 <- make_color_seq(10L)

show_boxplot_beeswarm <- function(r10 = 100)
{
  cat("Showing boxplot_beeswarm ...\n")
  data <- data.frame(matrix(nrow = 10 * r10, ncol = 2))
  data[,1] <- sprintf("c%s", rep(1:10, r10))
  data[,2] <- sample(seq(0, 1, by = 0.01), 10*r10, replace = TRUE)^2
  colnames(data) <- c("X_coord", "Y_coord")

  boxplot_beeswarm(data, color_seq10, "Test with Embedded Legend", TRUE)
  boxplot_beeswarm(data, color_seq10, "Test with External Legend", FALSE)
}

test_ggplot2_2d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  cat("Showing ggplot2_2d ...\n")
  ggplot2_2d(1:(10*n), 1:(10*n), types, types, color_seq10)
}

test_plotly_2d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  cat("Showing plotly_2d ...\n")
  plotly_2d(1:(10*n), 1:(10*n), types, types, color_seq10)
}

test_plotly_3d <- function(n = 10)
{
  types <- rep(LETTERS[1:10], n)
  cat("Showing plotly_3d ...\n")
  plotly_3d(1:(10*n), 1:(10*n), 1:(10*n), types, types, color_seq10)
}

get_set_data <- function(row = 10000, col = 4)
{
  test <- seq(0, 1, 0.1)
  data <- matrix(sample(test, row*col, replace=TRUE), nrow=row, ncol=col)
  rownames(data) <- sprintf("Row_%s", 1:row)
  colnames(data) <- sprintf("Col_%s", 1:col)
  data
}

stop("Automated testing complete! All subsequent functions have to be run manually.")

# --------------
# DEMONSTRATIONS
# --------------

default_plot
show_color_seqs()
show_boxplot_beeswarm()
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

inferno <- make_color_seq(5L, "Inferno") %>% rev()

upset_custom_data %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_variance(inferno)

upset_custom_data %>% truncate_rows() %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_variance(inferno, smooth = FALSE)

upset_custom_data %>% truncate_rows(500) %>% sort_row_sums() %>%
  set_f1_f2(c(0, 1), c(0, 4)) %>% plotly_heatmap_dendrogram(inferno)

my_datatable()

my_datatable(data.frame(cbind(upset_custom_data[1:100,], rep(LETTERS[1:10], 10))))

dev.off()
venn1_custom(100, "test")
