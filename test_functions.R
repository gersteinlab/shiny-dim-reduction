# This file tests app_functions.R and ui_functions.R

# -----
# SETUP
# -----

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("ui_functions.R", encoding="UTF-8")

# -----
# TESTS
# -----

test_color_seq <- function()
{
  print_clean("Functions tested: display_palette, color_seq")
  print_clean("Press enter to continue.")
  display_palette(single_color_seq, "Yale Blue")
  prompt <- readline()
  display_palette(double_color_seq, "Yale Blue + Harvard Crimson")
  prompt <- readline()
  
  for (color in c("Rainbow", "Heat", "Terrain", "Topography", "CM",
              "Viridis", "Magma", "Plasma", "Inferno", "Cividis"))
  {
    display_palette(
      color_seq(1000, color_type=color, reverse=FALSE), sprintf("%s Normal", color))
    prompt <- readline()
    display_palette(
      color_seq(1000, color_type=color, reverse=TRUE), sprintf("%s Reverse", color))
    prompt <- readline()
  }
  
  display_palette("#000000", "End of Presentation")
}

test_boxplot_beeswarm <- function(r10 = 100)
{
  print_clean("Functions tested: boxplot_beeswarm")
  data <- data.frame(matrix(nrow = 10*r10, ncol=2))
  data[,1] <- sprintf("c%s", rep(1:10, r10))
  data[,2] <- sample(seq(0, 1, by=0.01), 10*r10, replace=TRUE)^2
  colnames(data) <- c("X_coord", "Y_coord")
  cs <- color_seq(10)
  
  print_clean("Testing, press enter to continue.")
  boxplot_beeswarm(data, cs, "Test with Embedded Legend", TRUE)
  prompt <- readline()
  boxplot_beeswarm(data, cs, "Test with External Legend", FALSE)
}

# -------
# RUN ALL
# -------
test_color_seq()
test_boxplot_beeswarm()