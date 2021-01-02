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
  print_clean("Testing color_seqs, press enter to continue.")
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

# -------
# RUN ALL
# -------
test_color_seq()