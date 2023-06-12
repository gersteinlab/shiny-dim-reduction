# The purpose of this file is to manage color scales for our data.

if (!exists("sdr_config"))
  source("app/install.R")

library(viridis)

# -----------------
# UTILITY FUNCTIONS
# -----------------

is_color_vec <- function(x)
{
  is.character(x) && all(grepl('^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$', x))
}

is_color_scale <- function(x)
{
  is_color_vec(x) && is.character(names(x))
}

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
  stop_f("Unsupported color_type: %s", color_type)
}
