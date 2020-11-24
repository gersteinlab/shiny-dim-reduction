# install.packages("UpSetR")
# install.packages("dplyr")
library(UpSetR)
library(dplyr)

# important: the location of this file
yex <- "C:/Users/Justin Chang/Desktop/ProjectsR/Justin-Tool/ENTEx/Yucheng_Example"
# a threshold parameter - can be from 1 to 11, 1 is least strict
thresh <- 2

# ---------
# FUNCTIONS
# ---------

# creates an empty list with names
my_empty_list <- function(names)
{
  target <- vector(mode="list", length=length(names))
  names(target) <- names
  target
}

# Converts a matrix into a binary matrix: 1 if lower <= x <= upper, 0 otherwise.
frac_convert <- function(data, lower, upper)
{
  for (j in 1:ncol(data))
    data[,j] <- between(data[,j], lower, upper)
  storage.mode(data) <- "numeric"
  data[rowSums(data) > 0, colSums(data) > 0, drop = FALSE]
}

# Creates an UpSetR plot with the desired aesthetic.
upset_custom <- function(data) 
{
  upset(data, nsets = ncol(data), nintersects = 50, 
        sets.x.label = "Features Per Factor Level", 
        mainbar.y.label = "Features Per Factor Subset", 
        show.numbers = "yes",
        order.by = "freq", decreasing = T, mb.ratio = c(0.7, 0.3), text.scale = 1,
        line.size = 0.1, point.size = 2.1, shade.alpha = 0.4, matrix.dot.alpha = 0.5, 
        matrix.color = "royalblue4", main.bar.color = "royalblue4",
        sets.bar.color = "royalblue4", shade.color = "lightskyblue")
}

# -----------
# COMPUTATION
# -----------

setwd(yex)

# scale options 
sca_options <- c("Logarithmic", "Linear")

# load files
decorations <- readRDS("decorations.rds")
order_total <- readRDS("order_total.rds")
categories_full <- readRDS("categories_full.rds")

# create categories
cat_groups <- lapply(categories_full, function(x){names(x)})
name_cat <- unlist(cat_groups)
num_cat <- length(name_cat)
categories <- unlist(categories_full, recursive=FALSE)
names(categories) <- name_cat
names(name_cat) <- NULL

# calculate
sets <- my_empty_list(names(categories))
summary <- sets

for (cat in names(categories))
{
  sets[[cat]] <- my_empty_list(sca_options)
  summary[[cat]] <- sets[[cat]]
  
  for (sca in sca_options)
  {
    sets[[cat]][[sca]] <- readRDS(
      sprintf("Sets/Sets-%s_%s_%s.rds", thresh, sca, cat))$TISSUE %>% 
      frac_convert(0.4, 1) %>% data.frame()
    summary[[cat]][[sca]] <- apply(sets[[cat]][[sca]], 1, sum)
  }
}

table <- matrix(ncol=3, nrow=length(categories)*length(sca_options))
colnames(table) <- c("Total Tissue Number", "Uniquely (single tissue)", "Broadly (>90% all tissues)")
rownames(table) <- 1:30

zones <- list(c(0, 1),
              c(2, 5),
              c(6, 25),
              c(25, .Machine$integer.max))

for (cn in 1:length(categories))
{
  cat <- names(categories)[cn]
  for (sn in 1:2)
  {
    sca <- sca_options[sn]
    print(sprintf("%s %s:", cat, sca), quote=FALSE)
    ind <- 2*cn + sn - 2
    print(ind)
    
    rownames(table)[ind] <- sprintf("%s (%s)", cat, sca)
    table[ind, 1] <- length(summary[[cat]][[sca]])
    table[ind, 2] <- length(which(between(
      summary[[cat]][[sca]], 1, 1)))
    table[ind, 3] <- length(which(between(
      summary[[cat]][[sca]], 0.9*max(summary[[cat]][[sca]]), max(summary[[cat]][[sca]]))))
    
    # for (zone in zones)
    # {
    #   print(sprintf(
    #     "[%s, %s]: %s features", zone[1], zone[2], length(which(between(
    #       summary[[cat]][[sca]], zone[1], zone[2])))
    #   ), quote=FALSE)
    # }
    # 
    # print("", quote=FALSE)
  }
}

# For exploring UpSetR plots
# plot <- upset_custom(sets[["H3K27ac"]][["Logarithmic"]])
# plot