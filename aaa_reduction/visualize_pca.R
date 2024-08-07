# The goal of this script is to convert PCA to vis data.

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# ---------
# FUNCTIONS
# ---------

# generates a row of props
prop_colnames <- c("Components", "Variance")
my_props <- function(data) {
  eigs <- data$sdev^2
  cols <- ncol(data$x)
  props <- cumsum(eigs/sum(eigs))
  df <- cbind.data.frame(1:cols, props[1:cols])
  colnames(df) <- prop_colnames
  df
}

# ---------------------
# PERFORM VISUALIZATION
# ---------------------

dog <- name_cat
for (cat in dog)
{
  for (sub in sub_groups[[cat]])
  {
    for (sca in sca_options)
    {
      for (nor in nor_options)
      {
        for (fea in c(1, 10, 100))
        {
          loc <- sprintf("%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          print(loc)

          pca <- readRDS(sprintf("PCA/PCA_%s", loc))

          explore <- pca$x

          # NONE
          saveRDS(explore, sprintf("vis-PCA/NONE_PCA_%s", loc))

          # tSNE
          tsne2 <- perplexity_list
          tsne3 <- perplexity_list

          start <- my_timer()
          for (nei in perplexity_types)
          {
            tsne2[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 2, nei)$Y
            tsne3[[sprintf("P%s", nei)]] <- my_rTSNE(explore, 3, nei)$Y
          }
          print(my_timer(start))
          saveRDS(list("TSNE2"=tsne2, "TSNE3"=tsne3),
                  sprintf("vis-PCA/TSNE_PCA_%s", loc))

          # SUM
          saveRDS(my_props(pca), sprintf("vis-PCA/SUM_PCA_%s", loc))
        }
      }
    }
  }
}
