# The goal of this script is to perform PCA on the data.

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# ----------------------------
# PRINCIPAL COMPONENT ANALYSIS
# ----------------------------

dog <- name_cat
for (cat in dog)
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))

  for (sub in sub_groups[[cat]])
  {
    scaled <- get_col_sub(combined, cat, sub)

    for (sca in sca_options)
    {
      scaled <- do_scal(sca, scaled)

      for (nor in nor_options)
      {
        scaled <- do_norm(nor, scaled)

        for (fea in c(1, 10, 100))
        {
          pca_title <- sprintf("PCA/PCA_%s_%s_%s_%s_%s.rds", fea, nor, sca, sub, cat)
          if (!file.exists(pca_title))
          {
            print(pca_title)
            scaled <- feature_start(scaled, 1.0*fea/100)
            pca <- prcomp(scaled, center = TRUE, rank. = pc_cap)
            pca$rotation <- NULL
            pca$center <- NULL
            saveRDS(pca, pca_title)
          }
        }
      }
    }
  }
}
