# The goal of this script is to perform UMAP on the data.

library(reticulate)
library(umap)

# --------------
# USER VARIABLES
# --------------

source("~/Justin-Tool/shiny-dim-reduction/scaling.R")

# ---------
# FUNCTIONS
# ---------

my_UMAP <- function(data, dim, perp) {
  umap::umap(
    data,
    method = "umap-learn",
    n_neighbors = perp,
    n_components = dim,
    metric = 'euclidean',
    n_epochs = 500,
    min_dist = 0.1,
    input = "data",
    init = "random",
    verbose = FALSE,
    random_state = 0,
    transform_state = 0
  )
}

# ---------------------------------------------
# UNIFORM MANIFOLD APPROXIMATION AND PROJECTION
# ---------------------------------------------

dog <- names(categories)
for (cat in dog)
{
  combined <- myRDS(sprintf("combined/combined_%s.rds", cat))
  
  for (sub in sub_groups[[cat]])
  {
    scaled <- get_safe_sub(sub, combined, decorations, cat)
    
    for (sca in sca_options)
    {
      scaled <- do_scal(sca, scaled)
      
      for (nor in nor_options)
      {
        scaled <- do_norm(nor, scaled)
        
        for (fea in c(1, 10, 100))
        {
          data <- feature_start(scaled, fea/100)
          for (nei in perplexity_types)
          {
            umap_title <- sprintf("UMAP/UMAP-%s_%s_%s_%s_%s_%s.rds", 
                                  nei, fea, nor, sca, sub, cat)
            print(umap_title)
            if (!file.exists(umap_title))
            {
              myRDS(umap_title, my_UMAP(data, pc_cap, nei))
            }
          }
        }
      }
    }
  }
}