# The goal of this script is to perform PHATE on the data.

setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("scaling.R", encoding="UTF-8")

library(phateR)

# ---------
# FUNCTIONS
# ---------

my_PHATE <- function(data, dim, perp) {
  phateR::phate(
    data,
    ndim = dim,
    knn = perp,
    decay = 40,
    n.landmark = 2000,
    gamma = 1,
    t = "auto",
    mds.solver = "sgd",
    knn.dist.method = "euclidean",
    init = NULL,
    mds.method = "metric",
    mds.dist.method = "euclidean",
    t.max = 100,
    npca = pc_cap,
    verbose = 1,
    n.jobs = 1,
    seed = 0)
}

# ---------------------------------------------
# UNIFORM MANIFOLD APPROXIMATION AND PROJECTION
# ---------------------------------------------

for (cat in names(categories))
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  
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
            for (dim in c(2,3))
            {
              phate_title <- sprintf("PHATE/PHATE-%s-%s_%s_%s_%s_%s_%s.rds", 
                                     nei, dim, fea, nor, sca, sub, cat)
              if (!file.exists(phate_title))
              {
                print(phate_title)
                my_save <- my_PHATE(data, dim, nei)
                # prevents saving raw data
                my_save$params$data <- NULL
                myRDS(phate_title, my_save)
              }
            }
          }
        }
      }
    }
  }
}