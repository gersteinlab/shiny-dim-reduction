parameters <- list(
  "PCA" = list(
    "PC_CAP" = 10,
    "PERPLEXITY_TYPES" = auto_detect_perplexity(x)
  ),
  "VAE" = list(
    "PC_CAP" = 10,
    "PERPLEXITY_TYPES" = auto_detect_perplexity(x)
  ),
  "UMAP" = list(
    "PC_CAP" = 10,
    "PERPLEXITY_TYPES" = auto_detect_perplexity(x)
  ),
  "PHATE" = list(
    "PC_CAP" = 10,
    "PERPLEXITY_TYPES" = auto_detect_perplexity(x)
  ),
  "SETS" = list(
    "THRESHOLDS" = auto_detect_thresholds(x)
  )
)

# x: the matrix to be analyzed
# order: a data.frame of metadata characteristics, required for Sets
# pc_cap: the number of principal components after stage one reduction,
#   used by PCA, VAE, UMAP, PHATE. Defaults to 10 and must be an integer at least 3.
# perplexities: a vector of perplexities, used by PCA, VAE, UMAP, PHATE. Defaults to
#   3 autodetected values.
# thresholds: a vector of thresholds, used by Sets. Defaults to a vector of 6, spanning 5
#   intervals. Used by Sets.

mat_reduction <- function(x, order, pc_cap = 10, perplexities = NULL, thresholds = NULL)
{
  result <- my_empty_list(c("PCA", "VAE", "UMAP", "PHATE", "Sets"))
}
