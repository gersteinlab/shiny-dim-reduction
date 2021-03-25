# run the app first to load libraries

# --------------
# MISC FUNCTIONS
# --------------

df_keep_cols <- function(m)
{
  cols <- colnames(m)
  m <- data.frame(m)
  colnames(m) <- cols
  m
}

df_apply <- function(df, margin, fun)
{
  apply(df, margin, fun) %>% df_keep_cols
}

char_fill <- function(x)
{
  if (is.nan(x) || is.na(x))
    return("")
  return(x)
}

# ---------------------
# PCA ROTATION PCA TEST
# ---------------------
print(dim(pca$rotation))

rotate <- pca$rotation[,1:3]
ind <- rep(0, nrow(rotate))
for (i in 1:nrow(rotate))
{
  target <- abs(rotate[i, ])
  sum_ind <- which(target > sum(target)*0.5)

  if (length(sum_ind) > 0)
    ind[i] <- sum_ind
}
pca <- prcomp(scaled, center = TRUE, rank. = pc_cap)
lol <- prcomp(pca$rotation, center = TRUE, rank. = 3)
target <- lol$x

c1 <- apply(pca$rotation[,1:3], 1, function(x){
  target <- abs(x)
  sum_ind <- which(target > sum(target)*0.6)
  names(sum_ind) <- NULL

  if (length(sum_ind) > 0)
    return(sum_ind)
  return(0)
})
indices <- which(c1 != 0)

c2 <- as.factor(c1[indices])
bruh <- lol[indices,]

# ----------------------
# Kun's Methylation Data
# ----------------------
setwd(sprintf("%s/Kun", raw_loc))
filenames <- list.files()
first <- read_tsv_text(filenames[1])
f_matrix <- r1_to_cols(do.call(rbind, first))[,-1]
my_ids <- f_matrix[,1]
f_matrix <- f_matrix[,-1]
target <- apply(f_matrix, 1:2, as.numeric)
target <- target[rowSums(is.na(target)) == 0, ]

# -----------------
# Yucheng's Example
# -----------------
setwd(sprintf("%s/decorations_app/Dependencies", roo_loc))
myRDS("decorations.rds", decorations)
feat_counts <- my_empty_list(names(all_decorations))
for (dec in names(all_decorations))
  feat_counts[[dec]] <- nrow(all_decorations[[dec]])
sub_cat <- list("Decorations"=feat_counts)
myRDS("categories_full.rds", sub_cat)

# Continuing ...
myRDS("all_decorations.rds", all_decorations)
setwd(sprintf("%s/decorations_app/data", roo_loc))
for (dec in names(all_decorations))
{
  compressed <- all_decorations[[dec]] %>% Matrix(sparse=TRUE)
  myRDS(sprintf("%s.rds", dec),
        list("TISSUE"=compressed)
  )
}

# ------------------------
# EXRNA NORMALIZATION TEST
# ------------------------
order <- data.frame(matrix(colnames(all_decorations[[1]]), ncol=1))
colnames(order) <- "TISSUE"

new_order <- my_empty_list(names(all_decorations))
for (dec in names(all_decorations))
  new_order[[dec]] <- order
myRDS("order_total.rds", new_order)

cat <- name_cat[1]
combined <- myRDS(sprintf("combined/combined_%s.rds", cat))
sub <- sub_groups[[cat]][1]
scaled_raw <- get_safe_sub(sub, combined, decorations, cat)
net_data_pca <- my_empty_list(sca_options)

for (sca in sca_options)
{
  scaled <- do_scal(sca, scaled_raw)
  scaled <- do_scal(sca, scaled)
  net_data_pca[[sca]] <- my_empty_list(nor_options)

  for (nor in nor_options[1:4])
  {
    print(nor)
    scaled <- do_norm(nor, scaled)
    s2 <- feature_start(scaled, 1.0*10/100)
    pca <- prcomp(s2, center = TRUE, rank. = pc_cap)
    net_data_pca[[sca]][[nor]] <- pca
  }
}

net_data_tsne <- net_data_pca
for (sca in sca_options)
{
  net_data_tsne[[sca]] <- my_empty_list(nor_options)

  for (nor in nor_options[1:4])
  {
    net_data_tsne[[sca]][[nor]] <- my_rTSNE(net_data_pca[[sca]][[nor]]$x, 2, 50)
  }
}

biofluids <- order_total$miRNA$BIOFLUID

tsne <- net_data_tsne[["Logarithmic"]][["Global Min-Max"]]$Y
plotly_2d(tsne[,1], tsne[,2], "1", "2", "markers",
          biofluids, biofluids,
          color_seq(length(unique(biofluids)), "Rainbow", TRUE), "", TRUE)

net_data_tsne <- net_data_pca
for (sca in sca_options)
{
  net_data_tsne[[sca]] <- my_empty_list(nor_options)

  for (nor in nor_options[1:4])
  {
    net_data_tsne[[sca]][[nor]] <- my_rTSNE(net_data_pca[[sca]][[nor]]$x, 2, 50)
  }
}

# ---------------
# ENTEX TRANSPOSE
# ---------------
decor <- decorations$cCREs$Subsets
ref <- decor$Reference
decor$Reference <- NULL

for (dec in names(decor))
{
  decor[[dec]] <- ref[decor[[dec]]]
}

setwd(pro_loc)
dog <- names(categories)[1:10]
umap_total <- my_empty_list(dog)
tsne_total <- umap_total
u1_plots <- umap_total
t1_plots <- umap_total
u2_plots <- u1_plots
t2_plots <- t1_plots
u3_plots <- u1_plots
t3_plots <- t1_plots

cat <- "Methylation"
scaled <- myRDS(sprintf("combined/combined_%s.rds", cat))
indices <- sample(1:226807, 20000)
t1 <- rep("Unknown", 20000)
t2 <- t1
t3 <- t1
samples <- colnames(scaled)[indices]
d1 <- decor[c(5,8, 13)]
d2 <- decor[c(1:4, 6:7, 9:12)]
d3 <- decor[14:22]
r1 <- names(d1)
r2 <- names(d2)
r3 <- names(d3)

for (i in 1:20000)
{
  if (i %% 1000 == 0)
    print(i)

  samp <- samples[i]

  for (dec in r1)
  {
    if (samp %in% d1[[dec]])
    {
      t1[i] <- dec
      break
    }
  }

  for (dec in r2)
  {
    if (samp %in% d2[[dec]])
    {
      t2[i] <- dec
      break
    }
  }

  for (dec in r3)
  {
    if (samp %in% d3[[dec]])
    {
      t3[i] <- dec
      break
    }
  }
}

good <- which(t3 != "Unknown")
t11 <- t1[good]
t22 <- t2[good]
t33 <- t3[good]
col1 <- color_seq(length(unique(t11)), "Rainbow", TRUE)
col2 <- color_seq(length(unique(t22)), "Rainbow", TRUE)
col3 <- color_seq(length(unique(t33)), "Rainbow", TRUE)
indices <- indices[good]

sca <- "Linear"
nor <- "Global Min-Max"
nei <- 30

for (cat in dog)
{
  combined <- myRDS(sprintf("combined/combined_%s.rds", cat))

  scaled <- combined %>% do_scal(sca, .) %>% do_norm(nor, .)
  trans <- t(scaled[,indices])
  print(sprintf("Dimensions of %s: %s %s", cat, dim(trans)[1], dim(trans)[2]))

  umap_total[[cat]] <- my_UMAP(trans, pc_cap, nei)
  umap_emb <- umap_total[[cat]]$layout
  tsne_total[[cat]] <- my_rTSNE(umap_emb, 2, nei)
}

setwd(roo_loc)
saveRDS(tsne_total, "Tranpose/tsne_total.rds")
saveRDS(umap_total, "Tranpose/umap_total.rds")
saveRDS(t1, "Tranpose/t1.rds")
saveRDS(t2, "Tranpose/t2.rds")
saveRDS(t3, "Tranpose/t3.rds")

my_subsetter <- function(types)
{
  uni <- unique(types)
  ltype <- lapply(as.list(uni), function(x){length(which(types == x))})
  lnum <- mean(unlist(ltype))
  lind <- my_empty_list(uni)

  for (u in uni)
  {
    num <- 0
    for (i in 1:length(types))
    {
      if (types[i] == u)
      {
        num <- num+1
        if (num > lnum)
          types[i] <- "REMOVE"
      }
    }
  }

  which(types != "REMOVE")
}

s1 <- my_subsetter(t11)
s2 <- my_subsetter(t22)
s3 <- my_subsetter(t33)

for (cat in dog)
{
  umap_emb <- umap_total[[cat]]$layout
  tsne_emb <- tsne_total[[cat]]$Y

  umap_x <- umap_emb[,1]
  umap_y <- umap_emb[,2]
  tsne_x <- tsne_emb[,1]
  tsne_y <- tsne_emb[,2]

  t1_plots[[cat]] <- plotly_2d(tsne_x[s1], tsne_y[s1], "1", "2", "markers",
                               t11[s1], t11[s1], col1, "", TRUE)
  u1_plots[[cat]] <- plotly_2d(umap_x[s1], umap_y[s1], "1", "2", "markers",
                               t11[s1], t11[s1],  col1, "", TRUE)
  t2_plots[[cat]] <- plotly_2d(tsne_x[s2], tsne_y[s2], "1", "2", "markers",
                               t22[s2], t22[s2], col2, "", TRUE)
  u2_plots[[cat]] <- plotly_2d(umap_x[s2], umap_y[s2], "1", "2", "markers",
                               t22[s2], t22[s2],  col2, "", TRUE)
  t3_plots[[cat]] <- plotly_2d(tsne_x[s3], tsne_y[s3], "1", "2", "markers",
                               t33[s3], t33[s3], col3, "", TRUE)
  u3_plots[[cat]] <- plotly_2d(umap_x[s3], umap_y[s3], "1", "2", "markers",
                               t33[s3], t33[s3],  col3, "", TRUE)
}

# ------------------
# UNREVISED PORTABLE
# ------------------

# To begin, install R-Portable and GoogleChromePortable into your dist folder
# Add to R-Portable/App/R-Portable/etc/Rprofile.site:
#   .First = function(){.libPaths(.Library)}
#
# Open up R-Portable as an admin. Check library is portable with .libPaths().
# Install all necessary packages ...
# DO NOT COMPILE FROM SOURCE! Use the binaries from CRAN.
#
# Create a file called runShinyApp.R with the following lines:
#   message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
# options(browser='C:/Program Files (x86)/Google/Chrome/Application/chrome.exe')
# shiny::runApp('app', launch.browser=T)
#
# Create a file called run.bat with the following lines:
#   SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
# R-Portable\App\R-Portable\bin\Rscript.exe %ROPTS% runShinyApp.R 1> ShinyApp.log 2>&1
#
# copy and paste the app folder into the dist ... final product
# JC-Portable
# -runShinyApp.R
# -run.bat
# -app (DIR)
# -R-Portable (DIR)
# -GoogleChromePortable (DIR)

# --------------
# SET THRESHOLDS
# --------------

prec <- 200
hmm <- data.frame(matrix(0, nrow=prec, ncol=10))
colnames(hmm) <- dog[1:10]

for (cat in dog[1:10])
{
  print(cat)
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  scaled <- combined %>% do_scal("Linear", .) %>% do_norm(nor_options[1], .)
  for (i in 1:prec)
  {
    hmm[[cat]][i] <- sum(colSums(scaled >= i/prec) > 0)
  }
}

hmm$threshold <- 1:prec/prec
h2 <- melt(hmm, id.vars = "threshold")
colors <- color_seq(10, "Rainbow", TRUE)
plotly_2d(h2$threshold, h2$value,"","","lines+markers",
          h2$variable, h2$variable, colors, "", TRUE)

h3 <- hmm
for (i in 1:10)
  h3[,i] <- log2(h3[,i])
h4 <- melt(h3, id.vars="threshold")
plotly_2d(h4$threshold, h4$value,"","","lines+markers",
          h4$variable, h4$variable, colors, "", TRUE)

for (i in 1:9)
  print_clean(sprintf("%s: %s", dog[i], summary(lm(threshold~get(dog[i]),h3))$r.squared))

for (i in 10)
  print_clean(sprintf("%s: %s", dog[i], summary(lm(threshold~get(dog[i]),hmm))$r.squared))

# intended for converter

# sorts a list by names
sort_by_names <- function(target)
{
  target[base::order(names(target))]
}

# selects the top num columns of scaled, based on the numerical output of fun
select_top_cols <- function(scaled, num, fun)
{
  outcomes <- apply(scaled, 2, fun)
  sorted <- sort(outcomes, decreasing=TRUE, index.return=TRUE)$ix[1:num]
  scaled[,sorted,drop=FALSE]
}

# PORTABLE - currently not revised

# To begin, install R-Portable and GoogleChromePortable into your dist folder
# Add to R-Portable/App/R-Portable/etc/Rprofile.site:
#   .First = function(){.libPaths(.Library)}
#
# Open up R-Portable as an admin. Check library is portable with .libPaths().
# Install all necessary packages ...
# DO NOT COMPILE FROM SOURCE! Use the binaries from CRAN.
#
# Create a file called runShinyApp.R with the following lines:
#   message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
# options(browser='C:/Program Files (x86)/Google/Chrome/Application/chrome.exe')
# shiny::runApp('app', launch.browser=T)
#
# Create a file called run.bat with the following lines:
#   SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
# R-Portable\App\R-Portable\bin\Rscript.exe %ROPTS% runShinyApp.R 1> ShinyApp.log 2>&1
#
# copy and paste the app folder into the dist ... final product
# JC-Portable
# -runShinyApp.R
# -run.bat
# -app (DIR)
# -R-Portable (DIR)
# -GoogleChromePortable (DIR)
