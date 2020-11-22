# run the app first to load libraries

# lists memory usage (not used in app)
.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5) 
{
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand (not used in app)
lsos <- function(..., n=10) 
{
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

# example upset
data <- data.frame(matrix(sample(0:1, 100000, replace=TRUE), ncol = 4))
colnames(data) <- c("ENC001", "ENC002", "ENC003", "ENC004")
rownames(data) <- sprintf("Gene %s", 1:nrow(data))
upset <- upset_custom(data.frame(data), TRUE)

# I have no idea what the purpose of this is
data <- data.frame(matrix(sample(0:1, 16384, replace=TRUE), ncol = 4))
colnames(data) <- c("ENC001", "ENC002", "ENC003", "ENC004")
# View(data)
lol <- data[!duplicated(data), ]
lol <- lol[rowSums(lol) > 0,]
lol[,5] <- 0
colnames(lol)[5] <- "Occurrences"

for (i in 1:nrow(lol))
{
  subset <- which(lol[i,] == TRUE)
  
  for (j in 1:nrow(data))
  {
    if (sum(which(data[j,] == TRUE) %in% subset) == length(which(data[j,] == TRUE))
        && length(which(data[j,] == TRUE)) > 0)
      lol[i, 5] <- lol[i, 5]+1
  }
}
# View(lol)
target <- matrix(nrow=sum(lol[,5]), ncol=4)

index <- 1

for (i in 1:nrow(lol))
{
  for (j in 1:lol[i, 5])
  {
    for (k in 1:4)
    {
      target[index, k] <- as.numeric(lol[i, k])
    }
    index <- index+1
  }
}
# View(target)

upset_custom(data)
rownames(target) <- 1:nrow(target)
colnames(target) <- c("ENC001", "ENC002", "ENC003", "ENC004")
upset_custom(data.frame(target))

# df_keep_cols <- function(m)
# {
#   cols <- colnames(m)
#   m <- data.frame(m)
#   colnames(m) <- cols
#   m
# }
# 
# df_apply <- function(df, margin, fun)
# {
#   apply(df, margin, fun) %>% df_keep_cols
# }
# 
# char_fill <- function(x)
# {
#   if (is.nan(x) || is.na(x))
#     return("")
#   return(x)
# }

# Used to fill missing entries with 0s
add_missing <- function(table, ref, start, end)
{
  # numerical constants
  len <- nrow(table)
  s1 <- start-1
  missing <- end-s1
  revised_len <- len+missing
  
  p1 <- table[1:s1,]
  p2 <- table[start:len,]
  zeroes <- data.frame(matrix(0, ncol=ncol(table), nrow=missing))
  colnames(zeroes) <- colnames(p1)
  zeroes[1:missing,][,1] <- ref[start:end,][,1]
  revised <- rbind(p1, zeroes, p2)
  rownames(revised) <- 1:revised_len
  
  return(revised)
}

# Fixes discrepancies
circRNA <- add_missing(circRNA, miRNA, 3662, 3664)
ex_miRNA <- add_missing(ex_miRNA, miRNA, 3487, 3539)

# bios, dono, expe
bios_total <- read.csv(sprintf("Metadata/Bios_Mass/M%s.tsv", 1), sep='\t')
bios_total[,1] <- make.unique(bios_total[,1])
for (i in 2:nrow(common))
{
  bios_temp <- read.csv(sprintf("Metadata/Bios_Mass/M%s.tsv", i), sep='\t')
  bios_temp[,1] <- make.unique(bios_temp[,1])
  bios_total <- dplyr::full_join(bios_total, bios_temp, by="X.property")
  if (i %% 100 == 0)
    print(dim(bios_total))
}

expe_total <- read.csv(sprintf("Metadata/Expe_Mass/M%s.tsv", 1), sep='\t')
expe_total[,1] <- make.unique(expe_total[,1])
for (i in 2:nrow(common))
{
  expe_temp <- read.csv(sprintf("Metadata/Expe_Mass/M%s.tsv", i), sep='\t')
  expe_temp[,1] <- make.unique(expe_temp[,1])
  expe_total <- dplyr::full_join(expe_total, expe_temp, by="X.property")
  if (i %% 100 == 0)
    print(dim(expe_total))
}

dono_total <- read.csv(sprintf("Metadata/Dono_Mass/M%s.tsv", 1), sep='\t')
dono_total[,1] <- make.unique(dono_total[,1])
for (i in 2:nrow(common))
{
  dono_temp <- read.csv(sprintf("Metadata/Dono_Mass/M%s.tsv", i), sep='\t')
  dono_temp[,1] <- make.unique(dono_temp[,1])
  dono_total <- dplyr::full_join(dono_total, dono_temp, by="X.property")
  if (i %% 100 == 0)
    print(dim(dono_total))
}

colnames(bios_total) <- 1:ncol(bios_total)
colnames(expe_total) <- 1:ncol(expe_total)
colnames(dono_total) <- 1:ncol(dono_total)
rownames(bios_total) <- 1:nrow(bios_total)
rownames(expe_total) <- 1:nrow(expe_total)
rownames(dono_total) <- 1:nrow(dono_total)

saveRDS(bios_total, "Metadata/bios_total.rds")
saveRDS(expe_total, "Metadata/expe_total.rds")
saveRDS(dono_total, "Metadata/dono_total.rds")

vector(mode="list", length=len)
all_index <- 1
spe_index <- 1
for (i in 1:len)
{
  if (i %% 100 == 0)
    print(i)
  data <- rRNA_txt[[i]]
  colnames(data) <- data[1,]
  data <- data.frame(data[-1,]) %>% clean_names()
  
  all <- data %>% get_all()
  if (!is.null(all))
  {
    all_rRNA_data[[all_index]] <- all
    all_rRNA_added[i] <- TRUE
    all_index <- all_index + 1
  }
  
  spe <- data %>% get_species()
  if (!is.null(spe))
  {
    spe_rRNA_data[[spe_index]] <- spe
    spe_rRNA_added[i] <- TRUE
    spe_index <- spe_index + 1
  }
}
all_rRNA_data <- all_rRNA_data[1:sum(all_rRNA_added)]
spe_rRNA_data <- spe_rRNA_data[1:sum(spe_rRNA_added)]

all_index <- 1
spe_index <- 1
for (i in 1:len)
{
  if (i %% 100 == 0)
    print(i)
  data <- gene_txt[[i]]
  colnames(data) <- data[1,]
  data <- data.frame(data[-1,]) %>% clean_names()
  
  all <- data %>% get_all()
  if (!is.null(all))
  {
    all_gene_data[[all_index]] <- all
    all_gene_added[i] <- TRUE
    all_index <- all_index + 1
  }
  
  spe <- data %>% get_species()
  if (!is.null(spe))
  {
    spe_gene_data[[spe_index]] <- spe
    spe_gene_added[i] <- TRUE
    spe_index <- spe_index + 1
  }
}
all_gene_data <- all_gene_data[1:sum(all_gene_added)]
spe_gene_data <- spe_gene_data[1:sum(spe_gene_added)]

# print(dim(pca$rotation))
# 
# rotate <- pca$rotation[,1:3]
# ind <- rep(0, nrow(rotate))
# for (i in 1:nrow(rotate))
# {
#   target <- abs(rotate[i, ])
#   sum_ind <- which(target > sum(target)*0.5)
#   
#   if (length(sum_ind) > 0)
#     ind[i] <- sum_ind
# }
# pca <- prcomp(scaled, center = TRUE, rank. = pc_cap) 
# lol <- prcomp(pca$rotation, center = TRUE, rank. = 3)
# target <- lol$x
# 
# c1 <- apply(pca$rotation[,1:3], 1, function(x){
#   target <- abs(x)
#   sum_ind <- which(target > sum(target)*0.6)
#   names(sum_ind) <- NULL
#   
#   if (length(sum_ind) > 0)
#     return(sum_ind)
#   return(0)
# })
# indices <- which(c1 != 0)
# 
# c2 <- as.factor(c1[indices])
# bruh <- lol[indices,]

factors <- apply(metadata_miRNA, 2, function(x){length(unique(x))})
print(sprintf("%s: %s", names(factors), factors))

d2 <- strsplit(derp, c("\t", " ", "[", "]"), fixed=TRUE)[[1]]
d3 <- d2[str_detect(d2, '\\[')]
d4 <- regStr(d3, " \\[.*", "")
d5 <- regStr(d4, "\\n.*", "")


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

# upload Sets
setwd(dat_loc)
setwd("Sets")
filenames <- list.files()

for (file in filenames)
{
  data <- myRDS(file)
  save_db(data, sprintf("Sets/%s", file))
}





# -----
# exRNA
# -----
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
          biofluids, biofluids, color_seq(length(unique(biofluids)), "Rainbow", TRUE), "", TRUE)

net_data_tsne <- net_data_pca
for (sca in sca_options)
{
  net_data_tsne[[sca]] <- my_empty_list(nor_options)
  
  for (nor in nor_options[1:4])
  {
    net_data_tsne[[sca]][[nor]] <- my_rTSNE(net_data_pca[[sca]][[nor]]$x, 2, 50)
  }
}

source("~/Justin-Tool/shiny-dim-reduction/functions.R")

# -----
# ENTEx
# -----
# ---------
# TRANSPOSE
# ---------
source("~/Justin-Tool/shiny-dim-reduction/functions.R")

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
saveRDS(tsne_total, "tsne_total.rds")
saveRDS(umap_total, "umap_total.rds")
saveRDS(t1, "t1.rds")
saveRDS(t2, "t2.rds")
saveRDS(t3, "t3.rds")

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

# ----------------
# SETTING THE ROOT
# ----------------

# a root system that simply didn't work out

# a pre-determined location for saving the root location
root_save_loc <- "~/justin_tool_root.rds"

# prompt for saving the root directory
save_root <- function(root)
{
  save_root <- readline(prompt="
Type 'Y' and press enter to save the root directory to '~/justin_tool_root.rds'.
Type anything else and press enter to skip this step.")
  
  if (save_root == "Y")
  {
    saveRDS(root, root_save_loc)
    print_clean("The root directory has been saved to '~/justin_tool_root.rds'.")
  }
}

# sets the root directory
set_root <- function()
{
  # does a saved version of the root exist?
  exists_save <- tail(strsplit(root_save_loc, split='/', fixed="TRUE")[[1]], 1) %in% 
    list.files("~")
  
  if (exists("root"))
  {
    change_root <- readline(prompt=sprintf("
The current root directory of this tool is '%s'.
Type 'R' and press enter to reset the root and delete any saved root.
Type anything else and press enter to keep this directory.", root))
    
    if (change_root == 'R')
    {
      rm(root, envir=.GlobalEnv)
      
      if (exists_save)
        unlink(root_save_loc)
    }
    else
    {
      if (!exists_save)
        save_root(root)
    }
  }
  else
  {
    print_clean("The root directory of this tool has not been set.")
    
    if (exists_save)
    {
      read_root <- readline(prompt="
Type 'Y' and press enter to set the root from a saved location.
Type anything else and press enter to specify a new root.")
      
      if (read_root == "Y")
      {
        assign("root", readRDS(root_save_loc), envir = .GlobalEnv)
        print_clean("The root has been set from a saved location.")
      }
    }
  }
  
  while (!exists("root"))
  {
    attempt_root <- readline(prompt="Type 'Q' to quit or type in a directory.")
    
    if (attempt_root == "Q")
    {
      print_clean("set_root() was quit.")
      break
    }
    
    if (dir.exists(attempt_root))
    {
      assign("root", attempt_root, envir = .GlobalEnv)
      print_clean(sprintf("The root has been assigned to %s.", root))
      save_root(root)
    }
    else
    {
      print_clean("This directory could not be found.")
    }
  }
}

set_root()

# --------------
# USER VARIABLES
# --------------

# Only allow filtering on a characteristic with <= num_filters distinct values.
num_filters <- 60
# the maximum number of rows for a Sets matrix. Depends on browser strength.
max_points <- 20000

setwd(pro_loc)
dog <- name_cat
for (cat in dog)
{
  combined <- readRDS(sprintf("combined/combined_%s.rds", cat))
  
  for (sub in sub_groups[[cat]])
  {
    scaled <- get_safe_sub(sub, combined, decorations, cat)
    
    for (sca in sca_options) 
    {
      scaled <- combined %>% do_scal(sca, .) %>% do_norm(nor_options[1], .)
      sca_ind <- which(sca_options == sca)
      
      if (ncol(scaled) > max_points)
      {
        for (mar in mar_options)
        {
          scaled <- do_mark(mar, scaled, max_points)
          mar_ind <- which(mar_options == mar)
          
          saveRDS(t(scaled),
                  sprintf("Sets/%s_%s_%s_%s.rds", cat, sub, sca_ind, mar_ind))
        }
      }
      else
      {
        saveRDS(t(scaled),
                sprintf("Sets/%s_%s_%s_0.rds", cat, sub, sca_ind))
      }
    }
  }
}

if (length(my_chars()) < 1)
  return(NULL)

my_sub <- get_my_subset(decorations, input$category, subi())

sca_ind <- which(sca_options == input$scale)
addr <- sprintf("Sets/%s_%s_%s_0.rds", input$category, subi(), sca_ind)

if (length(my_sub) > max_points)
{
  mar_ind <- which(mar_options == input$mark)
  addr <- sprintf("Sets/%s_%s_%s_%s.rds", cat, sub, sca_ind, mar_ind)
  shinyjs::show("mark")
}
else
{
  shinyjs::hide("mark")
}

data <- readRDS(addr)
thre_1 <- input$set_thre[1]
thre_2 <- input$set_thre[2]

# thresholds
data <- data[,keep(),drop=FALSE] %>% between(thre_1, thre_2) %>%
  matrix(nrow=nrow(data), dimnames = dimnames(data))

target <- data %>% Matrix(sparse = TRUE)
summary <- summary(target)
rownames_final <- target@Dimnames[[2]]
summary_i <- summary[, 1]
summary_j <- summary[, 2]

associated <- order()[[filterby()]][keep()]
set_types <- unique(associated)
rowlen_final <- length(rownames_final)

final <- rep(0, rowlen_final*num_types)
lookup <- match(associated, set_types)
lookup2 <- (lookup - 1) * rowlen_final

for (len in 1:length(summary_i))
{
  num_i <- summary_i[len]
  index <- lookup2[num_i] + summary_j[len]
  final[index] <- final[index] + 1
}

final <- matrix(final, ncol=num_types)

numbers <- rep(0, num_types)
for (a in lookup)
  numbers[a] <- numbers[a] + 1
for (j in 1:num_types)
  final[,j] <- final[,j] / numbers[j]

colnames(final) <- set_types
rownames(final) <- rownames_final

if (ncol(final) < 1 || nrow(final) < 8)
  return(NULL)

data <- final %>% frac_convert(input$set_f1[1], input$set_f1[2]) %>% 
  rowSum_filter_bin(input$set_f2[1], input$set_f2[2]) %>% data.frame()

if (nrow(data) < 8)
  return(NULL)

downloadData(data)

if (ncol(data) == 1)
  return(venn1_custom(data, legend()))

if (ncol(data) == 2)
  return(venn2_custom(data, legend()))

return(upset_custom(data, legend(), ifelse(upse(), "freq", "degree")))

# -------
# MARKING
# -------

# performs feature marking
do_mark <- function(mar, scaled, num)
{
  if (mar == mar_options[1])
    return(scaled[,sample(1:ncol(scaled), num),drop=FALSE])
  if (mar == mar_options[2])
    return(select_top_cols(scaled, num, var))
  if (mar == mar_options[3])
    return(select_top_cols(scaled, num, mean))
  if (mar == mar_options[4])
    return(select_top_cols(scaled, num, max))
}

# options for set selection (marking features)
mar_options <- c("Random", "Variance", "Mean", "Maximum")
