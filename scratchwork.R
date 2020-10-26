library(UpSetR)
library(plotly)
library(viridis)
library(dplyr)

# Creates an UpSetR plot with the desired aesthetic.
upset_custom <- function(data, legend) {
  upset(data, nsets = ncol(data), nintersects = 50, 
        sets.x.label = "Number of Samples", 
        mainbar.y.label = "Number of Shared Characteristics", 
        show.numbers = ifelse(legend, "yes", "no"),
        order.by = "freq", decreasing = T, mb.ratio = c(0.7, 0.3), text.scale = 1,
        line.size = 0.1, point.size = 2.1, shade.alpha = 0.4, matrix.dot.alpha = 0.5, 
        matrix.color = "royalblue4", main.bar.color = "royalblue4",
        sets.bar.color = "royalblue4", shade.color = "lightskyblue")
}

# Creates a heatmap for sets on plotly2.
plotly2_binary <- function(binary, title, legend){
  binary <- binary[order(rowSums(binary),decreasing=T),]
  rows <- sprintf("X:%s", substring(rownames(binary), 0, 50))
  cols <- sprintf("Y:%s", substring(colnames(binary), 0, 50))
  rownames(binary) <- NULL
  colnames(binary) <- NULL
  plot_ly(x = rows, y = cols, z = t(binary), type="heatmap",
          colors=viridis_pal(option="magma", direction=-1)(5)) %>%
    layout(title = title, showlegend = legend)
}

# Converts a matrix into a binary matrix: 1 if lower <= x <= upper, 0 otherwise.
frac_convert <- function(data, lower, upper){
  target <- data %>% apply(1:2, function(i){as.numeric(between(i, lower, upper))})
  target[rowSums(target) > 0, colSums(target) > 0, drop = FALSE]
}

# example matrix
data <- data.frame(matrix(sample(0:1, 100000, replace=TRUE), ncol = 4))
colnames(data) <- c("ENC001", "ENC002", "ENC003", "ENC004")
rownames(data) <- sprintf("Gene %s", 1:nrow(data))

# convert to binary, depending on range of expression
binary <- frac_convert(data, 0.5, 0.8)

plotly <- plotly2_binary(data.frame(data), "Example Plotly", TRUE)
upset <- upset_custom(data.frame(binary), TRUE)

# displays upset plot
upset
# displays plotly
# plotly

# For each sample in "samples", is it present in "selected"?
get_filter <- function(samples, selected) {
  sapply(samples, function(i){any(grepl(i,selected))})
}

# Let's race!
start <- Sys.time()
b1 <- sapply(lol, function(i){any(grepl(i,lol2))})
print(as.numeric(Sys.time())-as.numeric(start))

start <- Sys.time()
b2 <- sapply(lol, function(i){i %in% lol2})
print(as.numeric(Sys.time())-as.numeric(start))

start <- Sys.time()
b3 <- lol %in% lol2
print(as.numeric(Sys.time())-as.numeric(start))

start <- Sys.time()
b4 <- filter(lol, contains(lol2))
print(as.numeric(Sys.time())-as.numeric(start))

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