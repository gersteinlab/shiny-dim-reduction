# The goal of this script is to convert raw ENTEx data
# into numeric data and metadata

project_name <- "ENTEx"
source("~/Justin-Tool/shiny-dim-reduction/converter.R")

# -----------------------
# LIBRARIES AND FUNCTIONS
# -----------------------

library(Matrix)

# creates the order data frame
create_order <- function(data, colnames)
{
  order <- matrix(ncol = length(colnames), nrow = nrow(data))
  colnames(order) <- colnames
  data.frame(order)
}

# apply to colnames of all of Fabio's data to remove EH38D and suffix!
EH38D_removal <- function(char_vec)
{
  splits <- substr(char_vec, start = 1, stop = 5)
  if (length(which(splits == "EH38D")) == length(char_vec))
    return(substr(char_vec, start = 6, stop = 12))
  return(char_vec)
}

# apply to all of Fabio's data after EH38D removal
factor_type_split <- function(data)
{
  splits <- strsplit(colnames(data), split="_", fixed=TRUE)
  colnames(data) <- unlist(lapply(splits, function(x){x[1]}))
  factor_types <- unlist(lapply(splits, function(x){x[2]}))
  unique_factors <- unique(factor_types)
  
  target <- vector(mode="list", length=length(unique_factors))
  names(target) <- unique_factors
  
  for (type in names(target))
  {
    matched <- which(factor_types == type)
    print(sprintf("Number of %s: %s (%s)", type, length(matched), 
                  round(length(matched)/length(colnames(data)), 3)
    ))
    target[[type]] <- data[,matched]
  }
  
  target
}

# checks for decorations present in all tissues
all_binary <- function(x){
  m <- apply(x, 1, function(k){
    
    counter <- 0
    for (i in 2:length(k))
      if (k[i] == "1")
        counter <- counter+1
      
      return(counter == length(k)-1)
  })
  x[m,,drop=FALSE]
}

# checks for decorations present in any tissue
any_binary <- function(x){
  m <- apply(x, 1, function(k){
    
    counter <- 0
    for (i in 2:length(k))
      if (k[i] == "1")
        counter <- counter+1
      
      return(counter > 0)
  })
  x[m,,drop=FALSE]
}

# --------------
# USER VARIABLES
# --------------

categories_full <- list(
  "cCREs"=list(
    "H3K27ac"=926535,
    "H3K27me3"=926535,
    "H3K4me1"=926535,
    "H3K4me3"=926535,
    "H3K9me3"=926535,
    "ATACseq"=926535,
    "CTCF"=926535,
    "DNase"=926535,
    "POLR2A"=926535,
    "Methylation"=226807
  ),
  "Expression"=list(
    "Protein_Coding_Genes"=17598,
    "Pseudogenes"=5657,
    "Long Non-Coding RNAs"=0
  ),
  "Proteomics"=list(
    "Peptide"=9411,
    "FPKM_TPM"=19377,
    "OMS"=6558
  )
)

cat_groups <- lapply(categories_full, function(x){names(x)})
name_cat <- unlist(cat_groups)
num_cat <- length(name_cat)
categories <- unlist(categories_full, recursive=FALSE)
names(categories) <- name_cat

# -----
# FABIO
# -----

order_total <- my_empty_list(name_cat)

fabio_filenames <- list(
  "k27ac.normalized.FN_JR.txt",
  "raw.average.K27me3.table.quantile.scaled.txt",
  "raw.average.k4me1.table.quantile.scaled.txt",
  "raw.average.k4me3.table.quantile.scaled.txt",
  "raw.average.K9me3.table.quantile.scaled.txt",
  
  "raw.average.ATAC.table.quantile.scaled.txt",
  "raw.average.CTCF.table.quantile.scaled.txt",
  "raw.average.DNase.table.quantile.scaled.txt",
  "raw.average.POLR2A.table.quantile.scaled.txt",
  "raw.average.methylation.table.quantile.scaled.txt"
)

dog <- name_cat[1:10]
names(fabio_filenames) <- dog

for (cat in dog)
{
  filename <- fabio_filenames[[cat]]
  
  # read data
  setwd(raw_loc)
  setwd("cCREs")
  print(sprintf("Reading %s for %s", filename, cat))
  my_lists <- read_tsv_text(filename)
  my_colnames <- EH38D_removal(my_lists[[1]])
  my_lists[[1]] <- c("CORNER_CORNER", my_colnames)
  data <- do.call(rbind, my_lists) %>% r1_to_cols()
  rm(my_lists)
  
  # process metadata
  print(sprintf("Processing metadata for %s", cat))
  order <- create_order(data, c("SAMPLE", "TISSUE", "INDIVIDUAL"))
  order[,1] <- data[,1]
  
  # delete samples row in data and convert to numeric
  data <- convert_to_num(data[,-1])
  print(ncol(data))
  
  for (j in 1:nrow(order))
  {
    words <- strsplit(order[j,1], ".ENC")[[1]]
    if (cat == "Methylation")
      words <- strsplit(order[j,1], "_enc")[[1]]
    
    words <- repStr(
      words, 
      c("_", "andrenal", "pancrease", 
        "centricle", "peyers", "Peyers"),
      c(" ", "adrenal", "pancreas",
        "ventricle", "Peyer's", "Peyer's")
    )
    
    order[j, 2] <- words[1]
    order[j, 3] <- words[2]
  }
  
  order_total[[cat]] <- order
  
  # save data
  setwd(pro_loc)
  saveRDS(data, sprintf("combined/combined_%s.rds", cat), compress=FALSE)
}

# -------
# YUCHENG
# -------

setwd(raw_loc)
setwd("Yucheng_New")

yyang_filenames <- list(
  "gene_expression.lnc.cufflinks_output.reformat",
  "gene_expression.pc.cufflinks_output.reformat",
  "gene_expression.pg.cufflinks_output.reformat" 
)

yyang_compile <- my_empty_list()

for (i in 1:3)
{
   
}

dog <- name_cat[11:13]
names(yyang_filenames) <- dog

for (cat in dog)
{
  setwd(raw_loc)
  filename <- yyang_filenames[[cat]]
  
  my_lists <- read_tsv_text(filename)
  data <- do.call(rbind, my_lists) %>% t() %>% r1_to_cols()
  colnames(data)[1] <- "CORNER_CORNER"
  
  # process metadata
  print(sprintf("Processing metadata for %s", cat))
  order <- create_order(data, c("SAMPLE", "TISSUE", "INDIVIDUAL"))
  order[,1] <- data[,1]
  
  # delete samples row in data and convert to numeric
  data <- convert_to_num(data[,-1])
  print(ncol(data))
  
  for (j in 1:nrow(order))
  {
    words <- strsplit(order[j,1], ".ENC")[[1]]
    
    words <- repStr(
      words, 
      c("_", "andrenal", "pancrease", 
        "centricle", "peyers", "Peyers"),
      c(" ", "adrenal", "pancreas",
        "ventricle", "Peyer's", "Peyer's")
    )
    
    order[j, 3] <- words[2]
    order[j, 2] <- words[1]
  }
  
  order_total[[cat]] <- order
  
  # save data
  setwd(pro_loc)
  saveRDS(data, sprintf("combined/combined_%s.rds", cat), compress=FALSE)
}

# -------------------------------
# Proteomics, FPKM/TPM, OMS Reads
# -------------------------------

filename <- "Entex_RNAvsProtein_data.txt"
setwd(raw_loc)

print("Reading proteomics data ...")
my_lists <- read_tsv_text(filename)
data <- do.call(rbind, my_lists) %>% t() %>% r1_to_cols()
colnames(data)[1] <- "CORNER_CORNER"

# # dropped - not samples
# gene_uniprot <- data[1:2,]
# KEEP THESE FEATURES
ft <- data[3:74,]
pd <- data[75:94,]
oms <- data[95:104,]
# # dropped - not samples
# counts <- data[105:117,]
# means <- data[118:119,]
# # dropped due to normalization feature
# ft_scaled <- data[120:127,]
# pd_scaled <- data[128:135,]
# # dropped - not samples
# correlation <- data[136,,drop=FALSE]

# ----------------------------------
# Proteomics, FPKM/TPM, OMS Metadata
# ----------------------------------

setwd(com_loc)

# process metadata
cat <- name_cat[13]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  pd, 
  c("SAMPLE", "TISSUE", "INDIVIDUAL", "EXPRESSION"))
order[,1] <- pd[,1]

# delete samples row in data and convert to numeric
pd <- convert_to_num(pd[,-1])
print(ncol(pd))

for (i in 1:nrow(order))
{
  words <- strsplit(order[i,1], ".", fixed=TRUE)[[1]]
  order[i, 4] <- words[1]
  order[i, 3] <- words[3]
  order[i, 2] <- words[2]
}

order_total[[cat]] <- order
myRDS(sprintf("combined_%s.rds", cat), pd)

# process metadata
cat <- name_cat[14]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  ft, 
  c("SAMPLE", "TISSUE", "INDIVIDUAL", "EXPRESSION", "EXP_COEFFICIENT"))
order[,1] <- ft[,1]

# delete samples row in data and convert to numeric
ft <- convert_to_num(ft[,-1])
print(ncol(ft))

for (i in 1:nrow(order))
{
  words <- strsplit(order[i,1], ".", fixed=TRUE)[[1]]
  ec <- strsplit(words[1], "-", fixed=TRUE)[[1]]
  order[i, 5] <- ec[1]
  order[i, 4] <- ec[2]
  order[i, 3] <- words[3]
  order[i, 2] <- words[2]
}

order_total[[cat]] <- order
myRDS(sprintf("combined_%s.rds", cat), ft)

# process metadata
cat <- name_cat[15]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  oms, 
  c("SAMPLE", "TISSUE", "INDIVIDUAL"))
order[,1] <- oms[,1]

# delete samples row in data and convert to numeric
oms <- convert_to_num(oms[,-1])
print(ncol(oms))

for (i in 1:nrow(order))
{
  words <- strsplit(order[i,1], ".", fixed=TRUE)[[1]]
  order[i, 3] <- words[3]
  order[i, 2] <- words[2]
}

order_total[[cat]] <- order
myRDS(sprintf("combined_%s.rds", cat), oms)

# -----------
# DECORATIONS
# -----------

combined_H3K27ac <- readRDS("combined_H3K27ac.rds")
ref <- colnames(combined_H3K27ac)
rm(combined_H3K27ac)

setwd(sprintf("%s/cCRE_ID", raw_loc))
file_names <- list.files()
all_decorations <- my_empty_list(file_names)

for (file in file_names)
  all_decorations[[file]] <- do.call(rbind, read_tsv_text(file)) %>% r1_to_cols()

formal_names <- c(
  "Active_CTCF",
  "Active_Distal",
  "Active_nonCTCF",
  "Active_Proximal",
  "Active_Total",
  "Bivalent_CTCF",
  "Bivalent_Proximal",
  "Bivalent_Total",
  "Repressive_CTCF",
  "Repressive_Distal",
  "Repressive_nonCTCF",
  "Repressive_Proximal",
  "Repressive_Total"
)

names(all_decorations) <- formal_names

clean_decor <- my_empty_list(formal_names)
for (file in formal_names)
  clean_decor[[file]] <- all_decorations[[file]][,1] %>% EH38D_removal()

for (file in formal_names)
  all_decorations[[file]] <- all_decorations[[file]][,-1]
for (file in formal_names)
  all_decorations[[file]] <- apply(all_decorations[[file]], 1:2, as.numeric)

ind_decor <- my_empty_list(formal_names)
for (file in formal_names)
  ind_decor[[file]] <- which(ref %in% (clean_decor[[file]]))
rm(clean_decor)
ind_decor <- c("Reference"=list(ref), ind_decor)

matrix_and <- function(ind1, ind2, all1, all2)
{
  indices <- intersect(ind1, ind2)
  all1[which(ind1 %in% indices),]*all2[which(ind2 %in% indices),]
}

# active
ind_decor$Active_Proximal_CTCF <- intersect(
  ind_decor$Active_Proximal, ind_decor$Active_CTCF
)

all_decorations$Active_Proximal_CTCF <- matrix_and(
  ind_decor$Active_Proximal, ind_decor$Active_CTCF,
  all_decorations$Active_Proximal, all_decorations$Active_CTCF
)

ind_decor$Active_Proximal_nonCTCF <- intersect(
  ind_decor$Active_Proximal, ind_decor$Active_nonCTCF
)

all_decorations$Active_Proximal_nonCTCF <- matrix_and(
  ind_decor$Active_Proximal, ind_decor$Active_nonCTCF,
  all_decorations$Active_Proximal, all_decorations$Active_nonCTCF
)

ind_decor$Active_Distal_CTCF <- intersect(
  ind_decor$Active_Distal, ind_decor$Active_CTCF
)

all_decorations$Active_Distal_CTCF <- matrix_and(
  ind_decor$Active_Distal, ind_decor$Active_CTCF,
  all_decorations$Active_Distal, all_decorations$Active_CTCF
)

ind_decor$Active_Distal_nonCTCF <- intersect(
  ind_decor$Active_Distal, ind_decor$Active_nonCTCF
)

all_decorations$Active_Distal_nonCTCF <- matrix_and(
  ind_decor$Active_Distal, ind_decor$Active_nonCTCF,
  all_decorations$Active_Distal, all_decorations$Active_nonCTCF
)

# repressive
ind_decor$Repressive_Proximal_CTCF <- intersect(
  ind_decor$Repressive_Proximal, ind_decor$Repressive_CTCF
)

all_decorations$Repressive_Proximal_CTCF <- matrix_and(
  ind_decor$Repressive_Proximal, ind_decor$Repressive_CTCF,
  all_decorations$Repressive_Proximal, all_decorations$Repressive_CTCF
)

ind_decor$Repressive_Proximal_nonCTCF <- intersect(
  ind_decor$Repressive_Proximal, ind_decor$Repressive_nonCTCF
)

all_decorations$Repressive_Proximal_nonCTCF <- matrix_and(
  ind_decor$Repressive_Proximal, ind_decor$Repressive_nonCTCF,
  all_decorations$Repressive_Proximal, all_decorations$Repressive_nonCTCF
)

ind_decor$Repressive_Distal_CTCF <- intersect(
  ind_decor$Repressive_Distal, ind_decor$Repressive_CTCF
)

all_decorations$Repressive_Distal_CTCF <- matrix_and(
  ind_decor$Repressive_Distal, ind_decor$Repressive_CTCF,
  all_decorations$Repressive_Distal, all_decorations$Repressive_CTCF
)

ind_decor$Repressive_Distal_nonCTCF <- intersect(
  ind_decor$Repressive_Distal, ind_decor$Repressive_nonCTCF
)

all_decorations$Repressive_Distal_nonCTCF <- matrix_and(
  ind_decor$Repressive_Distal, ind_decor$Repressive_nonCTCF,
  all_decorations$Repressive_Distal, all_decorations$Repressive_nonCTCF
)

# bivalent
ind_decor$Bivalent_Proximal_CTCF <- intersect(
  ind_decor$Bivalent_Proximal, ind_decor$Bivalent_CTCF
)

all_decorations$Bivalent_Proximal_CTCF <- matrix_and(
  ind_decor$Bivalent_Proximal, ind_decor$Bivalent_CTCF,
  all_decorations$Bivalent_Proximal, all_decorations$Bivalent_CTCF
)

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

# Continuing ...
myRDS("all_decorations.rds", all_decorations)
setwd(sprintf("%s/decorations_app/data", roo_loc))rm()
for (dec in names(all_decorations))
{
  compressed <- all_decorations[[dec]] %>% Matrix(sparse=TRUE)
  myRDS(sprintf("%s.rds", dec), 
        list("TISSUE"=compressed)
  )
}

decorations <- list(
  "cCREs"=list(
    "Categories"=names(categories_full$cCREs),
    "Subsets"=ind_decor
  )
)

setwd(sprintf("%s/decorations_app/Dependencies", roo_loc))
myRDS("decorations.rds", decorations)
feat_counts <- my_empty_list(names(all_decorations))
for (dec in names(all_decorations))
  feat_counts[[dec]] <- nrow(all_decorations[[dec]])
sub_cat <- list("Decorations"=feat_counts)
myRDS("categories_full.rds", sub_cat)

order <- data.frame(matrix(colnames(all_decorations[[1]]), ncol=1))
colnames(order) <- "TISSUE"

new_order <- my_empty_list(names(all_decorations))
for (dec in names(all_decorations))
  new_order[[dec]] <- order
myRDS("order_total.rds", new_order)

custom_color_scales <- list(
  "TISSUE"=list(
    'skeletal muscle tissue'='#7A67EE',
    'gastrocnemius medialis'='#7A67EE',
    'stomach'='#FFD39B',
    'transverse colon'='#EEC591',
    "Peyer's patch"='#CDB79E',
    'ileum'='#CDB79E',
    'right lobe of liver'='#CDB79E',
    'body of pancreas'='#CD9B1D',
    'esophagus squamous epithelium'='#CDAA7D',
    'gastroesophageal sphincter'='#CDAA7D',
    'sigmoid colon'='#CDB79E',
    'esophagus muscularis mucosa'='#8B7355',
    'muscle layer of esophagus'='#8B7355',
    'esophagus mucosa'='#8B7355',
    'esophagogastric junction'='#8B7355',
    'testis'='#A6A6A6',
    'uterus'='#EED5D2',
    'prostate'='#D9D9D9',
    'prostate gland'='#D9D9D9',
    'vagina'='#EED5D2',
    'ovary'='#FFB6C1',
    'thyroid gland'='#008B45',
    'adrenal gland'='#8FBC8F',
    'spleen'='#CDB79E',
    'right atrium auricular region'='#B452CD',
    'right cardiac atrium'='#B452CD',
    'heart left ventricle'='#7A378B',
    'thoracic aorta'='#8B1C62',
    'ascending aorta'='#8B1C62',
    'coronary artery'='#EE6A50',
    'tibial artery'='#FF0000',
    'omental fat pad'='#FFA54F',
    'subcutaneous adipose tissue'='#FFA54F',
    'tibial nerve'='#FFD700',
    'suprapubic skin'='#3A5FCD',
    'lower leg skin'='#1E90FF',
    'mammary gland'='#00CDCD',
    'breast epithelium'='#00CDCD',
    'upper lobe of left lung'='#9ACD32'
  )
)

for (cat in name_cat)
{
  order_total[[cat]]$INDIVIDUAL <- repStr(
    order_total[[cat]]$INDIVIDUAL,
    c("001", "002", "003", "004"),
    c("1", "2", "3", "4"))
}

# -----------------
# SAVE DEPENDENCIES
# -----------------

setwd(dep_loc)

# upload Sets
setwd(dat_loc)
setwd("Sets")
filenames <- list.files()

for (file in filenames)
{
  data <- myRDS(file)
  save_db(data, sprintf("Sets/%s", file))
}

myRDS("decorations.rds", decorations)
myRDS("categories_full.rds", categories_full)
myRDS("order_total.rds", order_total)

amazon_keys <- c("AKIAVI2HZGPON64RUYYJ",
                 "1AE4Jlbrp0Sfuq8Ew1gYNFkWOaqgrDVVvCJqCz8b",
                 "shiny-app-data-justin")
app_title <- "Dimensionality Reduction Plotting Tool for the ENTEx Project"
app_citations <- 
  "<u>ENCODE Paper 1:</u> ENCODE Project Consortium. An integrated encyclopedia of 
DNA elements in the human genome. Nature. 2012;489(7414):57-74. 
<a href=\"doi:10.1038/nature11247\" target=\"_blank\">
doi:10.1038/nature11247</a>
<br>
<u>ENCODE Paper 2:</u> Davis CA, Hitz BC, Sloan CA, et al. The Encyclopedia of 
DNA elements (ENCODE): data portal update. Nucleic Acids Res. 2018;46(D1):D794-D801. 
<a href=\"doi:10.1093/nar/gkx1081\" target=\"_blank\">
doi:10.1093/nar/gkx1081</a>
<br><br>
In addition, the ENCODE Consortium and several ENCODE production laboratories
graciously generated these datasets." 
perplexity_types <- c(2, 4, 6, 12, 20)
pc_cap <- 10
user_credentials <- list("guest"=hashpw("All@2019", gensalt(12)))

myRDS("amazon_keys.rds", amazon_keys)
myRDS("app_title.rds", app_title)
myRDS("app_citations.rds", app_citations)
myRDS("perplexity_types.rds", perplexity_types)
myRDS("pc_cap.rds", pc_cap)
myRDS("user_credentials.rds", user_credentials)