# The goal of this script is to convert raw ENTEx data
# into numeric data and metadata

project_name <- "ENTEx"
source("~/Justin-Tool/shiny-dim-reduction/converter.R")

# -----------------------
# LIBRARIES AND FUNCTIONS
# -----------------------

library(Matrix)

# creates the order data frame
create_order <- function(num_rows, colnames)
{
  order <- matrix(ncol = length(colnames), nrow = num_rows)
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

# apply to colnames of Yucheng's data to remove EH38E
EH38E_removal <- function(char_vec)
{
  splits <- substr(char_vec, start = 1, stop = 5)
  if (length(which(splits == "EH38E")) == length(char_vec))
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
    "Protein_Coding_Genes"=9571,
    "Pseudogenes"=17201,
    "Long Non-Coding RNAs"=2542
  ),
  "Proteomics"=list(
    "Peptide"=9411,
    "FPKM_TPM"=19377,
    "OMS"=6558
  )
)

init_cat(categories_full)

# -----
# FABIO
# -----

fabio_filenames <- list(
  "Fabio/k27ac.normalized.FN_JR.txt",
  "Fabio/raw.average.K27me3.table.quantile.scaled.txt",
  "Fabio/raw.average.k4me1.table.quantile.scaled.txt",
  "Fabio/raw.average.k4me3.table.quantile.scaled.txt",
  "Fabio/raw.average.K9me3.table.quantile.scaled.txt",
  "Fabio/raw.average.ATAC.table.quantile.scaled.txt",
  "Fabio/raw.average.CTCF.table.quantile.scaled.txt",
  "Fabio/raw.average.DNase.table.quantile.scaled.txt",
  "Fabio/raw.average.POLR2A.table.quantile.scaled.txt",
  "Fabio/raw.average.methylation.table.quantile.scaled.txt"
)

dog <- name_cat[1:10]
names(fabio_filenames) <- dog

for (cat in dog)
{
  filename <- fabio_filenames[[cat]]
  
  # read data
  setwd(raw_loc)
  print(sprintf("Reading %s for %s", filename, cat))
  begin <- my_timer()
  my_lists <- read_tsv_text(filename)
  print(sprintf("Seconds elapsed: %s", my_timer(begin)))
  begin <- my_timer()
  my_colnames <- c("CORNER_CORNER", EH38D_removal(my_lists[[1]]))
  my_lists[[1]] <- NULL
  data <- do.call(rbind, my_lists)
  rm(my_lists)
  colnames(data) <- my_colnames
  print(sprintf("Seconds elapsed: %s", my_timer(begin)))
  
  # process metadata
  print(sprintf("Processing metadata for %s", cat))
  begin <- my_timer()
  order <- create_order(nrow(data), c("SAMPLE", "TISSUE", "INDIVIDUAL"))
  order[,1] <- data[,1]
  
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
  print(sprintf("Seconds elapsed: %s", my_timer(begin)))
  
  # # delete samples row in data and convert to numeric
  # print(sprintf("Converting numerical data for %s", cat))
  # begin <- my_timer()
  # data <- convert_to_num(data[,-1])
  # print(ncol(data))
  # 
  # # save data
  # setwd(pro_loc)
  # saveRDS(data, sprintf("combined/combined_%s.rds", cat), compress=FALSE)
  # print(sprintf("Seconds elapsed: %s", my_timer(begin)))
}

# -------
# YUCHENG
# -------

yyang_folder_names <- list(
  "Yucheng/gene_expression.lnc.cufflinks_output.reformat",
  "Yucheng/gene_expression.pc.cufflinks_output.reformat",
  "Yucheng/gene_expression.pg.cufflinks_output.reformat" 
)

dog <- name_cat[11:13]
names(yyang_folder_names) <- dog

for (cat in dog)
{
  setwd(raw_loc)
  folder <- yyang_folder_names[[cat]]
  print(sprintf("Reading %s for %s", folder, cat))
  begin <- my_timer()
  folder_entries <- my_empty_list(list.files(folder))
  
  for (file in names(folder_entries))
  {
    folder_entries[[file]] <- sprintf("%s/%s", folder, file) %>% 
      read_tsv_text() %>% do.call(rbind, .)
  }
  
  rows <- lapply(folder_entries, function(x){x[,2]})
  data <- do.call(rbind, rows)
  colnames(data) <- folder_entries[[1]][,1]
  row_names <- repStr(rownames(data), ".txt", "")
  rownames(data) <- NULL
  print(sprintf("Seconds elapsed: %s", my_timer(begin)))
  
  print(sprintf("Processing metadata for %s", cat))
  begin <- my_timer()
  order <- create_order(length(row_names), c("SAMPLE", "TISSUE", "INDIVIDUAL"))
  order[,1] <- row_names
  
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
  
  # convert to numeric
  print(sprintf("Converting numerical data for %s", cat))
  begin <- my_timer()
  data <- convert_to_num(data)
  print(ncol(data))
  
  # save data
  setwd(pro_loc)
  saveRDS(data, sprintf("combined/combined_%s.rds", cat), compress=FALSE)
  print(sprintf("Seconds elapsed: %s", my_timer(begin)))
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

setwd(pro_loc)

# process metadata
cat <- name_cat[14]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  nrow(pd), 
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
saveRDS(pd, sprintf("combined/combined_%s.rds", cat))

# process metadata
cat <- name_cat[15]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  nrow(ft), 
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
saveRDS(ft, sprintf("combined/combined_%s.rds", cat))

# process metadata
cat <- name_cat[16]
print(sprintf("Processing metadata for %s", cat))
order <- create_order(
  nrow(oms), 
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
saveRDS(oms, sprintf("combined/combined_%s.rds", cat))

for (cat in name_cat)
{
  order_total[[cat]]$INDIVIDUAL <- repStr(
    order_total[[cat]]$INDIVIDUAL,
    c("001", "002", "003", "004"),
    c("1", "2", "3", "4"))
}

# -----------
# DECORATIONS
# -----------

setwd(raw_loc)
ED_mapping <- read_tsv_text("GRCh38-ccREs.bed")
ED_mapping <- do.call(rbind, ED_mapping)
input_E <- ED_mapping[,5] %>% EH38E_removal()
output_D <- ED_mapping[,4] %>% EH38D_removal()

ED_map <- function(vec_x)
{
  output_D[input_E %in% vec_x]
}

setwd(raw_loc)
setwd("allele_specific")

folder_list <- list.files()
folder_full <- my_empty_list(folder_list)
union_full <- folder_full

for (folder in folder_list)
{
  file_list <- list.files(folder)
  all_stuff <- my_empty_list(file_list)
  
  for (file in file_list)
  {
    all_stuff[[file]] <- read_tsv_text(sprintf("%s/%s", folder, file)) %>% 
      unlist() %>% EH38E_removal() %>% ED_map()
  }
  
  union <- NULL
  for (file in file_list)
  {
    union <- base::union(union, all_stuff[[file]])
  }
  
  print(length(union))
  
  folder_full[[folder]] <- all_stuff
  union_full[[folder]] <- union
}

setwd(pro_loc)
combined_H3K27ac <- readRDS("combined/combined_H3K27ac.rds")
ref <- colnames(combined_H3K27ac)
rm(combined_H3K27ac)

setwd(sprintf("%s/Decorations", raw_loc))
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

# allele specific
ind_decor$Active_Distal_CTCF_AS <- which(
  ref %in% union_full$active.distal.CTCF.AS)
ind_decor$Active_Distal_CTCF_nonAS <- which(
  ref %in% union_full$active.distal.CTCF.nonAS)
ind_decor$Active_Distal_nonCTCF_AS <- which(
  ref %in% union_full$active.distal.nonCTCF.AS)
ind_decor$Active_Distal_nonCTCF_nonAS <- which(
  ref %in% union_full$active.distal.nonCTCF.nonAS)
ind_decor$Active_Proximal_CTCF_AS <- which(
  ref %in% union_full$active.proximal.CTCF.AS)
ind_decor$Active_Proximal_CTCF_nonAS <- which(
  ref %in% union_full$active.proximal.CTCF.nonAS)
ind_decor$Active_Proximal_nonCTCF_AS <- which(
  ref %in% union_full$active.proximal.nonCTCF.AS)
ind_decor$Active_Proximal_nonCTCF_nonAS <- which(
  ref %in% union_full$active.proximal.nonCTCF.nonAS)

ind_decor_order <- ind_decor[c(1,
                               6,9,14,
                               5,3,2,4,
                               8,7,
                               13,11,10,12, 
                               15:30)]

decorations <- list(
  "cCREs"=list(
    "Categories"=names(categories_full$cCREs),
    "Subsets"=ind_decor_order
  )
)

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

# -----------------
# SAVE DEPENDENCIES
# -----------------

setwd(dep_loc)

self_save("decorations")
self_save("categories_full")
self_save("order_total")

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
user_credentials <- list("guest" = my_hash("All@2019")) 

self_save("amazon_keys")
self_save("app_title")
self_save("app_citations")
self_save("perplexity_types")
self_save("pc_cap")
self_save("user_credentials")
self_save("custom_color_scales")