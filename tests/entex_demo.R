# The goal of this script is to convert raw ENTEx data
# into numeric data and metadata.

# ---------------
# MANAGE APP DATA
# ---------------

source("app/preprocess.R")

app_data$title <- "Dimensionality Reduction Plotting Tool for the ENTEx Project"
app_data$citations <- "<u>ENCODE Paper 1:</u> ENCODE Project Consortium.
An integrated encyclopedia of DNA elements in the human genome.
Nature. 2012;489(7414):57-74.
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

app_data$groups <- list(
  "cCREs" = c(
    "H3K27ac",
    "H3K27me3",
    "H3K4me1",
    "H3K4me3",
    "H3K9me3",
    "ATACseq",
    "CTCF",
    "DNase",
    "POLR2A",
    "Methylation"
  ),
  "Expression" = c(
    "Protein_Coding_Genes",
    "Pseudogenes",
    "Long_Non_Coding_RNAs",
    "RAMPAGE"
  ),
  "Proteomics (Mass Spec)" = c(
    "Peptide",
    "FPKM_TPM",
    "OMS"
  )
)

formal_dec_names <- c(
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

custom_color_scales <- list(
  "TISSUE" = c(
    'skeletal muscle tissue' = '#7A67EE',
    'gastrocnemius medialis' = '#7A67EE',
    'stomach' = '#FFD39B',
    'transverse colon' = '#EEC591',
    "Peyer's patch" = '#CDB79E',
    'ileum' = '#CDB79E',
    'right lobe of liver' = '#CDB79E',
    'body of pancreas' = '#CD9B1D',
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
    'tibial nerve' = '#FFD700',
    'suprapubic skin' = '#3A5FCD',
    'lower leg skin' = '#1E90FF',
    'mammary gland' = '#00CDCD',
    'breast epithelium' = '#00CDCD',
    'upper lobe of left lung' = '#9ACD32'
  )
)

library(Matrix)

dec_pro <- sprintf("%s/decorations", pro_loc)
if (!dir.exists(dec_pro))
  dir.create(dec_pro)

# ---------
# FUNCTIONS
# ---------

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
  "Yucheng/gene_expression.pc.cufflinks_output.reformat",
  "Yucheng/gene_expression.pg.cufflinks_output.reformat",
  "Yucheng/gene_expression.lnc.cufflinks_output.reformat"
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

# -------
# RAMPAGE
# -------

setwd(raw_loc)
rampage_order <- read_tsv_text("metadata_20210106.tsv")
rampage_order[[1]] <- rampage_order[[1]][1:54]
rampage_order <- do.call(rbind, rampage_order) %>% r1_to_cols()

trunc_names <- repStr(list.files("RAMPAGE"), ".tsv", "")
rampage_order <- data.frame(rampage_order[rampage_order[,1] %in% trunc_names,])
good <- apply(rampage_order, 2, function(x){length(unique(x))}) %>% unlist()
rampage_order <- rampage_order[,good > 1]

rampage_list <- my_empty_list(trunc_names)

for (trunc in trunc_names)
{
  listed <- read_tsv_text(sprintf("RAMPAGE/%s.tsv", trunc))
  rampage_list[[trunc]] <- do.call(rbind, listed)[,c(4, 7)]
}

column_names <- rampage_list[[1]][,1]
rampage_mat <- rampage_list
for (trunc in trunc_names)
  rampage_mat[[trunc]] <- rampage_mat[[trunc]][,2]
rampage_mat <- do.call(rbind, rampage_mat)
rampage_mat <- rampage_mat[rampage_order[,1],]
colnames(rampage_mat) <- column_names
rownames(rampage_mat) <- NULL
rampage_com <- convert_to_num(rampage_mat)

setwd(pro_loc)
cat <- name_cat[14]
saveRDS(rampage_com, sprintf("combined/combined_%s.rds", cat))
order_total[[cat]] <- rampage_order[,c(1:2, 4:6)]
colnames(order_total[[cat]]) <- c("FILE_ACCESSION", "EXPERIMENT_ACCESSION",
                                  "TISSUE", "DATE_RELEASED", "TECHNICAL_REPLICATE")

# ----------------------------------
# Proteomics, FPKM/TPM, OMS Metadata
# ----------------------------------

setwd(pro_loc)

# process metadata
cat <- name_cat[15]
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
cat <- name_cat[16]
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
cat <- name_cat[17]
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

# ------------------
# DECORATIONS PART 1
# ------------------

setwd(raw_loc)
ED_mapping <- do.call(rbind, read_tsv_text("GRCh38-ccREs.bed"))
input_E <- ED_mapping[,5] %>% EH38E_removal()
output_D <- ED_mapping[,4] %>% EH38D_removal()

ED_map <- function(vec_x)
{
  output_D[input_E %in% vec_x]
}

setwd(sprintf("%s/allele_specific", raw_loc))

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

setwd(sprintf("%s/decorations", raw_loc))
file_names <- list.files()
all_decorations <- my_empty_list(file_names)

for (file in file_names)
  all_decorations[[file]] <- do.call(rbind, read_tsv_text(file)) %>% r1_to_cols()

names(all_decorations) <- formal_dec_names

# extract all row names and clean them
clean_decor <- my_empty_list(formal_dec_names)
for (file in formal_dec_names)
  clean_decor[[file]] <- all_decorations[[file]][,1] %>% EH38D_removal()

# convert rest of the matrices to numeric
for (file in formal_dec_names)
  all_decorations[[file]] <- apply(all_decorations[[file]][,-1], 1:2, as.numeric)

# save
setwd(dec_pro)
self_save("all_decorations")
self_save("clean_decor")
self_save("folder_full")
self_save("union_full")
self_save("ref")

# ------------------
# DECORATIONS PART 2
# ------------------

# recovery
setwd(sprintf("%s/decorations", pro_loc))
self_load("all_decorations")
self_load("clean_decor")
self_load("folder_full")
self_load("union_full")
self_load("ref")

ind_decor <- my_empty_list(formal_dec_names)
for (file in formal_dec_names)
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

# -----------------
# SAVE DEPENDENCIES
# -----------------

setwd(dep_loc)

self_save("decorations")
self_save("categories_full")
self_save("order_total")

perplexity_types <- c(2, 4, 6, 12, 20)
pc_cap <- 10

self_save("app_title")
self_save("app_citations")
self_save("perplexity_types")
self_save("pc_cap")
self_save("user_credentials")
self_save("custom_color_scales")

# ---------------
# YUCHENG EXAMPLE
# ---------------
setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("app_functions.R", encoding="UTF-8")

default_proc <- function(data)
{
  data %>% set_f1_f2(c(0.4,1),c(0,60)) %>% num_nan_binary()
}

dAct <- all_decorations$Active_Distal %>% default_proc()
pAct <- all_decorations$Active_Proximal %>% default_proc()
dRep <- all_decorations$Repressive_Distal %>% default_proc()
pRep <- all_decorations$Repressive_Proximal %>% default_proc()

timur <- c("body_of_pancreas",
           "upper_lobe_of_left_lung",
           "ascending_aorta",
           "thyroid_gland",
           "tibial_nerve",
           "heart_left_ventricle",
           "stomach",
           "right_atrium_auricular_region",
           "esophagus_muscularis_mucosa",
           "sigmoid_colon",
           "gastroesophageal_sphincter",
           "esophagus_squamous_epithelium",
           "breast_epithelium",
           "adrenal_gland",
           "Peyers_patch",
           "vagina",
           "coronary_artery",
           "transverse_colon",
           "gastrocnemius_medialis",
           "uterus",
           "right_lobe_of_liver",
           "spleen")

dAct_cols <- c(timur, setdiff(colnames(dAct), timur))
safeDAct <- dAct[,timur]
longDAct <- dAct[,dAct_cols]

safedActPlot <- safeDAct %>% upset_custom(50, 0.5, TRUE, "gray23", "gray88")
longdActPlot <- longDAct %>% upset_custom(50, 0.5, TRUE, "gray23", "gray88")

setwd(dec_pro)

c("safedActPlot", "longdActPlot") %>% self_save()

pdf(file = "safedActPlot.pdf", width = 16, height = 8)
safedActPlot
dev.off()

pdf(file = "longdActPlot.pdf", width = 16, height = 8)
longdActPlot
dev.off()

protein <- readRDS(sprintf(
  "%s/Sets/Sets-1_Logarithmic_Protein_Coding_Genes.rds",
  pro_loc
))$TISSUE

hmm <- protein %>% set_f1_f2(c(0.25,1),c(0,60)) %>% num_nan_binary()
colnames(hmm) <- repStr(colnames(hmm), ".", "_")

timur_2 <- timur[timur %in% colnames(hmm)]
safeProt <- hmm[,timur_2]
prot_cols <- c(timur_2, setdiff(colnames(hmm), timur_2))
longProt <- hmm[,prot_cols]

protein_safe <- safeProt %>% upset_custom(50, 0.5, TRUE, "gray23", "gray88")
protein_long <- longProt %>% upset_custom(50, 0.5, TRUE, "gray23", "gray88")

pdf(file = "protein_safe.pdf", width = 16, height = 8)
protein_safe
dev.off()

pdf(file = "protein_long.pdf", width = 16, height = 8)
protein_long
dev.off()

pActPlot <- pAct %>% upset_custom(TRUE, 50, c(0.5, 0.5))
dRepPlot <- dRep %>% upset_custom(TRUE, 50, c(0.5, 0.5))
pRepPlot <- pRep %>% upset_custom(TRUE, 50, c(0.5, 0.5))

# save
c("dAct", "dActPlot", "pAct", "pActPlot",
  "dRep", "dRepPlot", "pRep", "pRepPlot") %>% self_save()

# recover
setwd(dec_pro)
c("dAct", "dActPlot", "pAct", "pActPlot",
  "dRep", "dRepPlot", "pRep", "pRepPlot") %>% self_load()


upset_plot = function (df, ncols=5, order, Nintersects, keeporder=FALSE){

  p = upset(df,
            nsets=ncols,  # number of tissues
            nintersects = Nintersects, # number of bars in x
            point.size = 4.5,
            line.size = 2,
            number.angles = 0,
            mainbar.y.label = Ylabel,
            sets.x.label = Xlabel,
            order.by = order,
            mb.ratio = c(0.5, 0.5),
            #              keep.order=keeporder, # doesn't work?
            text.scale = 1.8,
  )
  return(p)
}

upset_revised <- function(data, Xlabel, Ylabel, nint=200, order="freq", keep_order=TRUE)
{
  upset(data,
        sets = rev(colnames(data)),
        sets.x.label = Xlabel,
        mainbar.y.label = Ylabel,
        mb.ratio = c(0.5, 0.5),
        nintersects = nint,
        order.by = order,
        keep.order=keep_order,
        point.size = 5,
        line.size = 1,
        text.scale = 2)
}

# BROADNESS
setwd(sprintf("%s/shiny-dim-reduction", Sys.getenv("SHINY_DIM_REDUCTION_ROOT")))
source("pipeline.R", encoding="UTF-8")
source("app_functions.R", encoding="UTF-8")

# a threshold parameter - can be from 1 to 11, 1 is least strict
thresh <- 2

# -----------
# COMPUTATION
# -----------

setwd(dep_loc)

# scale options
sca_options <- c("Logarithmic", "Linear")

# load files
decorations <- readRDS("decorations.rds")
order_total <- readRDS("order_total.rds")
categories_full <- readRDS("categories_full.rds")

# create categories
init_cat()

# calculate
setwd(pro_loc)
sets <- my_empty_list(names(categories))
summary <- sets

for (cat in names(categories))
{
  sets[[cat]] <- my_empty_list(sca_options)
  summary[[cat]] <- sets[[cat]]

  for (sca in sca_options)
  {
    sets[[cat]][[sca]] <- readRDS(
      sprintf("Sets/Sets-%s_%s_%s.rds", thresh, sca, cat))$TISSUE %>%
      set_f1_f2(c(0.4, 1), c(0, 60)) %>% num_nan_binary()
    summary[[cat]][[sca]] <- apply(sets[[cat]][[sca]], 1, sum)
  }
}

table <- matrix(ncol=3, nrow=length(categories)*length(sca_options))
colnames(table) <- c("Total Tissue Number",
                     "Uniquely (single tissue)",
                     "Broadly (>90% all tissues)")
rownames(table) <- 1:(length(sca_options)*num_cat)

for (cn in 1:length(categories))
{
  cat <- names(categories)[cn]
  for (sn in 1:2)
  {
    sca <- sca_options[sn]
    print(sprintf("%s %s:", cat, sca), quote=FALSE)
    ind <- 2*cn + sn - 2
    print(ind)

    numbers <- summary[[cat]][[sca]]

    rownames(table)[ind] <- sprintf("%s (%s)", cat, sca)
    table[ind, 1] <- length(numbers)
    table[ind, 2] <- sum(between(numbers, 1, 1))
    table[ind, 3] <- sum(between(numbers, 0.9*max(numbers), max(numbers))
    )
  }
}

setwd(sprintf("%s/yucheng_example", roo_loc))
write.csv(table, file="table.csv")
