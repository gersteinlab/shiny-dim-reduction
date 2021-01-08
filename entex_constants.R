# The purpose of this file is to store long texts.

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
    "Protein_Coding_Genes"=17201,
    "Pseudogenes"=2542,
    "Long_Non_Coding_RNAs"=9571,
    "RAMPAGE"=148246
  ),
  "Proteomics"=list(
    "Peptide"=9411,
    "FPKM_TPM"=19377,
    "OMS"=6558
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

app_title <- "Dimensionality Reduction Plotting Tool for the ENTEx Project"
app_citations <- "<u>ENCODE Paper 1:</u> ENCODE Project Consortium. 
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