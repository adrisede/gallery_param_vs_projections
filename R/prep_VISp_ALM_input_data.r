library(scrattch.io)
library(Matrix)

load("//allen/programs/celltypes/workgroups/rnaseqanalysis/yzizhen/SmartSeq_cells/V1_ALM/process_24411/select.markers.rda")

tome <- "//allen/programs/celltypes/workgroups/rnaseqanalysis/shiny/tomes/facs/mouse_V1_ALM_20180520/transcrip.tome"

# Load annotations
anno <- read_tome_anno(tome)

anno <- anno %>%
  # Remove outlier clusters
  filter(cluster_id %in% 1:133)

all_samples <- read_tome_sample_names(tome)
all_genes <- read_tome_gene_names(tome)


# Load count data
counts <- read_tome_dgCMatrix(tome, "/data/exon")
rownames(counts) <- all_samples
colnames(counts) <- all_genes

# Filter for markers
counts <- counts[, select.markers]

counts <- t(counts)

# Filter for samples
counts <- counts[,anno$sample_name]

# log transform
norm_counts <- log2(counts + 1)

# convert to matrix for projection input
norm_counts <- as.matrix(norm_counts)
