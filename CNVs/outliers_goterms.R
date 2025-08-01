###Outliers
####Autosomes. Repeat for X and Y
# Read in Fst results
white_common <- read.table(file = "CNV_autosomes_CB_CL.weir.fst", header = TRUE)

#Remove NAs
white_common <- white_common %>%
  filter(!is.na(WEIR_AND_COCKERHAM_FST))

#Take the Fst values that are above the 0.99 quantile
my_threshold <- quantile(white_common$WEIR_AND_COCKERHAM_FST, 0.99, na.rm = T)
white_common$outliers <- ifelse(white_common$WEIR_AND_COCKERHAM_FST >= my_threshold, TRUE, FALSE)
head(white_common)

#Write out a table that has outliers and non-outliers
write.table(white_common, file = "CNVs/White_Common_CNV_T_F_outliers.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

#Write out a table that has only outliers 
white_common_outliers <- white_common %>% 
  dplyr::select(CHROM, POS, WEIR_AND_COCKERHAM_FST, outliers) %>% 
  filter(outliers == TRUE)

View(white_common_outliers)

write.table(white_common_outliers, file = "White_Common_CNV_T_outliers.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

######################
#Calcualte outlier intervals
#Load
library(tidyverse)
library("rtracklayer")

outlier_intervals <- cnv_outliers %>%
  filter(!is.na(WEIR_AND_COCKERHAM_FST)) %>%
  select(CHROM, POS, WEIR_AND_COCKERHAM_FST) %>%
  mutate(start = POS - 100, end = POS + 100) %>%
  mutate(strand = NA) %>%
  select(CHROM, start, end)

outlier_intervals

names(outlier_intervals) <- c("chrom", "start", "end")
write.table(outlier_intervals, "CNV_outlier_regions.bed", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

outlier_intervals_snp <- cnv_outliers %>%
  filter(!is.na(WEIR_AND_COCKERHAM_FST)) %>%
  select(CHROM, POS, WEIR_AND_COCKERHAM_FST) 

# Read in the interval list file
interval_list_file <- "CNV_outlier_regions.bed"
interval_list <- import(interval_list_file, format = "bed")

# Read in the gene annotation file
gene_annotation_file <- "stickleback_v5_ensembl_genes.gff3.gz"
gene_annotation <- import(gene_annotation_file, format = "gff")

# Convert the interval list to a GRanges object
interval_list_granges <- as(interval_list, "GRanges")

# Convert the gene annotation to a GRanges object
gene_annotation_granges <- as(gene_annotation, "GRanges")

# Find the genes that intersect the intervals
intersecting_genes <- findOverlaps(interval_list_granges, gene_annotation_granges, type = "any", select = "all", ignore.strand = TRUE)
intersecting_genes <- data.frame(intersecting_genes)

match_subject_query_info <- function(intersect_df_row){
  
  subject_index <- intersecting_genes[intersect_df_row,1]
  query_index <- intersecting_genes[intersect_df_row,2]
  
  subject_dat <- outlier_intervals_snp[subject_index,]
  query_dat <- gene_annotation_granges[query_index] %>% data.frame %>% select(source, ID, Name)
  
  out_dat <- cbind(subject_dat, query_dat)
  names(out_dat) <- c("chrom", "pos", "fst", "gene_source", "gene_ID", "gene_name")
  out_dat
  
}

hits_df <- lapply(1:nrow(intersecting_genes), match_subject_query_info)
hits_df <- bind_rows(hits_df)

hits_df <- hits_df %>%
  mutate(browser_coords = paste0(chrom, ":", pos-1000, "..", pos +1000)) %>%
  arrange(chrom, pos)

hits_df %>%
  filter(!is.na(gene_name)) %>%
  group_by(gene_name, gene_ID) %>%
  View

write.table(hits_df, "CNV_autosome_hits_raw.txt", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
view(hits_df)


hits_df_summary <- hits_df %>%
  filter(!is.na(gene_name)) %>%
  group_by(gene_name, gene_ID) %>%
  summarize(n_snps = n(), avg_fst = mean(fst, na.rm = TRUE)) 

hits_df_summary
write.table(hits_df_summary, "CNV_autosome_hits_summary.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
view(hits_df_summary)

#################
#Get GO terms for CNV outliers from biomaRt
library(tidyverse)
library(biomaRt)

#Input ensembl IDs from hits list. Gene names are from stickleback v5 assembly.
ensembl.ids<-read.table("CNV_autosome_hits_summary.txt", header = FALSE)
cols<-c("gene_name", "gene_ID", "n_snps", "avg_fst")
colnames(ensembl.ids) <-cols
#ensembl.ids <- ensembl.ids %>%
#rename("gene_ID" = GeneID)

View(ensembl.ids)

#Set up mart. There are many archived ensembl databases. Select GAAC dataset
listEnsemblArchives()
ensembl.con <- useMart("ensembl", host="https://jan2024.archive.ensembl.org", dataset="gaculeatus_gene_ensembl")

#Look at different filters and attributes
atts <- listAttributes(ensembl.con)
filters<-listFilters(ensembl.con)

#Get the GO terms. Set atrributes, filters, and values.
Goterms <- getBM(attributes = c('chromosome_name', 'start_position', 'end_position', 'ensembl_gene_id', 'external_gene_name', 'go_id','name_1006', 'definition_1006'),
                 filters = "ensembl_gene_id", 
                 values = ensembl.ids$gene_ID,
                 mart = ensembl.con)

#Merge the goterms table with the hits table
view(Goterms)
Goterms <- Goterms %>%
  rename(gene_ID = ensembl_gene_id) 

hits_goterms <- left_join(Goterms, ensembl.ids, by = "gene_ID")


