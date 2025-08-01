#######Find overlap between white PBS outliers and Genes 
library(tidyverse)
library("rtracklayer")

PBS_df <- read.table("PBS/white_PBS_filtered_autosomes_noXIX.txt", header = TRUE)
head(PBS_df)

outlier_intervals <- PBS_df %>%
  filter(!is.na(white_PBS)) %>%
  select(CHROM, POS, white_PBS) %>%
  mutate(start = POS - 100, end = POS + 100) %>%
  mutate(strand = NA) %>%
  select(CHROM, start, end)

outlier_intervals

names(outlier_intervals) <- c("chrom", "start", "end")
write.table(outlier_intervals, "PBS/high_PBS_regions.bed", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

outlier_intervals_snp <- PBS_df %>%
  filter(!is.na(white_PBS)) %>%
  select(CHROM, POS, white_PBS) 

# Read in the interval list file
interval_list_file <- "PBS/high_PBS_regions.bed"
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
  names(out_dat) <- c("chrom", "pos", "PBS", "gene_source", "gene_ID", "gene_name")
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

write.table(hits_df, "PBS/PBS_hits_raw.txt", quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
view(hits_df)


hits_df_summary <- hits_df %>%
  filter(!is.na(gene_name)) %>%
  group_by(gene_name, gene_ID) %>%
  summarize(n_snps = n(), avg_PBS = mean(PBS, na.rm = TRUE)) 

hits_df_summary
write.table(hits_df_summary, "PBS/PBS_hits_summary.txt", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
view(hits_df_summary)

#################
#Get GO terms for PBS outliers from biomaRt
library(biomaRt)

#Input ensembl IDs from hits list. Gene names are from stickleback v5 assembly.
ensembl.ids<-read.table("PBS/PBS_hits_summary.txt", header = FALSE)
cols<-c("gene_name", "gene_ID", "n_snps", "avg_PBS")
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
  
  
view(hits_goterms)
write.csv(hits_goterms, "PBS/PBS_hits_autosomes_noXIX_goterms_v111.csv", row.names = FALSE)
