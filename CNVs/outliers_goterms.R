#Autosomes
# Read in Fst results
white_common <- read.table(file = "CNVs/CNV_autosomes_CB_CL.weir.fst", header = TRUE)

#Remove NAs
white_common <- white_common %>%
  filter(!is.na(WEIR_AND_COCKERHAM_FST))

#Take the Fst values that are above the 0.99 quantile
my_threshold <- quantile(white_common$WEIR_AND_COCKERHAM_FST, 0.99, na.rm = T)
white_common$outliers <- ifelse(white_common$WEIR_AND_COCKERHAM_FST >= my_threshold, TRUE, FALSE)
head(white_common)

#Write out a table that has outliers and non-outliers
write.table(white_common, file = "CNVs/White_Common_XIX_CNV_T_F_outliers.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

#Write out a table that has only outliers 
white_common_outliers <- white_common %>% 
  dplyr::select(CHROM, POS, WEIR_AND_COCKERHAM_FST, outliers) %>% 
  filter(outliers == TRUE)

View(white_common_outliers)

write.table(white_common_outliers, file = "White_Common_XIX_CNV_T_outliers.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

