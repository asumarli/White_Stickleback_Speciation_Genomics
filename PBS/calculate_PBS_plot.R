#Calculate PBS and plot genome-wide PBS

library(tidyverse)
library(stringr)

#Autosomes. Repeat for X and Y. Then combine. 
# Read in pairwise Fst for each pair. Use fst to calculate T for each pair.
# First do white_common
white_common <- read.table(file = "PBS/white_common_fst.txt.weir.fst", header = TRUE)
colnames(white_common)[3] <- "white_common_fst"

white_common <- white_common %>%
  filter(!is.na(white_common_fst)) %>%
  filter(!CHROM == "chrXIX") %>%
  mutate(white_common_fst = replace(white_common_fst, white_common_fst < 0 , 0))

#Calculate T between white and common
white_common <- white_common %>%
  mutate(white_common_T = -log(1 - white_common_fst))

view(white_common)

#Repeat for white_quebec
white_quebec <- read.table(file = "PBS/white_Quebec_fst.txt.weir.fst", header = TRUE)
colnames(white_quebec)[3] <- "white_quebec_fst"

white_quebec <-white_quebec %>%
  filter(!is.na(white_quebec_fst)) %>%
  filter(!CHROM == "chrXIX") %>%
  mutate(white_quebec_fst = replace(white_quebec_fst, white_quebec_fst < 0 , 0))

#Calculate T
white_quebec <- white_quebec %>%
  mutate(white_quebec_T = -log(1 - white_quebec_fst))

#Repeat for Quebec_common
quebec_common <- read.table(file = "PBS/Quebec_common_fst.txt.weir.fst", header = TRUE)
colnames(quebec_common)[3] <- "quebec_common_fst"

quebec_common <- quebec_common %>%
  filter(!is.na(quebec_common_fst)) %>%
  filter(!CHROM == "chrXIX") %>%
  mutate(quebec_common_fst = replace(quebec_common_fst, quebec_common_fst < 0 , 0))

#Calculate T 
quebec_common <- quebec_common %>%
  mutate(quebec_common_T = -log(1 - quebec_common_fst))
view(quebec_common)

#Join each table together
fst_all <- left_join(white_common, quebec_common, by = c("CHROM","POS")) 
fst_all <- left_join(fst_all, white_quebec, by = c("CHROM","POS"))

#Calculate PBS for white GAAC
PBS_summaries <- fst_all %>%
  mutate(white_PBS = ((white_common_T+white_quebec_T)-quebec_common_T)/2) %>%
  mutate(common_PBS = ((white_common_T+quebec_common_T)-white_quebec_T)/2) %>%
  mutate(quebec_PBS = ((white_quebec_T+quebec_common_T)-white_common_T)/2)
  

#Make a subset 
PBS_sumaries_subset <- PBS_summaries %>%
  select(CHROM, POS, white_common_T, white_quebec_T,quebec_common_T, white_PBS, common_PBS, quebec_PBS)

# Set negative values to 0.
PBS <- PBS_sumaries_subset %>% 
  mutate(white_PBS = replace(white_PBS, white_PBS < 0 , 0)) %>%
  mutate(common_PBS = replace(common_PBS, common_PBS < 0 , 0)) %>%
  mutate(quebec_PBS = replace(quebec_PBS, quebec_PBS < 0 , 0))

#Take the PBS values that are above the 0.99 quantile
my_threshold <- quantile(PBS$common_PBS, 0.99, na.rm = T)
PBS$outliers <- ifelse(PBS$white_PBS >= my_threshold, TRUE, FALSE)
head(PBS)

#Write a table with white PBS outliers on the autosomes. Include non outliers.
write.table(PBS, file = "PBS/PBS_autosomes_noXIX.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

white_PBS_filtered <- white_PBS %>% 
  dplyr::select(CHROM, POS, white_PBS, outliers) %>% 
  filter(outliers == TRUE)

View(white_PBS_filtered)

#Write a table with white PBS outliers on the autosomes. Only include outliers.
write.table(white_PBS_filtered, file = "PBS/white_PBS_filtered_autosomes_noXIX.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")


##########Plot white PBS
#Reorder chromosomes
#Remove "chr" in chromosome
white_PBS$CHROM <- str_replace(white_PBS$CHROM, "chr", "")

#Reorder chromosomes
chr_nums <- c(1:21)
chr_roman <- as.roman(chr_nums)

chr_ordered <- paste0(chr_roman)
chr_ordered <- c(chr_ordered, "Un")

white_PBS_order <- white_PBS %>%
  select(CHROM, POS, white_PBS) %>%
  mutate(CHROM= factor(CHROM, levels = chr_ordered))

view(white_PBS_order)


plot <- white_PBS_order %>%
  ggplot(aes(x = POS)) +
  geom_point(aes(y = white_PBS, fill = outliers, color = outliers), size = .6)+
  scale_color_manual(values = c("TRUE"="deepskyblue4","FALSE"="darkgrey"))+
  scale_fill_manual(values = c("TRUE"="deepskyblue4","FALSE"="darkgrey"))+
  facet_grid( ~ CHROM, scales = "free", space = "free", switch = "x")+
  scale_y_continuous(limits = c(0,2)) +
  xlab("Chromosome") +
  ylab("White Population Branch Statistic") +
  theme_minimal()
  
  
plot + theme(axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           panel.spacing.x=unit(0.1, "lines"),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank(),
           strip.text = element_text(face = "bold"),
           legend.position="none")
