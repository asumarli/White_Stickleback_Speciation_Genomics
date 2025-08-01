#####
# Plot of Fst outliers, non outliers, and cross validated outliers. 
#Read in csv of cross-validated outliers
CV_outliers <- read.table(file = "CNVs/Cross_Validated_Outliers.csv", header = TRUE, sep = ",")
CV_outliers <- CV_outliers %>%
  select(CHROM, POS, WEIR_AND_COCKERHAM_FST, Outliers, Cross_Val)

# Read in outliers and non outliers from autosomes, XIX, and Y
Outliers <- read.table(file = "CNVs/White_Common_CNV_T_F_outliers.txt", header = TRUE)
Outliers_XIX <- read.table(file = "CNVs/White_Common_XIX_CNV_T_F_outliers.txt", header = TRUE)
Outliers_Y <- read.table(file = "CNVs/male_CNV_Y_outliers.txt", header = TRUE)

#Bind files
Outliers <- rbind(Outliers, Outliers_XIX, Outliers_Y)

#Only take out FALSE values
Outliers <- Outliers %>%
  filter(outliers == "FALSE") %>%
  rename("Outliers" = outliers)

# Add cross validation column saying that all are FAIL  
Outliers$Cross_Val <- "FAIL"

#Now bind the two together and plot
Outliers_CV <- rbind(Outliers, CV_outliers)
View(Outliers_CV)

#Plot fst 
###
library(stringr)
Outliers_CV$CHROM <- str_replace(Outliers_CV$CHROM, "chr", "")

#Reorder chromosomes
chr_nums <- c(1:21)
chr_roman <- as.roman(chr_nums)

chr_ordered <- paste0(chr_roman)
chr_ordered <- c(chr_ordered, "Un", "Y")

Outliers_CV_order <- Outliers_CV %>%
  mutate(CHROM= factor(CHROM, levels = chr_ordered))

view(Outliers_CV_order)
odd<- c("I", "III", "V", "VII", "IX", "XI", "XIII", "XV", "XVII", "XIX", "XXI", "Y")

Outliers_CV_order$odd <- Outliers_CV_order$CHROM %in% odd
Outliers_CV_order$odd <- str_replace(Outliers_CV_order$odd, "TRUE", "odd")
Outliers_CV_order$odd <- str_replace(Outliers_CV_order$odd, "FALSE", "even")


#Plot
plot <- Outliers_CV_order %>%
  ggplot(aes(x = POS)) +
  geom_point(aes(y = WEIR_AND_COCKERHAM_FST, fill = Cross_Val, color = odd, alpha = .8, shape = Cross_Val, size = Cross_Val))+
  scale_size_manual(values = c("FAIL"=1, "PASS"=2))+
  scale_color_manual(values = c("odd"="black","even"="darkgrey"))+
  scale_fill_manual(values = c("FAIL"=NULL, "PASS"="red"))+
  scale_shape_manual(values = c("FAIL"=19, "PASS"=21))+
  facet_grid( ~ CHROM, scales = "free", space = "free", switch = "x")+
  scale_y_continuous(limits = c(0,1)) +
  xlab("Chromosome") +
  ylab("Fst") +
  theme_classic()


plot + theme(axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             panel.spacing.x=unit(0.0, "mm"),
             panel.grid.minor = element_blank(),
             strip.text = element_text(face = "bold"),
             strip.background = element_rect(color = "white"),
             legend.position="none")


