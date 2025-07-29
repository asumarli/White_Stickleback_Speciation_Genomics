#Plot fst, dxy, and fst output from pixy.

library(tidyverse)
library(magrittr)
library(ggplot2)

#Autosomes
#Read in Fst
CB_CL_fst <- read.table("pixy_output/pixy_fst.txt", header = TRUE)

#Get sum of fst. Take the mean pos from window_pos 1 and 2.
fst <- CB_CL_fst %>%
  select(chromosome, window_pos_1, window_pos_2, avg_wc_fst) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_wc_fst)) %>%
  filter(!chromosome == "chrXIX") %>%
  filter(!chromosome == "chrM") %>%
  filter(avg_wc_fst > 0)

fst <- fst %>%
  mutate(avg_wc_fst = replace(avg_wc_fst, avg_wc_fst < 0 , 0))

#Read in pi
CB_CL_pi <- read.table("pixy_output/pixy_pi.txt", header = TRUE)
head(CB_CL_pi)
pi <- CB_CL_pi %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_pi)) %>%
  filter(!chromosome == "chrXIX") %>%
  filter(!chromosome == "chrM")

#Calculate pi for whites and commons separately
#Common
common_pi <- pi %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "common") %>%
  rename("common_pi"="avg_pi")

common_pi <- common_pi %>%
  select(chromosome, pos, common_pi)

#White
white_pi <- pi %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "white") %>%
  rename("white_pi"="avg_pi")

white_pi <- white_pi %>%
  select(chromosome, pos, white_pi) 

#Read in dxy
CB_CL_dxy <- read.table("pixy_output/pixy_dxy.txt", header = TRUE)

dxy <- CB_CL_dxy %>%
  select(chromosome, window_pos_1, window_pos_2, avg_dxy) %>%
  filter(!chromosome == "chrXIX") %>%  
  filter(!chromosome == "chrM") %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_dxy))

#Join fst, pi, and dxy into one table. 
#First make a column called "datset". 

fst$dataset<-"Fst"
common_pi$dataset<-"Common pi"
white_pi$dataset<-"White pi"
dxy$dataset <- "Dxy"

#bind rows together
pixy_sums_autosomes<-bind_rows(fst, dxy, white_pi, common_pi)

#Repeat above for X and Y pixy output
#join with pixy_sums_Y and XIX
pixy_sums_all <- rbind(pixy_sums_autosomes, pixy_sums_XIX, pixy_sums_Y)

#Get rid of "chr" in chromosome
library(stringr)
pixy_sums_all$chromosome <- str_replace(pixy_sums_all$chromosome, "chr", "")

#Reorder chromosomes
chr_nums <- c(1:21)
chr_roman <- as.roman(chr_nums)

chr_ordered <- paste0(chr_roman)
chr_ordered <- c(chr_ordered, "Un", "Y")

#Reoder datasets
data_order <- c("Fst", "Dxy", "White pi", "Common pi")

pixy_sum_order <- pixy_sums_all %>%
  select(chromosome, pos, avg_wc_fst, avg_dxy, white_pi, common_pi, dataset) %>%
  mutate(chromosome = factor(chromosome, levels = chr_ordered)) %>%
  mutate(dataset = factor(dataset, levels = data_order))

view(pixy_sum_order)
write.table(pixy_sum_order, "pixy_output/pixy_sum_order.txt", row.names = FALSE, quote = FALSE, col.names = TRUE, sep = "\t")

#Plot all chromosomes 
odd<- c("I", "III", "V", "VII", "IX", "XI", "XIII", "XV", "XVII", "XIX", "XXI", "Y")

pixy_sum_order$odd <- pixy_sum_order$chromosome %in% odd

p <- pixy_sum_order %>%  
  ggplot(aes(x = pos))+
  geom_point(aes(y = avg_wc_fst), size = .5, col = "black", alpha = .5)+
  geom_point(aes(y = white_pi), size = .5, col = "deepskyblue4", alpha = .5)+
  geom_point(aes(y = common_pi), size = .5, col = "darkgreen", alpha = .5)+
  geom_point(aes(y = avg_dxy), size = .5, col = "darkgrey", alpha = .5)+
  facet_grid(dataset ~ chromosome, scales = "free",space = "free", margins = FALSE, switch = "x") + 
  xlab("Chromsome")+
  ylab("Statistic Value") + 
  theme_classic() 


p + theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
        panel.spacing = unit(.0,'mm'),
        panel.grid.major = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(color = "white")) 
