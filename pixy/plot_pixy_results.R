#Plot fst, dxy, and fst output from pixy.

#Load packages
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(greekLetters)

#Autosomes
#Fst

CB_CL_fst_aut <- read.table("pixy_output/pixy_fst.txt", header = TRUE)
head(CB_CL_fst_aut)

#Get sum of fst. Take the mean pos from window_pos 1 and 2.
fst_aut <- CB_CL_fst_aut %>%
  select(chromosome, window_pos_1, window_pos_2, avg_wc_fst) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_wc_fst)) %>%
  filter(!chromosome == "chrXIX") %>%
  filter(!chromosome == "chrM") %>%
  filter(avg_wc_fst > 0)

fst_aut <- fst_aut %>%
  mutate(avg_wc_fst = replace(avg_wc_fst, avg_wc_fst < 0 , 0))

#Calculate mean and sd.
mean <- mean(x = fst_aut$avg_wc_fst)
sd <- sd(x = fst_aut$avg_wc_fst,na.rm = TRUE)
round(mean, digits = 5)
round(sd, digits = 5)

#Calculate CI
#Bootstrap CI
z <- vector()
for(i in 1:10000){
  xboot <- sample(fst_aut$avg_wc_fst, replace=TRUE)
  z[i] <- mean(xboot)
}
hist(fst_aut$avg_wc_fst)
hist(z)

CI<-quantile(z, probs = c(0.025,0.975))
head(round(CI, 5))
0.03426-0.03319

#Read in pi
CB_CL_pi_aut <- read.table("pixy_output/pixy_pi.txt", header = TRUE)
head(CB_CL_pi_aut)
pi_aut <- CB_CL_pi_aut %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi, count_diffs, count_comparisons) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_pi)) %>%
  filter(!chromosome == "chrXIX") %>%
  filter(!chromosome == "chrM") 

#Calculate pi for whites and commons separately
#common
common_pi_aut <- pi_aut %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "common") %>%
  rename("common_pi_aut"="avg_pi")

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CB_aut_sum_count_diffs<-sum(common_pi_aut$count_diffs, na.rm = T)
CB_aut_sum_count_comps<-sum(common_pi_aut$count_comparisons, na.rm = T)
CB_aut_sum_count_diffs/CB_aut_sum_count_comps
round(CB_aut_sum_count_diffs/CB_aut_sum_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CB count diffs 
CB_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(common_pi_aut$count_diffs, replace=TRUE)
  CB_count_diff_random_sample[i] <- mean(xboot)
  }
hist(CB_count_diff_random_sample)

CB_count_diff_random_sample_CI<-quantile(count_diff_random_sample, probs = c(0.025, 0.975))
head(round(CB_count_diff_random_sample_CI, 5))

#Random sample of CB count comparisons
CB_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(common_pi_aut$count_comparisons, replace=TRUE)
  CB_count_comp_random_sample[i] <- mean(xboot)
}
hist(CB_count_comp_random_sample)

CB_count_comp_random_sample_CI<-quantile(CB_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(CB_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00221 - (20636.4/9369755), 5)
round(((21092.87/9490243) - 0.00221), 5)


#Calculate pi for whites and whites separately
#White
white_pi_aut <- pi_aut %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "white") %>%
  rename("white_pi_aut"="avg_pi")

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CL_aut_sum_count_diffs<-sum(white_pi_aut$count_diffs, na.rm = T)
CL_aut_sum_count_comps<-sum(white_pi_aut$count_comparisons, na.rm = T)
CL_aut_sum_count_diffs/CL_aut_sum_count_comps
round(CL_aut_sum_count_diffs/CL_aut_sum_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
CL_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(white_pi_aut$count_diffs, replace=TRUE)
  CL_count_diff_random_sample[i] <- mean(xboot)
}
hist(CL_count_diff_random_sample)

CL_count_diff_random_sample_CI<-quantile(CL_count_diff_random_sample, probs = c(0.025, 0.975))
head(round(CL_count_diff_random_sample_CI, 5))

#Random sample of CL count comparisons
CL_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(white_pi_aut$count_comparisons, replace=TRUE)
  CL_count_comp_random_sample[i] <- mean(xboot)
}
hist(count_comp_random_sample)

CL_count_comp_random_sample_CI<-quantile(CL_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(CL_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00219 - (21278.05/9762409), 5)
round(((21744.46/9888235) - 0.00219), 5)

#Dxy
dxy <- read.table("pixy_output/pixy_dxy.txt", header = TRUE)
head(dxy)

CB_CL_dxy <- dxy %>%
  filter(!chromosome == "chrXIX") %>%  
  filter(!chromosome == "chrM") %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_dxy))

sum_count_diffs<-sum(CB_CL_dxy$count_diffs, na.rm = T)
sum_count_comps<-sum(CB_CL_dxy$count_comparisons, na.rm = T)
sum_count_diffs/sum_count_comps
round(sum_count_diffs/sum_count_comps, digits = 5)

#Calculate CI for dxy. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
dxy_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_dxy$count_diffs, replace=TRUE)
  dxy_count_diff_random_sample[i] <- mean(xboot)
}
hist(dxy_count_diff_random_sample)

dxy_count_diff_random_sample_CI<-quantile(dxy_count_diff_random_sample, probs = c(0.025, 0.975))
head(round(dxy_count_diff_random_sample_CI, 5))

#Random sample of CL count comparisons
dxy_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_dxy$count_comparisons, replace=TRUE)
  dxy_count_comp_random_sample[i] <- mean(xboot)
}
hist(dxy_count_comp_random_sample)

dxy_count_comp_random_sample_CI<-quantile(dxy_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(dxy_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00226 - (44127.655/19598993), 5)
round(((45090.47/19853672 ) - 0.00226), 5)

#Repeat the above for the
#X Chromosome
#Fst

CB_CL_fst_XIX <- read.table("pixy_output/XIX_pixy_fst.txt", header = TRUE)
head(CB_CL_fst_XIX)

#Get sum of fst. Take the mean pos from window_pos 1 and 2.
fst_XIX <- CB_CL_fst_XIX %>%
  select(chromosome, window_pos_1, window_pos_2, avg_wc_fst) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_wc_fst)) %>%
  filter(avg_wc_fst > 0)

fst_XIX <- fst_XIX %>%
  mutate(avg_wc_fst = replace(avg_wc_fst, avg_wc_fst < 0 , 0))

#Calculate mean and sd.
mean <- mean(x = fst_XIX$avg_wc_fst)
sd <- sd(x = fst_XIX$avg_wc_fst,na.rm = TRUE)
round(mean, digits = 5)
round(sd, digits = 5)

#Calculate CI
#Bootstrap CI
z <- vector()
for(i in 1:10000){
  xboot <- sample(fst_XIX$avg_wc_fst, replace=TRUE)
  z[i] <- mean(xboot)
}
hist(fst_XIX$avg_wc_fst)
hist(z)

CI<-quantile(z, probs = c(0.025,0.975))
head(round(CI, 5))
0.03334-0.02934
0.03762-0.03334

#Read in pi
CB_CL_pi_XIX <- read.table("pixy_output/XIX_pixy_pi.txt", header = TRUE)
head(CB_CL_pi_XIX)
pi_XIX <- CB_CL_pi_XIX %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi, count_diffs, count_comparisons) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_pi))

#Calculate pi for whites and commons separately
#common 
common_pi_XIX <- pi_XIX %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "common") %>%
  rename("common_pi_XIX"="avg_pi")

#WITHOUT the PAR
CB_pi_non_par <- common_pi_XIX %>%
  filter(window_pos_1 > 2550001) %>%  
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(common_pi_XIX))

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CB_non_par_sum_count_diffs<-sum(CB_pi_non_par$count_diffs, na.rm = T)
CB_non_par_count_comps<-sum(CB_pi_non_par$count_comparisons, na.rm = T)
CB_non_par_sum_count_diffs/CB_non_par_count_comps
round(CB_non_par_sum_count_diffs/CB_non_par_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CB count diffs 
CB_non_par_sum_count_diffs_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_pi_non_par$count_diffs, replace=TRUE)
  CB_non_par_sum_count_diffs_random_sample[i] <- mean(xboot)
}
hist(CB_non_par_sum_count_diffs_random_sample)

CB_non_par_sum_count_diffs_random_sample_CI<-quantile(CB_non_par_sum_count_diffs_random_sample, probs = c(0.025, 0.975))
head(round(CB_non_par_sum_count_diffs_random_sample_CI, 5))

#Random sample of CB count comparisons
CB_non_par_count_comps_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_pi_non_par$count_comparisons, replace=TRUE)
  CB_non_par_count_comps_random_sample[i] <- mean(xboot)
}
hist(CB_non_par_count_comps_random_sample)

CB_non_par_count_comps_random_sample_CI<-quantile(CB_non_par_count_comps_random_sample, probs = c(0.025, 0.975))
head(round(CB_non_par_count_comps_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00119 - (4765.288/4134335), 5)
round(((5265.350/4280383) - 0.00119), 5)

#Repeat for just the PAR
CB_pi_PAR <- common_pi_XIX %>%
  filter(window_pos_1 < 2550001) %>%  
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(common_pi_XIX))

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CB_PAR_sum_count_diffs<-sum(CB_pi_PAR$count_diffs, na.rm = T)
CB_PAR_count_comps<-sum(CB_pi_PAR$count_comparisons, na.rm = T)
CB_PAR_sum_count_diffs/CB_PAR_count_comps
round(CB_PAR_sum_count_diffs/CB_PAR_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CB count diffs 
CB_PAR_sum_count_diffs_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_pi_PAR$count_diffs, replace=TRUE)
  CB_PAR_sum_count_diffs_random_sample[i] <- mean(xboot)
}
hist(CB_PAR_sum_count_diffs_random_sample)

CB_PAR_sum_count_diffs_random_sample_CI<-quantile(CB_PAR_sum_count_diffs_random_sample, probs = c(0.025, 0.975))
head(round(CB_PAR_sum_count_diffs_random_sample_CI, 5))

#Random sample of CB count comparisons
CB_PAR_count_comps_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_pi_PAR$count_comparisons, replace=TRUE)
  CB_PAR_count_comps_random_sample[i] <- mean(xboot)
}
hist(CB_PAR_count_comps_random_sample)

CB_PAR_count_comps_random_sample_CI<-quantile(CB_PAR_count_comps_random_sample, probs = c(0.025, 0.975))
head(round(CB_PAR_count_comps_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00421 - (11082.28/2696288), 5)
round(((14035.30/3258598) - 0.00421), 5)

#Calculate pi for whites  separately
#####White
white_pi_XIX <- pi_XIX %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "white") %>%
  rename("white_pi_XIX"="avg_pi")

#WITHOUT the PAR
CL_pi_non_par <- white_pi_XIX %>%
  filter(window_pos_1 > 2550001) %>%  
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(white_pi_XIX))

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CL_non_par_sum_count_diffs<-sum(CL_pi_non_par$count_diffs, na.rm = T)
CL_non_par_count_comps<-sum(CL_pi_non_par$count_comparisons, na.rm = T)
CL_non_par_sum_count_diffs/CL_non_par_count_comps
round(CL_non_par_sum_count_diffs/CL_non_par_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
CL_non_par_sum_count_diffs_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CL_pi_non_par$count_diffs, replace=TRUE)
  CL_non_par_sum_count_diffs_random_sample[i] <- mean(xboot)
}
hist(CL_non_par_sum_count_diffs_random_sample)

CL_non_par_sum_count_diffs_random_sample_CI<-quantile(CL_non_par_sum_count_diffs_random_sample, probs = c(0.025, 0.975))
head(round(CL_non_par_sum_count_diffs_random_sample_CI, 5))

#Random sample of CL count comparisons
CL_non_par_count_comps_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CL_pi_non_par$count_comparisons, replace=TRUE)
  CL_non_par_count_comps_random_sample[i] <- mean(xboot)
}
hist(CL_non_par_count_comps_random_sample)

CL_non_par_count_comps_random_sample_CI<-quantile(CL_non_par_count_comps_random_sample, probs = c(0.025, 0.975))
head(round(CL_non_par_count_comps_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00116 - (4927.300/4390547), 5)
round(((5443.742/4545534) - 0.00116), 5)

#Repeat for just the PAR
CL_pi_PAR <- white_pi_XIX %>%
  filter(window_pos_1 < 2550001) %>%  
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(white_pi_XIX))

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CL_PAR_sum_count_diffs<-sum(CL_pi_PAR$count_diffs, na.rm = T)
CL_PAR_count_comps<-sum(CL_pi_PAR$count_comparisons, na.rm = T)
CL_PAR_sum_count_diffs/CL_PAR_count_comps
round(CL_PAR_sum_count_diffs/CL_PAR_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
CL_PAR_sum_count_diffs_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CL_pi_PAR$count_diffs, replace=TRUE)
  CL_PAR_sum_count_diffs_random_sample[i] <- mean(xboot)
}
hist(CL_PAR_sum_count_diffs_random_sample)

CL_PAR_sum_count_diffs_random_sample_CI<-quantile(CL_PAR_sum_count_diffs_random_sample, probs = c(0.025, 0.975))
head(round(CL_PAR_sum_count_diffs_random_sample_CI, 5))

#Random sample of CL count comparisons
CL_PAR_count_comps_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CL_pi_PAR$count_comparisons, replace=TRUE)
  CL_PAR_count_comps_random_sample[i] <- mean(xboot)
}
hist(CL_PAR_count_comps_random_sample)

CL_PAR_count_comps_random_sample_CI<-quantile(CL_PAR_count_comps_random_sample, probs = c(0.025, 0.975))
head(round(CL_PAR_count_comps_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00417 - (11592.65/2845404), 5)
round(((14739.74/3442772) - 0.00417), 5)

#Dxy
XIX_dxy <- read.table("pixy_output/XIX_pixy_dxy.txt", header = TRUE)
head(XIX_dxy)

CB_CL_XIX_dxy <- XIX_dxy %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_dxy))

sum_count_diffs<-sum(CB_CL_XIX_dxy$count_diffs, na.rm = T)
sum_count_comps<-sum(CB_CL_XIX_dxy$count_comparisons, na.rm = T)
sum_count_diffs/sum_count_comps
round(sum_count_diffs/sum_count_comps, digits = 5)

#Calculate CI for XIX_dxy. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
XIX_dxy_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_XIX_dxy$count_diffs, replace=TRUE)
  XIX_dxy_count_diff_random_sample[i] <- mean(xboot)
}
hist(XIX_dxy_count_diff_random_sample)

XIX_dxy_count_diff_random_sample_CI<-quantile(XIX_dxy_count_diff_random_sample, probs = c(0.025, 0.975))
head(round(XIX_dxy_count_diff_random_sample_CI, 5))

#Random sample of CL count comparisons
XIX_dxy_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_XIX_dxy$count_comparisons, replace=TRUE)
  XIX_dxy_count_comp_random_sample[i] <- mean(xboot)
}
hist(XIX_dxy_count_comp_random_sample)

XIX_dxy_count_comp_random_sample_CI<-quantile(XIX_dxy_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(XIX_dxy_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00148 - (12185.92/8594483), 5)
round(((13798.47/8951886) - 0.00148), 5)

###############
#Y chr 
#Fst
CB_CL_fst_Y <- read.table("pixy_output/Y_pixy_fst.txt", header = TRUE)
head(CB_CL_fst_Y)

#Get sum of fst. Take the mean pos from window_pos 1 and 2.
fst_Y <- CB_CL_fst_Y %>%
  select(chromosome, window_pos_1, window_pos_2, avg_wc_fst) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_wc_fst)) %>%
  filter(avg_wc_fst > 0)

#Calculate mean and sd.
mean <- mean(x = fst_Y$avg_wc_fst)
sd <- sd(x = fst_Y$avg_wc_fst, na.rm = TRUE)
round(mean, digits = 5)
round(sd, digits = 5)

#Calculate CI
#Bootstrap CI
z <- vector()
for(i in 1:10000){
  xboot <- sample(fst_Y$avg_wc_fst, replace=TRUE)
  z[i] <- mean(xboot)
}
hist(fst_Y$avg_wc_fst)
hist(z)

CI<-quantile(z, probs = c(0.025,0.975))
head(round(CI, 5))
0.33928-0.32681
0.35187-0.33928

#Read in pi
CB_CL_pi_Y <- read.table("pixy_output/Y_pixy_pi.txt", header = TRUE)
head(CB_CL_pi_Y)
pi_Y <- CB_CL_pi_Y %>%
  select(pop, chromosome, window_pos_1, window_pos_2, avg_pi, count_diffs, count_comparisons) %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_pi))

#Calculate pi for whites and commons separately
#common
common_pi_Y <- pi_Y %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "Common") %>%
  rename("common_pi_Y"="avg_pi")

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CB_Y_sum_count_diffs<-sum(common_pi_Y$count_diffs, na.rm = T)
CB_Y_sum_count_comps<-sum(common_pi_Y$count_comparisons, na.rm = T)
CB_Y_sum_count_diffs/CB_Y_sum_count_comps
round(CB_Y_sum_count_diffs/CB_Y_sum_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CB count diffs 
CB_Y_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(common_pi_Y$count_diffs, replace=TRUE)
  CB_Y_count_diff_random_sample[i] <- mean(xboot)
}
hist(CB_Y_count_diff_random_sample)

CB_Y_count_diff_random_sample_CI<-quantile(CB_Y_count_diff_random_sample, probs = c(0.025, 0.975))
head(round(CB_Y_count_diff_random_sample_CI, 5))

#Random sample of CB count comparisons
CB_Y_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(common_pi_Y$count_comparisons, replace=TRUE)
  CB_Y_count_comp_random_sample[i] <- mean(xboot)
}
hist(CB_Y_count_comp_random_sample)

CB_Y_count_comp_random_sample_CI<-quantile(CB_Y_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(CB_Y_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(0.00009 - (582.0434/6782602), 5)
round(((645.8804/7137974) - 0.00009), 5)


#Calculate pi for whites and whites separately
#White
white_pi_Y <- pi_Y %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(pop == "White") %>%
  rename("white_pi_Y"="avg_pi")

#Calculate mean as: 
#(window 1 count_diffs + window 2 count_diffs) / (window 1 comparisons + window 2 comparisons)
CL_Y_sum_count_diffs<-sum(white_pi_Y$count_diffs, na.rm = T)
CL_Y_sum_count_comps<-sum(white_pi_Y$count_comparisons, na.rm = T)
mean<-(CL_Y_sum_count_diffs/CL_Y_sum_count_comps)
round(CL_Y_sum_count_diffs/CL_Y_sum_count_comps, digits = 5)

#Calculate CI for pi. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
CL_Y_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(white_pi_Y$count_diffs, replace=TRUE)
  CL_Y_count_diff_random_sample[i] <- mean(xboot)
}
hist(CL_Y_count_diff_random_sample)

CL_Y_count_diff_random_sample_CI<-quantile(CL_Y_count_diff_random_sample, probs = c(0.025, 0.975))
head(CL_Y_count_diff_random_sample_CI)

#Random sample of CL count comparisons
CL_Y_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(white_pi_Y$count_comparisons, replace=TRUE)
  CL_Y_count_comp_random_sample[i] <- mean(xboot)
}
hist(CL_Y_count_comp_random_sample)

CL_Y_count_comp_random_sample_CI<-quantile(CL_Y_count_comp_random_sample, probs = c(0.025, 0.975))
head(CL_Y_count_comp_random_sample_CI)

#Subtract from pi calculated earlier 
round(mean - (982.082/6836975), 5)
round(((1063.126/7206589) - mean), 5)

#Dxy
Y_dxy <- read.table("pixy_output/Y_pixy_dxy.txt", header = TRUE)
head(Y_dxy)

CB_CL_Y_dxy <- Y_dxy %>%
  mutate(pos = window_pos_1 + (window_pos_2-window_pos_1)/2) %>%
  filter(!is.na(avg_dxy))

sum_count_diffs<-sum(CB_CL_Y_dxy$count_diffs, na.rm = T)
sum_count_comps<-sum(CB_CL_Y_dxy$count_comparisons, na.rm = T)
mean<-sum_count_diffs/sum_count_comps
mean
round(sum_count_diffs/sum_count_comps, digits = 5)

#Calculate CI for Y_dxy. For both count diffs and count comp:
#1.) take a random sample, 
#2.) calculate lower limit of 95% CI, 
#3.)subtract from mean pi calculated earlier.

#Random sample of CL count diffs 
Y_dxy_count_diff_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_Y_dxy$count_diffs, replace=TRUE)
  Y_dxy_count_diff_random_sample[i] <- mean(xboot)
}
hist(Y_dxy_count_diff_random_sample)

Y_dxy_count_diff_random_sample_CI<-quantile(Y_dxy_count_diff_random_sample, probs = c(0.025, 0.975))
head(round(Y_dxy_count_diff_random_sample_CI, 5))

#Random sample of CL count comparisons
Y_dxy_count_comp_random_sample <- vector()
for(i in 1:10000){
  xboot <- sample(CB_CL_Y_dxy$count_comparisons, replace=TRUE)
  Y_dxy_count_comp_random_sample[i] <- mean(xboot)
}
hist(Y_dxy_count_comp_random_sample)

Y_dxy_count_comp_random_sample_CI<-quantile(Y_dxy_count_comp_random_sample, probs = c(0.025, 0.975))
head(round(Y_dxy_count_comp_random_sample_CI, 5))

#Subtract from pi calculated earlier 
round(mean - (2701.517/14335366), 5)
round(((2949.201/15077829 ) - mean), 5)

#########
#Prepare data for plotting
#Start with autosomes
fst_aut$dataset<-"Fst"
common_pi_aut$dataset<-"Common pi"
white_pi_aut$dataset<-"White pi"
CB_CL_dxy$dataset <- "Dxy"


pixy_sums_aut<-bind_rows(fst_aut, CB_CL_dxy, white_pi_aut, common_pi_aut)
pixy_sums_aut <- pixy_sums_aut %>%
  rename("white_pi" = "white_pi_aut") %>%
  rename("common_pi" = "common_pi_aut")

view(pixy_sums_aut)

####
#XIX chromosome
fst_XIX$dataset<-"Fst"
common_pi_XIX$dataset<-"Common pi"
white_pi_XIX$dataset<-"White pi"
CB_CL_XIX_dxy$dataset <- "Dxy"


pixy_sums_XIX<-bind_rows(fst_XIX, CB_CL_XIX_dxy, white_pi_XIX, common_pi_XIX)
pixy_sums_XIX<-pixy_sums_XIX %>%
  rename("white_pi" = "white_pi_XIX") %>%
  rename("common_pi" = "common_pi_XIX")


view(pixy_sums_XIX)

####
#Y chromosome
fst_Y$dataset<-"Fst"
common_pi_Y$dataset<-"Common pi"
white_pi_Y$dataset<-"White pi"
CB_CL_Y_dxy$dataset <- "Dxy"


pixy_sums_Y<-bind_rows(fst_Y, CB_CL_Y_dxy, white_pi_Y, common_pi_Y)
view(pixy_sums_Y)

pixy_sums_Y<-pixy_sums_Y %>%
  rename("white_pi" = "white_pi_Y") %>%
  rename("common_pi" = "common_pi_Y")


#Now join with pixy_sums_Y and XIX
pixy_sums_all <- rbind(pixy_sums_aut, pixy_sums_XIX, pixy_sums_Y)
view(pixy_sums_all)

#Get rid of "chr" in chromosome
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

fst_plot <- pixy_sum_order %>%  
  filter(!is.na(chromosome))%>%
  ggplot(aes(x = pos))+
  geom_point(aes(y = avg_wc_fst, fill = odd, color = odd), size = .5, alpha = .5)+
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "darkgrey"))+
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "darkgrey"))+
  facet_grid( ~ chromosome, scales = "free_x", space = "free",switch = "x")+
  scale_y_continuous(limits = c(0,.75)) +
  xlab(label = NULL)+
  ylab(expression(F[ST])) + 
  theme_classic() 


fst_plot <- fst_plot + theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank(),
                   panel.spacing.y = unit(0, 'mm'),
                   panel.grid.major = element_blank(),
                   strip.text = element_text(face = "bold"),
                   strip.background = element_rect(color = "white"),
                   legend.position="none") 


fst_plot

dxy_plot <- pixy_sum_order %>% 
  filter(!is.na(chromosome))%>%
  ggplot(aes(x = pos))+
  geom_point(aes(y = avg_dxy, fill = odd, color = odd), size = .5, alpha = .5)+
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "gold3"))+
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gold3"))+
  facet_grid( ~ chromosome, scales = "free_x", space = "free", switch = "x")+
  scale_y_continuous(limits = c(0,.01)) +
  xlab(NULL)+
  ylab(expression(italic(d[XY]))) + 
  theme_classic()


dxy_plot <- dxy_plot + theme(axis.ticks.x = element_blank(),
                   axis.text.x = element_blank(),
                   panel.spacing = unit(0,'mm'),
                   panel.grid.major = element_blank(),
                   strip.text = element_text(face = "bold"),
                   strip.background = element_rect(color = "white"),
                   legend.position="none") 

dxy_plot

white_pi <- pixy_sum_order %>%  
  ggplot(aes(x = pos))+
  geom_point(aes(y = white_pi, fill = odd, color = odd), size = .5, alpha = .5)+
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "deepskyblue4"))+
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "deepskyblue4"))+
  facet_grid( ~ chromosome, scales = "free", space = "free", switch = "x")+
  scale_y_continuous(limits = c(0,.01)) +
  xlab(NULL)+
  ylab(expression(paste("white", " ", pi))) + 
  theme_classic() 


white_pi <-white_pi + theme(axis.ticks.x = element_blank(),
                            axis.text.x = element_blank(),
                            panel.spacing.y = unit(.0,'mm'),
                            panel.grid.major = element_blank(),
                            strip.text = element_text(face = "bold"),
                            strip.background = element_rect(color = "white"),
                            legend.position="none") 

common_pi <- pixy_sum_order %>%  
  ggplot(aes(x = pos))+
  geom_point(aes(y = avg_dxy, fill = odd, color = odd), size = .5, alpha = .5)+
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "green4"))+
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "green4"))+
  facet_grid( ~ chromosome, scales = "free", space = "free", switch = "x")+
  scale_y_continuous(limits = c(0,.01)) +
  xlab("Chromsome")+
  ylab(expression(paste("common", " ", pi))) + 
  theme_classic() 


common_pi <- common_pi + theme(axis.ticks.x = element_blank(),
                               axis.text.x = element_blank(),
                               panel.spacing.y = unit(.0,'mm'),
                               panel.grid.major = element_blank(),
                               strip.text = element_text(face = "bold"),
                               strip.background = element_rect(color = "white"),
                               legend.position="none") 

#Plot all the stats together
ggarrange(fst_plot, dxy_plot, white_pi, common_pi, nrow = 4, align = "v")
