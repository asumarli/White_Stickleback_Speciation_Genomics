####PCA of Autosomes
#Repeat below for XIX and Y 

#load packages
library("SNPRelate")
library("ggplot2")
library("tidyverse")

#Convert vcf to gds
snpgdsVCF2GDS("for_PBS_autosomes_no_XIX.vcf.gz","for_PBS_autosomes_no_XIX.gds")

#Read in gds and perform PCA
genofile = snpgdsOpen("data/for_PBS_autosomes_no_XIX.gds")
pca1 = snpgdsPCA(genofile, autosome.only = F)

#View
plot(pca1$eigenvect[,1],pca1$eigenvect[,2], type="n")
text(pca1$eigenvect[,1],pca1$eigenvect[,2], labels=pca1$sample.id)

#Calculate proportion of variance in each pc
pca1$varprop
head(round(pca1$varprop*100, 2))

#Read in the pop designations without sample names as a .txt
pop_code <- read.table("meta_PBS_autosomes.txt", header = TRUE)
head(pop_code)
pop_code <- pop_code %>%
  rename("sample.id" =  "ID")
pop_code

#make a dataframe of eigenvectors
tab <- data.frame(sample.id = pca1$sample.id,
                  EV1 = pca1$eigenvect[,1],    # the first eigenvector
                  EV2 = pca1$eigenvect[,2],    # the second eigenvector
                  EV3 = pca1$eigenvect[,3],    # the third eigenvector
                  EV4 = pca1$eigenvect[,4],    # the fourth eigenvector
                  stringsAsFactors = FALSE)
head(tab)

#Join meta with PCs by sample.id
tab <- inner_join(tab, pop_code)

#Plot the first 2 PC

x <- tab %>%  
  ggplot() +
  geom_point(aes(x = EV1, y = EV2, fill = Pop, shape = Loc, size = 1)) +
  scale_fill_manual(values = alpha(c("white" = "deepskyblue3", "common" = "green3", "Quebec" = "darkorchid"),.8)) +
  scale_shape_manual(values = c("Canal_Lake" = 21, "Cherry_Burton_Road" = 21, "Rainbow_Haven" = 22, "Baie_de_L_Isle_Verte" = 23, "Baddeck" = 24)) +
  xlab("PC1 (3.22%)") +
  ylab("PC2 (1.88%)") +
  theme_linedraw()+
  theme(legend.position = "none")

x
