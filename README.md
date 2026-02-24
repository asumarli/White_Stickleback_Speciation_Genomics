## **Strong but diffuse genetic divergence underlies differentiation in an incipient species of marine stickleback - 2025**

This repository contains code and metadata used to analyze data for this project. It includes code for generating:
### 1. [Base Map](Map/plot_base_map.R) 
### 2. [Input files]
### 3. [Sex-specific references](sex_specific_references/mask)
### 4. [PCA](PCA/PCA_plot.R)
### 5. Summary Statistics
* [Fst, pi, dxy in pixy](pixy/calculate_pi_fst_dxy)
* [Fst in vcftools](PBS/calculate_Fst)
* [Population Branch Statistic](PBS/calculate_PBS_plot.R)   
### 6. Outlier Analysis
* [Liftover and Snpeff](PBS/liftover_snpeff)
* [Retrieve Goterms](PBS/calculate_outlier_intervals_and_retreive_goterms.R)
### 7. Demographies
* [PSMC](PSMC/generate_input_file)
* [dadi](dadi) (including file generation: liftover, snpeff, snpsift, ancestral allele annotation)
### 8. CNV Analyses
* [Fst/Vst](CNVs/Fst_Vst)
* [PCA](CNVs/deldup_PCA)
* [Outlier Analysis](CNVs/outliers_goterms.R)

