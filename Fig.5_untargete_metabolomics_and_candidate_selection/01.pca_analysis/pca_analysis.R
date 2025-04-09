suppressPackageStartupMessages(library("tidyverse"))
source("01.metabolic_candidate_selection/01.pca_analysis/mypca_function.R")

####################################################################
# Import dataframe with both genotype info and metabolite abundances
####################################################################

peaks <- read.csv("01.metabolic_candidate_selection/genotype_and_peak_data.csv", 
                  stringsAsFactors = F)

sample_info <- read.csv("01.metabolic_candidate_selection/sample_genotype_phenotype.csv", 
                        stringsAsFactors = F)

df <- bind_cols(sample_info, peaks) %>% 
  select(- sample,)

##############
# PCA analysis
##############

pca_results <- mypca(peaks, center = TRUE, scale = TRUE)

### Scree plot: % variance explained per PC
df_explained_variance <- data.frame(
  exp_var = pca_results$explained_var$exp_var
  ) %>% 
  rownames_to_column("PC") %>% 
  mutate(PC = factor(PC,levels = unique(PC)))

scree_plot <- 
  ggplot(df_explained_variance, aes(x = PC, y = exp_var)) + 
  ylab('Explained variance (%)') + 
  ggtitle('Explained variance per principal component') + 
  geom_bar(stat = "identity") 
ggsave(filename = "01.metabolic_candidate_selection/01.pca_analysis/scree_plot.png", 
       plot = scree_plot)

### Sample scores
scores <- pca_results$scores
scores["genotype"] <- sample_info$genotype

# Score plot PC1 versus PC2
# Comment: this score plot separates 

pc1_vs_pc2 <- ggplot(scores) + 
  geom_point(aes(x = PC1, y = PC2, shape = genotype, col = genotype), size = 2) + 
  xlab(paste0('PC1 (',df_explained_variance[1,2],'% explained variance)')) + 
  ylab(paste0('PC2 (',df_explained_variance[2,2],'% explained variance)')) + 
  ggtitle('PCA score plot')
pc1_vs_pc2
ggsave(filename = "01.metabolic_candidate_selection/01.pca_analysis/PC1_vs_PC2.png", 
       plot = pc1_vs_pc2)

# # Score plot PC1 versus PC3
pc2_vs_pc3 <- ggplot(scores) + 
  geom_point(aes(x = PC21, y = PC3, shape = genotype, col = genotype), size = 2) + 
  xlab(paste0('PC2 (',df_explained_variance[2,2],'% explained variance)')) + 
  ylab(paste0('PC3 (',df_explained_variance[3,2],'% explained variance)')) + 
  ggtitle('PCA score plot')
pc2_vs_pc3
ggsave(filename = "01.metabolic_candidate_selection/01.pca_analysis/PC1_vs_PC3.png", 
       plot = pc1_vs_pc3)
