peaks <- read.csv("01.metabolic_candidate_selection/genotype_and_peak_data.csv")

sample_info <- read.csv("01.metabolic_candidate_selection/sample_genotype_phenotype.csv")


# finding the row index of IL27_6
row_index_of_outliers = c(
  match(x = "IL27_6", table = sample_info$sample),
  match(x = "s_ch_1", table = sample_info$sample)
)

# remove this line from the peaks data
peaks = peaks[-row_index_of_outliers,]

# remove it from the sample to genotype df
sample_info <- sample_info[-row_index_of_outliers,]


df <- bind_cols(sample_info, peaks)