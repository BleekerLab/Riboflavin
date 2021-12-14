library("checkpoint")
checkpoint("2020-12-01")

#############
# Library
#############
library(ggpubr)
library(multcomp)
library(car)
library(tidyverse)
library(rstatix) # pipe friendly statistical tests


#############
# Data import
#############
screening_data <- read.csv("hertelling.csv", 
                   header = T, 
                   stringsAsFactors = FALSE) # ensures that you do the conversion yourself


# Genotype and stage as factors and filter genotype
screening_data <- screening_data %>% 
  mutate(genotype = factor(genotype, levels = unique(genotype))) %>% 
  mutate(stage = factor(stage, levels = c("egg", "fourth+exuviae"))) %>%
  filter(genotype == "Moneymaker" | genotype == "LA1840")
         


###########################
## plot for eggs and nymphs
###########################
egg_nymph_plot <- ggplot(screening_data, aes(x=stage, y=number, fill=stage)) + 
  labs(y = "# of individuals") +
  geom_boxplot() + 
  geom_point(aes(fill = stage), size = 1, shape = 21,
             position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  facet_wrap(~ genotype)
egg_nymph_plot

ggsave(filename = "Fig.1_Bioassay_screen_Arjen/plot_eggs_nymphs.png", 
       plot = egg_nymph_plot)


##############
## ANOVA tests
##############

## statistics eggs

screening_data %>% 
  select(- replicate) %>% 
  nest(data = c(genotype, number)) %>% 
  mutate(aov_test = map(data, ~ aov(number ~ genotype, data = .x))) %>% 
  mutate(tidy_aov_test = map(aov_test, tidy)) %>% 
  unnest(tidy_aov_test) %>% 
  filter(term == "genotype")
  


## paired t-test per genotype
stat.test <- screening_data %>%
  group_by(genotype) %>%
  pairwise_t_test(
    number ~ stage, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) ##%>%
  select(-df, -statistic, -p) # Remove details
stat.test




