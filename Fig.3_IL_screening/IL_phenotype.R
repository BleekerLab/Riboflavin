library("checkpoint")
checkpoint("2021-01-01")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("skimr"))
suppressPackageStartupMessages(library("Hmisc"))
suppressPackageStartupMessages(library("car"))
suppressPackageStartupMessages(library("multcomp"))

IL_data <- read.csv("Fig.3_IL_screening/IL_screen.csv", 
                         header = T, 
                         sep = ",", 
                         dec = ".", 
                         check.names = FALSE)

IL_data$stage <- factor(IL_data$stage, levels= c("eggs", "4th_instar_nymph"))
IL_data$Genotype <- factor(IL_data$Genotype, levels= c("KG1955", "IL27", "IL28", "IL29"))

skim(feeding_data)
###################
# Data exploration 
##################

# What is the difference in number of eggs? No difference
IL_data %>% 
  dplyr::filter(stage == "eggs") %>% 
  group_by(Genotype) %>% 
  ggplot(., aes(x = Genotype, y = number, group = Genotype)) +
  geom_boxplot(aes(color = Genotype)) +
  labs(x = "Genotype", y = "Number of eggs")+
  theme_classic() +
  geom_point(aes(fill = Genotype), size = 2, shape = 21,
             position = position_jitterdodge())

## statistics eggs
eggs <- aov(number~Genotype, data = IL_data[which(IL_data$stage=='eggs'), ])
summary(eggs)

shapiro.test(resid(eggs))
leveneTest(eggs)

test_eggs <- glht(eggs, linfct=mcp(Genotype="Dunnett"))
summary(test_eggs)


# What is the difference in number of 4th instars? Lower on IL1927
IL_data %>% 
  dplyr::filter(stage == "4th_instar_nymph") %>% 
  group_by(Genotype) %>% 
  ggplot(., aes(x = Genotype, y = number, group = Genotype)) +
  geom_boxplot(aes(color = Genotype)) +
  labs(x = "Genotype", y = "Number of 4th instar nymphs")+
  theme_classic() +
  geom_point(aes(fill = Genotype), size = 2, shape = 21,
             position = position_jitterdodge())

## statistics eggs
instars <- aov(number~Genotype, data = IL_data[which(IL_data$stage=='4th_instar_nymph'), ])
summary(instars)

shapiro.test(resid(instars))
leveneTest(instars)

test_instars <- glht(instars, linfct=mcp(Genotype="Dunnett"))
summary(test_instars)
