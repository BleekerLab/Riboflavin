
Screen <- read.csv("Fig.1_Bioassay_screen_Arjen/hertelling.csv", 
                   header = T, 
                   sep = ";")

#Percentage<-read.csv("hertelling_Arjens_screen.csv", header=T, sep = ";", dec = ",")

checkpoint::checkpoint("2020-12-01")
library(ggpubr)
library(multcomp)
library(car)
library(tidyverse)
#library(rstatix)


## Make sure MM is the first genotype
Screen[1:4,"genotype"] <- "AMM"
Screen[24:27,"genotype"] <- "AMM"

##Make Screen$genotype into a factor
Screen$genotype = as.factor(Screen$genotype)
Screen$stage = as.factor(Screen$stage)


## statistics eggs
eggs <- aov(number~genotype, data = Screen[which(Screen$stage=='egg'), ])
summary(eggs)

shapiro.test(resid(eggs))
leveneTest(eggs)

test_eggs <- glht(eggs, linfct=mcp(genotype="Dunnett"))
summary(test_eggs)

## statistics nymphs
nymphs <- aov(number~genotype, data = Screen[which(Screen$stage=='fourth+exuviae'), ])
summary(nymphs)

shapiro.test(resid(nymphs))
leveneTest(nymphs)

test_nymphs <- glht(nymphs, linfct=mcp(genotype="Dunnett"))
summary(test_nymphs)


##remove empty rows
Screen <- Screen[-c(47:73),]

## paired t-test per genotype
stat.test <- Screen %>%
  group_by(genotype) %>%
  pairwise_t_test(
    number ~ stage, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) ##%>%
  select(-df, -statistic, -p) # Remove details
stat.test



library(ggplot2)

##plot for eggs and nymphs
egg_nymph_plot <- ggplot(Screen, aes(x=genotype, y=number, fill=stage)) + 
  labs(y = "# of individuals") +
  geom_boxplot() + 
  geom_point(aes(fill = stage), size = 1, shape = 21,
             position = position_jitterdodge()) +
  theme_classic()
  
egg_nymph_plot

