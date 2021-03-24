library("checkpoint")
checkpoint("2020-12-01")
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("skimr"))
suppressPackageStartupMessages(library("Hmisc"))

feeding_data <- read.csv("Fig.6_Riboflavin_feeding_bioassay/data.csv", 
                         header = T, 
                         sep = ",", 
                         dec = ".", 
                         check.names = FALSE) %>% 
  mutate(total_ind = left + right) %>% 
  # because do not know whether the numbers are eggs or instars
  dplyr::filter(stage != "eggs_crawler") 

skim(feeding_data)
###################
# Data exploration 
##################

# On day 1 and 3, same number of eggs between control and riboflavin?
feeding_data %>% 
  dplyr::filter(day == 1| day == 3) %>% 
  mutate(day = factor(day)) %>%
  ggplot(., aes(x = treatment, y = total_ind, fill = treatment)) +
  geom_boxplot() +
  geom_point() +
  facet_wrap(~ day) +
  labs(y="number of eggs per plant")+
  ggtitle("Effect of riboflavin on WF fertility")

# Group day 1 and 3 and show the number of eggs per condition
feeding_data %>% 
  dplyr::filter(day == 1| day == 3) %>% 
  mutate(day = factor(day)) %>%
  ggplot(., aes(x = treatment, y = total_ind, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(aes(shape = day), size = 2, width = 0.05) +
  ggtitle("Effect of riboflavin on WF fertility")

# An effect of riboflavin on the number of instars regardless of their development stage?
# Yes so it seems. A positive effect. 
feeding_data %>% 
  dplyr::filter(stage == "instars") %>% 
  ggplot(., aes(x = treatment, y = total_ind, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.1)
  

# Instar line plots: one per treatment
feeding_data %>% 
  dplyr::filter(stage == "instars" | stage == "4th_instar") %>% 
  group_by(treatment, day, stage) %>% 
  summarise(average_total_ind = mean(total_ind),
            std_total_ind = sd(total_ind)) %>% 
  ggplot(., aes(x = day, y = average_total_ind, group = stage)) +
  facet_wrap(~treatment) +
  geom_line(aes(color = stage)) 

# All individuals (eggs, instars) but not 4th instar because they are already counted
feeding_data %>% 
  dplyr::filter(stage != "4th_instar") %>% 
  group_by(treatment, day) %>% 
  summarise(average_total_ind = mean(total_ind),
            std_total_ind = sd(total_ind)) %>% 
  ggplot(., aes(x = day, y = average_total_ind, group = treatment)) +
  geom_line(aes(color = treatment)) +
  labs(x = "Days", y = "Total number of individuals") +
  scale_x_continuous(limits = c(0, 20))


# An effect of riboflavin on the number of instars before the 4th stage?
# Here riboflavin increases the number of 1-2-3rd instars
feeding_data %>% 
  dplyr::filter(stage != "4th_instar") %>%
  dplyr::filter(stage != "eggs") %>%
  ggplot(., aes(x = treatment, y = total_ind, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.1)

# An effect of riboflavin on the 4th instar number?
feeding_data %>% 
  dplyr::filter(stage == "4th_instar") %>% 
  ggplot(., aes(x = treatment, y = total_ind, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.1) +
  facet_wrap(~ date)


# plot_line <- ggplot(feeding_data, aes(day, ratio, colour = treatment)) +
#  stat_summary(fun = mean, geom = "point") + 
#   stat_summary(fun = mean, geom = "line", aes(group = treatment)) + 
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=0.2) + 
#   labs(x = "days", y = "ratio nymphs/eggs", colour = "treatment") +
#   theme_classic()+theme(axis.title =element_text(size=8))
# 
# plot_line
# 
# 
# plot_4line <- ggplot(feeding_data, aes(day, ratio_4, colour = treatment))
# fourth_line<-plot_4line + stat_summary(fun = mean, geom = "point") + 
#   stat_summary(fun = mean, geom = "line", aes(group = treatment)) + 
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=0.2) + 
#   labs(x = "days", y = "ratio fourth instars/eggs", colour = "treatment") +
#   theme_classic()+theme(axis.title =element_text(size=8))
# 
# fourth_line
