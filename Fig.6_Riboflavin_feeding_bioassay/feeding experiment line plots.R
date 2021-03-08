
feeding_data<-read.csv("data.csv", header=T, sep = ",", dec = ".")

library(ggplot2)
library(Hmisc)


plot_line <- ggplot(feeding_data, aes(day, ratio, colour = treatment))
total_line<-plot_line + stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = treatment)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=0.2) + 
  labs(x = "days", y = "ratio nymphs/eggs", colour = "treatment") +
  theme_classic()+theme(axis.title =element_text(size=8))

total_line


plot_4line <- ggplot(feeding_data, aes(day, ratio_4, colour = treatment))
fourth_line<-plot_4line + stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = treatment)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=0.2) + 
  labs(x = "days", y = "ratio fourth instars/eggs", colour = "treatment") +
  theme_classic()+theme(axis.title =element_text(size=8))

fourth_line
