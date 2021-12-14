# I want to try using a dose-responce curve (drc) type of model for the 
# riboflavin feeding data.

library("checkpoint")
checkpoint("2021-01-01")

library(tidyverse)
library(broom)
library(drc)
library(modelr)
##attach(S.alba) # the data used in example
library(egg) 


# Loading the data
df <- read.csv("data.csv", 
               header = T, 
               sep = ",", 
               dec = ".", 
               stringsAsFactors = FALSE, 
               check.names = FALSE) %>% 
  mutate(total_ind = left + right) %>% 
  mutate(survival_rate = total_ind / start_eggs) %>% 
  # set first day of counting to d0
  mutate(day = day-1) %>% 
  # because do not know whether the numbers are eggs or instars
  #dplyr::filter(day != 2) %>%
  #dplyr::filter(day != 4) %>%
  # because we only need total instars
  dplyr::filter(stage != 'fourth_instar')


# I want to use the right model, so that it the first step. 
# The drc package has a function for selection the best fitting model,
# but needs an initial model to start with. I'll use LL.4
model.LL4<- drm(total_ind ~ day,
                data = df, 
                fct = LL.4 (names = c("Slope", 
                                      "Lower Limit", 
                                      "Upper Limit", 
                                      "ED50")))
mselect(model.LL4, 
        fctList = list (W1.3 (),
                        W1.4 (), 
                        W2.3 (), 
                        W2.4 (),  
                        LL.3 (),
                        MM.3 ())) 




# define drm function to use with map
drm.func <- function(x) {
  drm(total_ind ~ day, 
      fct = W2.3(names = c("Slope", "Upper Limit", "ED50")), 
      data = x)
}

predict.fun <- function(x) {
  add_predictions(data.frame(day = seq(0,30)), x)
}

coefs.fun <- function(x) {coef(x) %>% tidy}


df2 <- df %>% group_by(treatment) %>% nest() %>%
  mutate(drmod = map(data, drm.func), 
         pred = map(drmod, predict.fun),
         coefs = map(drmod, coefs.fun))

# plot raw data, model and ED50 line
df2 %>%
  unnest(data) %>% 
  ggplot() + 
  geom_point(aes(day, 
                 total_ind, 
                 color = treatment)) +
  geom_line(aes(day, 
                pred, 
                color = treatment), 
            data = df2 %>% 
              unnest(pred)) +
  geom_vline(aes(xintercept = x, 
                 color = treatment), 
             linetype = 5,
             data = df2 %>% 
               unnest(coefs) %>% 
               filter(names == "ED50:(Intercept)")) +
  theme_bw() 


df2 %>%
  ungroup(treatment) %>%
  purrr::map_dfr(
  df2~drmod,
  ~ as_tibble(confint(., "pred", level = 0.95, pool = TRUE))
)
df3 <- bind_cols(df2, test)

# try the tidydrc package
devtools::install_github("angelovangel/tidydrc")
library(tidydrc)



tidydrc_model(df, day, total_ind, model = W2.3(), treatment) %>%
  tidydrc_plot(ed50 = F, color = ~treatment, confint = T) + 
  labs(x = "days", y = "count") +
  ylim(0,160)

# summary of coefficients
df2 %>% unnest(coefs) %>% spread(names, x)
