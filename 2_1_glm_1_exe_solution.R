library(tidyverse)
library(ggformula)

# data: drop var year and compute factor day (as Work/Sat/Sun)
library(mosaic)
d <- Births78 %>% 
  select(-year) %>% 
  mutate(day = ifelse(wday %in% c("Sun", "Sat"),                  
                      as.character(wday),                         
                      "Work"),
         day = factor(day, levels = c("Work", "Sat", "Sun")))     
head(d)


# the previous model, with month
mod_1 <- lm(births ~ day + poly(month, 5), data=d)
summary(mod_1)

# a new model with day of year (instead of month)
mod_2 <- lm(births ~ day + poly(day_of_year, 5), data=d)
summary(mod_2)

# save predicted values from the new model
d$pred <- predict(mod_2)



# plot observed and predicted values for new model
gf_point(births ~ date, col= ~ day, data = d) %>%
  gf_line(pred ~ date, col = ~day)   


# new model looks smoother variation along time
# also, slightly better fit:  Ajusted R-squared  
round(summary(mod_1)$r.squared, 3)
round(summary(mod_2)$r.squared, 3)


# Prefered model ----

# What is the model intended for?
# - Explanation: model based on day of year is better
# - Prediction:  model based on month is more practical for predictions


