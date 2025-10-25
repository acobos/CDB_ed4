library(tidyverse)

# US 1978 bank holidays 
h <- as.Date(c("1978-01-02", "1978-02-20", "1978-05-29", "1978-07-04", 
               "1978-09-04", "1978-11-11", "1978-11-23", "1978-12-25"))

# drop var year and compute:
# - factor day (as Work/Sat/Sun)
# - new var holiday (yes/no)
library(mosaic)

d <- Births78 %>% 
  select(-year, -day_of_week) %>% 
  mutate(day = ifelse(wday %in% c("Sun", "Sat"),                  
                      as.character(wday),                         
                      "Work"),
         day = factor(day, levels = c("Work", "Sat", "Sun")),
         holiday = ifelse(date %in% h & day == "Work",
                    "Yes", 
                    "No"))

head(d)


# the two models ----
model_1 <- lm(births ~ day + holiday + poly(day_of_year,5) , data = d)
model_2 <- lm(births ~ day + holiday + poly(day_of_year,5) + day_of_month, data = d)

# R-squared
summary(model_1)$r.squared
summary(model_2)$r.squared

# Adjusted R-squared
summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared

# because they are nested models:
anova(model_1, model_2, test="LRT")

# adding the day of month does not improve the fit

