library(tidyverse)


# Read & prepare data 

library(GLMsData)
data(lungcap)                                            # get the dataset

d <- lungcap %>% 
  janitor::clean_names() %>%
  mutate(height = round(2.54 * ht),                       # converts from inches to cm
         gender = as.factor(gender),                      # creates factor for gender
         smoke = factor(smoke,                            # creates factor for smoke
                        levels = 0:1,
                        labels = c("no", "yes"))) %>% 
  select(-ht)                                             # drops ht

rm(lungcap)                                               # removes lungcap


# Exercise 1 ------------------------------------------------------------------

# Get a summary of dataset variables
summary(d)


# subset non smokers
dns <- filter(d, smoke == "no")

# Fit a model for FEV with height and gender as  explanatory variables
model1 <- lm(fev ~ height + gender, data = dns) 
summary(model1)

library(ggformula)
gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model1) ~ height, col = ~ gender, data = dns) 

plot(model1, 1)
plot(model1, 2)

# Fit a new model using a 2nd degree polynomial for `height`
model2 <- lm(fev ~ poly(height,2) + gender, data = dns) 


# show model graphically 
gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model2) ~ height, col = ~ gender, data = dns) 


# assess assumptions
plot(model2, 1)
plot(model2, 2)


# model improvement
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

anova(model1, model2)


# refit with smokers
model2 <- lm(fev ~ poly(height,2) + gender + smoke, data = d) 
summary(model2)

gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model2) ~ height, col = ~ gender, data = dns) 


# Exercise 2 ------------------------------------------------------------------


# boxplots
gf_boxplot(fev ~ smoke, data = d) + coord_flip()
gf_boxplot(age ~ smoke, data = d) + coord_flip()


# 1st model
model1 <- lm(fev ~ smoke, data = d)
summary(model1)

# 2nd model
model2 <- lm(fev ~ smoke + age, data = d)
summary(model2)
