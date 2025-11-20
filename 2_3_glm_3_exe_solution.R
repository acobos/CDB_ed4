library(tidyverse)
library(ggformula)

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

# boxplots
gf_boxplot(fev ~ smoke, data = d) + coord_flip()
gf_boxplot(age ~ smoke, data = d) + coord_flip()
gf_point(fev ~ age, col = ~ smoke, data = d)

# 1st model
model1 <- lm(fev ~ smoke, data = d)
summary(model1)

# 2nd model
model2 <- lm(fev ~ smoke + age, data = d)
summary(model2)

# Clearly, age is a confounder for smoke in this data



# Exercise 2 ------------------------------------------------------------------

# subset non smokers
dns <- filter(d, smoke == "no")

# Fit a model for FEV with height and gender as  explanatory variables
model1 <- lm(fev ~ height + gender, data = dns) 

# model summary 
summary(model1)

# plot the data and overlay the model
gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model1) ~ height, col = ~ gender, data = dns) 


# assess model assumptions graphically
plot(model1, 1)      # shows non-linearity
plot(model1, 2)      # reasonable approximation to Normality
                     # some outliers

# does a polynomial for height improve the fit?
model2 <- lm(fev ~ poly(height,2) + gender, data = dns) 

summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
anova(model1, model2)  # significant improvement in fit


# show model graphically 
gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model2) ~ height, col = ~ gender, data = dns) 

# assess assumptions
plot(model2, 1)      # no non-linearity, but heteroscedasticity 
plot(model2, 2)


# model improvement
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

anova(model1, model2)          # significant improvement in fit


# log transformed FEV
model3 <- lm(log(fev) ~ height + gender, data = dns) 
summary(model3)

# assess assumptions
plot(model3, 1)      # no non-linearity + homoscedasticity 
plot(model3, 2)

# show model graphically 
gf_point(log(fev) ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(predict(model3) ~ height, col = ~ gender, data = dns) 

gf_point(fev ~ height, col = ~ gender, data =dns, alpha = 0.3)  %>% 
  gf_line(exp(predict(model3)) ~ height, col = ~ gender, data = dns) 

