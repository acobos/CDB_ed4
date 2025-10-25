library(tidyverse)

# data ----
d <- rio::import("http://www.statsci.org/data/oz/ms212.txt")
head(d)


# missings ?
colSums(is.na(d))

# subset to drop missings and create a factor group
d <- d %>% 
  na.omit() %>% 
  mutate(group = factor(Ran, levels=c(2,1), labels=c("Sat","Ran")))

colSums(is.na(d))
levels(d$group)


# linear model for Pulse2 with group as predictor ----

# look at data first!
library(ggformula)
gf_boxplot(Pulse2 ~ group, data = d)  +
  stat_summary(fun.y=mean, geom="point", color="red") +
  coord_flip()

# model and summary
m1 <- lm(Pulse2 ~ group, data = d)
summary(m1)

# did running have an effect on the pulse rate?
# Yes: a very significant p-vaue for groupRan

# by how much the pulse is affected by running? provide a 95 CI
coef(m1)
confint(m1)

# predicted value of Pulse2 for students that sat
# since Sat is the reference level, the Intercept
coef(m1)[1]

# predicted values of Pulse2 for those who Ran:
# the Intercept plus the estimated coefficient for groupRan
coef(m1)[1] + coef(m1)[2]
sum(coef(m1))               # same (because only these two coeffs)


# Verify by computing the means of Pulse2 in both groups
library(mosaic)
mean(Pulse2 ~ group, data = d)

# model with both group and Pulse1 as predictors ----

# look at data first!
gf_point(Pulse2 ~ Pulse1, col= ~ group, data = d)

# test for a possible interaction between predictors
m2 <- lm(Pulse2 ~ group + Pulse1 + group:Pulse1, data = d)
summary(m2) 

# same, excluding outlier in Ran
m2 <- lm(Pulse2 ~ group + Pulse1 + group:Pulse1, data = d, 
         subset = Pulse1 < 125)
summary(m2) 

# still not significant, so remove interaction (and keep outlier)
m3 <- lm(Pulse2 ~ group + Pulse1, data = d)
summary(m3) 

# what is now the estimated effect of running on the pulse rate? provide 95% CI
coef(m3)[2]
confint(m3)[2,]

# compare to model 1
coef(m1)[2]
confint(m1)[2,]



