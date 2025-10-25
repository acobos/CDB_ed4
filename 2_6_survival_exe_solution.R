# load packages needed 
library(tidyverse)
library(survival)
library(survminer)

# access ovarian dataset
head(ovarian)

# data preparation ----
d <- ovarian %>% 
  # define factors
  mutate(rx = factor(rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B")),
         resid.ds = factor(resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes")),
         ecog.ps = factor(ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad")),
  # dichotomize age 
         age_group = factor(ifelse(age >=50, "old", "young")))

head(d)

# create survival object ----
surv_object <- Surv(d$futime, d$fustat)
surv_object 


# Fit survival curves for each treatment group using the Kaplan-Meier method ----
fit1 <- survfit(surv_object ~ rx, data = d)

# plot the curves with risk table
ggsurvplot(fit1, data = d, risk.table = TRUE)

# median survival time
fit1

# survival at 1 and 2 years
summary(fit1, time = c(365, 730))

# log-rank test
survdiff(surv_object ~ rx, data = d) 

# Examine predictive value of residual disease status ----
fit2 <- survfit(surv_object ~ resid.ds, data = d)
ggsurvplot(fit2, data = d, pval = TRUE, pval.method = TRUE)




