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



# Fit a Cox proportional hazards model ----
fit_coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, data = d)

fit_coxph
summary(fit_coxph)

# plot HR's
ggforest(fit_coxph, data = d, fontsize = 1)

# verify PH assumption
cox.zph(fit_coxph)

