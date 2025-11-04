library(tidyverse)
library(mosaic)

# reading the data
d <- rio::import("bta.xls")
head(d)


# BTA Sens & Spec
tally(bta ~ cystoscopy, data=d)
tally(bta ~ cystoscopy, data=d, format="percent") %>% round(1)

# Cytology Sens & Spec
tally(cytology ~ cystoscopy, data=d)
tally(cytology ~ cystoscopy, data=d, format="percent") %>% round(1)


# compare sensitivities
d %>% 
  filter(cystoscopy == "Positive") %>%         # keep only these patients!
  tally(bta ~ cytology)                        # this gives an error...

# because arg data= in mosaic::tally() is not the first arg!
# can be done with the data=. trick
d %>% 
  filter(cystoscopy == "Positive") %>%         # keep only these patients!
  tally(bta ~ cytology, data=.)                

# convenient alternative
tally(bta ~ cytology, data=d, subset = cystoscopy == "Positive") %>% 
  mcnemar.test(x)

# compare specificities
tally(bta ~ cytology, data=d, subset = cystoscopy == "Negative") %>% 
  mcnemar.test(x)

# agreement
x <- tally(bta ~ cytology, data=d)
x

library(epiR)
epi.kappa(x)$prop.agree
epi.kappa(x)$kappa


# EXTRA: diagnostic accuracy measures with function ThresholdROC::diagnostic()

?ThresholdROC::diagnostic

tally(bta ~ cystoscopy, data=d)                       # not the table we need!

tally(fct_rev(bta) ~ fct_rev(cystoscopy), data=d)     # this table is ok

# accuracy measures for BTA test
tally(fct_rev(bta) ~ fct_rev(cystoscopy), data=d) %>% 
  ThresholdROC::diagnostic()

# accuracy measures for cytology
tally(fct_rev(cytology) ~ fct_rev(cystoscopy), data=d) %>% 
  ThresholdROC::diagnostic()

