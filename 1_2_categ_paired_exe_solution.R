library(tidyverse)
library(mosaic)

# reading the data
d <- rio::import("data/bta.xls")
head(d)


# BTA Sens & Spec
tally(bta ~ cystoscopy, data=d)
tally(bta ~ cystoscopy, data=d, format="percent")

# Cytology Sens & Spec
tally(cytology ~ cystoscopy, data=d)
tally(cytology ~ cystoscopy, data=d, format="percent")


# compare sensitivities
x <- tally(bta ~ cytology, data=d, subset = cystoscopy == "Positive")
x
mcnemar.test(x)

# compare specificities
x <- tally(bta ~ cytology, data=d, subset = cystoscopy == "Negative")
x
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


tally(fct_rev(bta) ~ fct_rev(cystoscopy), data=d) %>% ThresholdROC::diagnostic()

tally(fct_rev(cytology) ~ fct_rev(cystoscopy), data=d) %>% ThresholdROC::diagnostic()

