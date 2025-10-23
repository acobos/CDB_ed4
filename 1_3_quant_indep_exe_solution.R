library(tidyverse)

# Australian athletes data in package DAAG ----
library(DAAG)
data(ais)
head(ais)

# first, see boxplot by sex
library(ggformula)

gf_boxplot(hc ~ sex, data = ais)  %>% 
  gf_summary(fun = mean, geom = "point", color = "red") %>% 
  gf_refine(coord_flip())

# distributions are about symmetrical; an outlier in males,
# but mean and median very similar, so parametric test is OK.
# more variability in females than in males, so Welch test.
t.test(hc ~ sex, data = ais)

# by how much, on average?
res <- t.test(hc ~ sex, data = ais)
names(res)

# by how much, on average?
res$estimate
res$estimate[1]                      # mean in group f
res$estimate[2]                      # mean in group m
res$estimate[1] - res$estimate[2]    # their difference (f-m)

res$conf.int                         # the CI (and confidence level)


# Wilcoxon RST
wilcox.test(hc ~ sex, data = ais)




# Total choleterol and sex (HTA data subset) ----
d <- rio::import("data/hta.xlsx") %>% 
  filter(!is.na(sbp_v1)) %>% 
  select(sex, total_c) %>% 
  mutate(sex = factor(sex, 1:2, c("male", "female"))) %>% 
  na.omit()

gf_boxplot(total_c ~ sex, data = d)  %>% 
  gf_summary(fun = mean, geom = "point", color = "red") %>% 
  gf_refine(coord_flip())


# A hardly credible far outlier in females
t.test(total_c ~ sex, data = d)
wilcox.test(total_c ~ sex, data = d)


# see what happens if we remove this case
t.test(total_c ~ sex, data = filter(d, total_c > 100))



