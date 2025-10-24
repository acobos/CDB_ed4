library(ggformula)
library(MKinfer)

# data
d <- rio::import("data/erosive_esophagitis.xls")

head(d)


# explore distributions of cost by treatment
gf_boxplot(cost ~ treatment, data = d) %>% 
  gf_summary(fun = mean, geom = "point", color = "red") %>% 
  gf_refine(coord_flip())


library(MKinfer)

# permutation test
set.seed(1)                            # for reproducibility
perm.t.test(cost ~ treatment, data = d)


# bootstrap
set.seed(1)                            # for reproducibility
boot.t.test(cost ~ treatment, data = d)
