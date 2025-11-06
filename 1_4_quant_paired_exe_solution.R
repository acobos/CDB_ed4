library(tidyverse)
library(ggformula)


# Read data and compute differences
fileURL <- "https://raw.githubusercontent.com/acobos/Datasets/master/Life_Expectancy_with_continent.csv"
d <- rio::import(fileURL) %>% 
  mutate(difference = LE_women - LE_men)
head(d)

# inspection of distribution of differences
gf_boxplot(difference ~ "", data=d, xlab="", ylab="difference women - men)")  %>% 
  gf_summary(fun = mean, geom="point", color="red")  %>% 
  gf_refine(coord_flip())

# slight asymmetry, no outliers, n = 191
## t-test is OK
t.test(d$difference)
t.test(d$LE_women, d$LE_men, paired = TRUE)        # equivalent

# Provide some estimate of the difference
res <- t.test(d$LE_women, d$LE_men, paired = TRUE) 
names(res)
res$estimate
res$conf.int

paste(round(res$estimate,1),
      " (95% CI: ",
      paste(round(res$conf.int,1), collapse = ", "),
      ")", sep="")
