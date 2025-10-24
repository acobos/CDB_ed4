library(tidyverse)
library(mosaic)
library(ggformula)

# data
d <- MASS::birthwt %>% 
  mutate(race = factor(race, levels = 1:3, labels = c("white", "black", "other")),
         low = factor(low, levels = c(1,0), labels = c("Low", "Normal")),
         smoke = factor(smoke, levels = c(1,0), labels = c("smoker", "non_smoker")),
         # mother's weight: convert pounds to kg)
         lwt_kg = 0.453592 * lwt,  
         # ftv grouped
         ftv_grouped = factor(ftv, 
                              levels = c(0:4, 6),
                              labels = c("none", "one", rep("2++", 4))))

head(d, 4)        


# sample sizes 
tally(~ftv_grouped, data = d)


# Boxplot inspectionof birthweight
gf_boxplot(bwt ~ ftv_grouped, data = d) %>% 
  gf_summary(fun = mean, geom = "point", color = "red") %>% 
  gf_refine(coord_flip())

# Nothing to worry about: a single, non-influential outlier (n = 100).

# ANOVA
res <- aov(bwt ~ ftv_grouped, data = d)
anova(res)

# Better assessment of normality
d$residuals <- res$residuals         # save residuals to d
gf_qq(~residuals, data = d) %>%      # QQ-plot
  gf_qqline(col="red") 

# Normality test 
shapiro.test(d$residuals)         

# Homoscedasticity test
bartlett.test(bwt ~ ftv_grouped, data = d)    


# Mother's weight ----
gf_boxplot(lwt_kg ~ ftv_grouped, data = d) %>% 
  gf_summary(fun = mean, geom = "point", color = "red") %>% 
  gf_refine(coord_flip())

# clear asymmetry and outliers
kruskal.test(lwt_kg ~ ftv_grouped, data = d)


