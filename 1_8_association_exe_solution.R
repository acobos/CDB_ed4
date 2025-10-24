# Solutions to exercise 9.1

library(dplyr)
library(ggformula)

# It is reasonable to expect that the physical constitution of newborns will be 
# similar to that of their mothers, so that high-weight mothers will tend to have 
# heavier children than a low-weight mothers.

# If this was true, would you expect a direct or an inverse relation of birthweights
# and mother’s weights? 
# answer: A direct relation is expected: the higher the mother's weight, the higher 
#         the birthweight.


# Use the birthwt dataset in package MASS to answer the following questions.

# Compute a new variable expressing the weight of mothers in kilograms (lwt_kg)
d <- MASS::birthwt %>%
  mutate(lwt_kg =  round(lwt * 0.453592))          # convert pounds to kg


# Produce a scatterplot to see if birthweights are related to the mother’s weights. 
# Are there any outliers?
gf_point(bwt ~ lwt_kg, data = d, alpha = 0.3)
# A weak direct relation is seen in the scatterplot.
# Some outliers in the higher end of the mother's weight distribution


# Compute the Pearson’s and the Spearman’s correlation coefficients. 
# Which one would you choose and why?
mosaic::cor(bwt ~ lwt, data = d)                           # Pearson's r
mosaic::cor(bwt ~ lwt, data = d, method="spearman")        # Spearman's r
# Better Spearman's r, because there are some outliers


# With the one you have chosen, test the null hypothesis that the correlation 
# is zero in the population. Since a direct relation is expected, what would be 
# the appropriate alternative hypothesis for a one-sided test?
# answer:  a one sided alternative better; because we expect a positive relation,
#          the appropriate alternative is:      H_A: rho > 0.
res <- mosaic::cor.test(bwt ~ lwt, data = d, 
                        method="spearman",           
                        alternative = "greater",
                        exact=FALSE)

# Conduct a one-sided test
# We did it above, let's see the results
res        


# What is the p-value? 
res$p.value
res$p.value %>% scales::pvalue()    # formatted as it should be reported (3 decimals)

# Does it provide evidence of a direct relation of birthweight and the mother’s weight?
# yes, it does.
