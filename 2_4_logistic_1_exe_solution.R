# data
d <- rio::import("https://raw.githubusercontent.com/acobos/Datasets/master/Mysterious_event.txt",
                 stringsAsFactors  = TRUE)

lapply(d, levels)

# Survival by class ----
# Data exploration
library(mosaic)
tally(Survival ~ Class, data = d)
tally(Survival ~ Class, data = d, format="percent")


# m1: Prob(survived) = f(Class)
m1 <- glm(Survival ~ Class, family = binomial, data = d)

# class with lowest survival probability?
summary(m1)
# class S has lowest coef, so lowest exp(b), lowest odds, and lowest prob

# OR of survival for class S vs 1
exp(m1$coefficients)
exp(m1$coefficients)["ClassS"]

# OR of survival for class 1 vs S
1 / exp(m1$coefficients)["ClassS"]

# Extra:  verify that 
# - exp(intercept) is the odds of Survival for class 1
# - exp(coef) for class S is the OR for class S vs class 1
p <- tally(Survival ~ Class, data = d, format="percent")
p

p[2,1]/p[1,1]                       # odds for class 1
exp(m1$coef)["(Intercept)"]

(p[2,4]/p[1,4]) / (p[2,1]/p[1,1])   # OR class S vs 1
exp(m1$coef)["ClassS"]                        

