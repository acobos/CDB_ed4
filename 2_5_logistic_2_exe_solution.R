# data
d <- rio::import("https://raw.githubusercontent.com/acobos/Datasets/master/Mysterious_event.txt",
                 stringsAsFactors  = TRUE)

lapply(d, levels)

# previous exercise ----
m1 <- glm(Survival ~ Class, family = binomial, data = d)
summary(m1)


# m1: Prob(survived) = f(Class, Age, Sex) ----
m2 <- glm(Survival ~ Class + Age + Sex, family=binomial, data=d)

# class with lowest survival probability, given sex and age
summary(m2)
# class 3 has lowest coef, so lowest exp(b), lowest odds, and lowest prob

# OR of survival for females vs males, given class and age
exp(m2$coef)["Sexmale"]         # OR males vs females
1 / exp(m2$coef)["Sexmale"]     # OR females vs males


# Compare both models ----
anova(m1,m2, test='LRT')
# m2 has significantly better fit than m1, because it provides a 
# significant reduction in deviance
