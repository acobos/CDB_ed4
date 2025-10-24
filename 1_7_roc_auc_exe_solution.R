# Exercise on marker accuracy of subarachnoid hemorrhageprognosis
library(pROC)
data(aSAH)
head(aSAH)

# converting wfns (factor) to numeric
class(aSAH$wfns)
aSAH$wfns <- as.numeric(aSAH$wfns)
class(aSAH$wfns)
head(aSAH)
unique(aSAH$wfns)


# just to see distribution of wfns by outcome: clearly non-normal !
library(ggformula)
gf_boxplot(wfns ~ outcome, data = aSAH) +
  stat_summary(fun=mean, geom="point", color="red") + 
  coord_flip()


# ROC analysis
roc_ndka <- roc(outcome ~ ndka, data = aSAH)
roc_s100b <- roc(outcome ~ s100b, data = aSAH)
roc_wfns <- roc(outcome ~ wfns, data = aSAH)

# plot all three ROC curves

# using base R graphic system
plot(roc_ndka, col="blue")
plot(roc_s100b, col="darkgreen", add = TRUE)
plot(roc_wfns, col="red", add = TRUE)
legend("bottomright", 
       legend = c("ndka", "s100b", "wfns"), 
       col = c("blue", "darkgreen", "red"),
       lwd = 2)

# using ggplot2 graphic system
ggroc(list(ndka = roc_ndka, 
           s100b = roc_s100b,
           wfns = roc_wfns))  + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="darkgrey", linetype="dashed")
  

# compute AUC's and CI's using bootstrap (normal approximation not appropriate here)
auc(roc_ndka);  set.seed(1);  ci(roc_ndka, method = "bootstrap") 
auc(roc_s100b);  set.seed(1);  ci(roc_s100b, method = "bootstrap") 
auc(roc_wfns);  set.seed(1);  ci(roc_wfns, method = "bootstrap") 


# comparisons of wfns vs ndka and s100b
set.seed(1)
roc.test(roc_wfns, roc_ndka, method = "bootstrap")
set.seed(1)
roc.test(roc_wfns, roc_s100b, method = "bootstrap")

# ROC curve and optimal threshold
plot.roc(roc_wfns, col="red", 
         print.auc=TRUE, 
         print.thres = "best")

# see optimal threshold
aSAH %>% gf_boxplot(wfns ~ outcome) %>% 
  gf_hline(yintercept = 3.5, lty=2, col = "red")

