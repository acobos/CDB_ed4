# birth weight data in package MASS ----
library(tidyverse)

d <- MASS::birthwt %>% 
  select(low, race) %>% 
  mutate(race = factor(race, levels = 1:3, labels = c("white", "black", "other")),
         low = factor(low, levels = c(1,0), labels = c("Low", "Normal"))) %>% 
  rename(bw=low)

head(d)   


# low birth weight related to race? ----
library(mosaic)

# the contingency table (saving it to x)
bw_race <- tally(bw ~ race, data = d)
bw_race


# see percentages of low/normal in each race
tally(bw ~ race, data = d, format = "percent") %>% round(1)


# same, graphically
library(ggformula)
gf_props( ~ race, fill = ~ bw, position = "fill", data = d)


# chisquare test
res <- chisq.test(bw_race)
res

# review expected frequencies to confirm they are not too low (e.g., <5)
res$expected          



# compute RR for low weight in black vs white
white_vs_black <- bw_race %>% t() %>% Epi::twoby2(print = FALSE)

white_vs_black

white_vs_black$measures

white_vs_black$measures[1,]                  # pick row 1

1/white_vs_black$measures[1,]                # invert results



# compute RR for low weight in other vs white
race_bw <- bw_race %>% t()

race_bw

race_bw[-2,]                   # and proceed as before... or 

race_bw[c(3,1),]               #pick rows 3 and 1

race_bw[c(3,1),] %>% Epi::twoby2()

other_vs_white <- race_bw[c(3,1),] %>% Epi::twoby2(print = FALSE)

other_vs_white

other_vs_white$measures[1,]


# Last, merge black and other into a single category
d <- mutate(d, race_2 = ifelse(d$race == "white", "white", "black/other"))

tally(race ~ race_2, data = d)                    # verify

tally(race_2 ~ bw, data=d)

bo_vs_w <- tally(race_2 ~ bw, data=d) %>% Epi::twoby2(print = FALSE)

bo_vs_w$measures[1,]
