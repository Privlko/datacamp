###longitudinal data

library(nlme)
library(tidyverse)
library(lme4)

head(BodyWeight)

count(BodyWeight, Time)

count(BodyWeight, Diet)



##calcium example
# Individuals with data at each visit number
calcium

head(calcium)

count(calcium, visit)

# Individuals in each group
count(calcium, group)
count(calcium, visit, group)

BodyWeight %>%
  mutate(Time = paste0('Time_', Time)) %>%
  spread(Time, weight) %>%
  select(Rat, Diet, Time_1, Time_8, everything())

