###longitudinal data

library(nlme)
library(tidyverse)



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

tbl_df(BodyWeight) %>%
  mutate(Time = paste0('Time_', Time)) %>%
  spread(Time, weight) %>%
  select(Rat, Diet, Time_1, Time_8, everything())


BodyWeight %>% 
  mutate(Time = paste0('Time_', Time)) %>% 
  spread(Time, weight) %>% 
  select(Time_1, Time_8, Time_15:Time_64) %>% 
  correlate() %>%
  shave(upper = FALSE) %>%
  fashion(decimals = 3)


## descriptive statistics

BodyWeight %>%
  group_by(Time) %>%
  summarize(mean_wgt = mean(weight, na.rm = TRUE),
            med_wgt = median(weight, na.rm = TRUE),
            min_wgt = min(weight, na.rm = TRUE),
            max_wgt = max(weight, na.rm = TRUE),
            sd_wgt = sd(weight, na.rm = TRUE),
            num_miss = sum(is.na(weight)),
            n = n())


##visualising descriptives

ggplot(BodyWeight, aes(x = factor(Time), y = weight)) + 
  geom_violin(aes(fill = Diet)) + 
  xlab("Time (in days)") + 
  ylab("Weight") + 
  theme_bw(base_size = 16)
