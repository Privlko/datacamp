
ru_data <- readRDS("C:/Users/Ivan/Desktop/dir/papers/ru_mobility/ru_data.rds") %>% 
  select(round, idind, wage, gender)

head(ru_data)

count(ru_data, round)

count(ru_data, idind)
count(ru_data, idind, round)

tbl_df(ru_data) %>%
  mutate(round = paste0('round_', round)) %>%
  spread(round, wage) %>%
  select(idind, everything())


##descriptive statistics
ru_data %>%
  group_by(round) %>%
  summarize(mean_wage = mean(wage, na.rm = TRUE),
            med_wgt = median(wage, na.rm = TRUE),
            min_wgt = min(wage, na.rm = TRUE),
            max_wgt = max(wage, na.rm = TRUE),
            sd_wgt = sd(wage, na.rm = TRUE),
            num_miss = sum(is.na(wage)),
            n = n())

ggplot(ru_data, aes(x = factor(round), y = wage)) + 
  geom_violin(aes(fill = gender)) + 
  xlab("Round") + 
  ylab("Wage") + 
  scale_y_log10()+
  theme_bw(base_size = 16)


ru_data %>% 
  filter(idind < 100) %>% 
  ggplot(aes(x = round, y = wage)) +  
  geom_line(aes(group = idind), alpha = 0.6) + 
  geom_point(aes(group = idind)) +
  geom_smooth(se = FALSE, size = 2) + 
  theme_bw(base_size = 16) +  
  xlab("Round") +  
  ylab("Wage")



##model thinking of time

library(nlme) 
library(dplyr)
library(lme4)

rumod <- mutate(ru_data, round = round - 20)
rumod

m1 <- lmer(wage ~ 1 + round + (1 | idind), data = rumod)

summary(m1)
