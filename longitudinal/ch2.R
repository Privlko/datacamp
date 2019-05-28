library(nlme)
library(lme4)
library(tidyverse)

View(BodyWeight) 

ggplot(BodyWeight, aes(x = Time, y = weight)) +  
  geom_line(aes(group = Rat), alpha = 0.6) + 
  geom_smooth(se = FALSE, size = 2) + 
  theme_bw(base_size = 16) +  
  xlab("Number of Days") +  
  ylab("Weight (grams)")



B1 <- mutate(BodyWeight, Time = Time - 1)

body_ri <- lmer(weight ~ 1 + Time + (1 | Rat), data = B1)

summary(body_ri)


B2 <- BodyWeight %>%
  mutate(Time = Time - 1,
         diet_f = paste("Diet", Diet, sep = " "))


view(B2)

body_weight <- lmer(weight ~ 1 + Time + diet_f + 
                      (1 + Time  | Rat), data = B2)

summary(body_weight)



bodyweight_agg <- B2 %>%
  mutate(pred_values = predict(body_weight, re.form = NA)) %>%
  group_by(Time, Diet) %>%
  summarize(mean_diet_pred = mean(pred_values))

head(B2)
head(bodyweight_agg)


ggplot(bodyweight_agg, aes(x = Time, y = mean_diet_pred, color = Diet)) + 
  geom_point(data = BodyWeight, aes(x = Time, y = weight)) +
  geom_line(size = 2) + 
  ylab("Body Weight") + 
  xlab("Time (in days)") +
  theme_bw(base_size = 16)
