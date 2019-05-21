# Run a mixed-effects model of bone mineral density vs.
# an intercept, the visit and the treatment group
bmd_group <- lmer(bmd ~ 1 + visit + group + (1 + visit | person),
                  data = calcium)

# View summary
summary(bmd_group)


# Modify the formula to include age as a fixed effect
bmd_group_age <- lmer(bmd ~ 1 + visit + group + age + (1 + visit | person),
                      data = calcium)

# View summary
summary(bmd_group_age)



# Calculate aggregate trends
calcium_agg <- calcium %>%
  mutate(pred_values = predict(bmd_group_age, re.form = NA)) %>%
  group_by(visit, group) %>%
  summarize(predicted_bmd = mean(pred_values))

head(calcium)
# See the results
calcium_agg


# Calculate aggregate trends
calcium_agg <- calcium %>%
  mutate(pred_values = predict(bmd_group_age, re.form = NA)) %>%
  group_by(visit, group) %>%
  summarize(predicted_bmd = mean(pred_values))

# Plot visit on x-axis, color by group
ggplot(calcium_agg, aes(x = visit, color = group)) +
  # Add points with actual bmd on y-axis
  geom_point(aes(y = bmd), data = calcium) +
  # Add lines with predicted bmd on y-axis
  geom_line(aes(y = predicted_bmd), size = 1.25) +
  xlab('Visit Number') +
  ylab('Model Predicted Bone Mineral Density (g/cm^2)')




# Calculate aggregate trends
calcium_agg <- calcium %>%
  mutate(pred_values = predict(bmd_group_age, re.form = NA)) %>%
  group_by(visit, group) %>%
  summarize(predicted_bmd = mean(pred_values))

# Plot visit on x-axis, color by group
ggplot(calcium_agg, aes(x = visit, color = group)) +
  # Add points with actual bmd on y-axis
  geom_point(aes(y = bmd), data = calcium) +
  # Add lines with predicted bmd on y-axis
  geom_line(aes(y = predicted_bmd), size = 1.25) +
  xlab('Visit Number') +
  ylab('Model Predicted Bone Mineral Density (g/cm^2)')