# Display structure of mtcars
str(mtcars)

# Convert cyl and am to factors
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Define positions
posn.d <- position_dodge(width=0.1)
posn.jd <- position_jitterdodge(jitter.width=0.1, dodge.width=0.2)
posn.j <-position_jitter(width=0.2)

# Base layers
wt.cyl.am <-ggplot(mtcars, aes(x=cyl, y=wt, group=am, col=am, fill=am))
wt.cyl.am

# wt.cyl.am, posn.d, posn.jd and posn.j are available

# Plot 1: Jittered, dodged scatter plot with transparent points
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)

# Plot 2: Mean and SD - the easy way
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)+
  stat_summary(fun.data = mean_sdl, 
               position =posn.d,
               fun.args = list(mult=1))

# Plot 3: Mean and 95% CI - the easy way
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)+
  stat_summary(fun.data=mean_cl_normal,
               position =posn.d,
               fun.args = list(mult=1))

# Plot 4: Mean and SD - with T-tipped error bars - fill in ___
wt.cyl.am +
  stat_summary(geom = 'point', fun.y = mean,
               position = posn.d) +
  stat_summary(geom = 'errorbar', fun.data = mean_sdl,
               position = posn.d, fun.args = list(mult = 1), width = 0.1)

# Play vector xx is available
xx
# Function to save range for use in ggplot
gg_range <- function(x) {
  # Change x below to return the instructed values
  data.frame(ymin = min(x), # Min
             ymax = max(x)) # Max
}

gg_range(xx)
# Required output
#   ymin ymax
# 1    1  100

# Function to Custom function
med_IQR <- function(x) {
  # Change x below to return the instructed values
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}

med_IQR(xx)
# Required output
#        y  ymin  ymax
# 25% 50.5 25.75 75.25


# The base ggplot command; you don't have to change this
wt.cyl.am <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am))

# Add three stat_summary calls to wt.cyl.am
wt.cyl.am +
  #1
  stat_summary(geom = 'linerange', 
               fun.data = med_IQR,
               position = posn.d, 
               size = 3) +
  #2
  stat_summary(geom = 'linerange', 
               position = posn.d, 
               size = 3,
               alpha = 0.4,
               fun.data = gg_range) +
  #3
  stat_summary(geom = 'point', fun.y = median,
               position = posn.d, size = 3,
               col = 'black', shape = 'X')


# Basic ggplot() command, coded for you
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + geom_point() + geom_smooth()

# Add scale_x_continuous()
p + scale_x_continuous(limits=c(3,6), expand=c(0,0))

# Add coord_cartesian(): the proper way to zoom in
p+coord_cartesian(xlim=c(3,6))

# Create a stacked bar plot: wide.bar
wide.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
  geom_bar()


# Convert wide.bar to pie chart
wide.bar +
  coord_polar(theta = 'y')

# Create stacked bar plot: thin.bar
thin.bar <- ggplot(mtcars, 
                   aes(x = 1, 
                       fill = cyl)) +
  geom_bar(width = 0.1) +
  scale_x_continuous(limits = c(0.5,1.5))



# Convert thin.bar to "ring" type pie chart
thin.bar + 
  coord_polar(theta = 'y')


# Basic scatter plot
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# 1 - Separate rows according to transmission type, am
p +
  facet_grid(am ~ .)

# 2 - Separate columns according to cylinders, cyl
p +
  facet_grid(. ~ cyl)

# 3 - Separate by both columns and rows 
p +
  facet_grid(am ~ cyl)


# Code to create the cyl_am col and myCol vector
mtcars$cyl_am <- paste(mtcars$cyl, mtcars$am, sep = "_")
mtcars

myCol <- rbind(brewer.pal(9, "Blues")[c(3,6,8)],
               brewer.pal(9, "Reds")[c(3,6,8)])

# Map cyl_am onto col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() +
  # Add a manual colour scale
  scale_color_manual(values = myCol)


# Grid facet on gear vs. vs
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am)) +
  geom_point() +
  # Add a manual colour scale
  scale_color_manual(values = myCol) +
  facet_grid(gear ~ vs)


# Also map disp to size
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl_am, size=disp)) +
  geom_point() +
  # Add a manual colour scale
  scale_color_manual(values = myCol) +
  facet_grid(gear ~ vs)

# Basic scatter plot
p <- ggplot(mamsleep, aes(x = time, y = name, col = sleep)) +
  geom_point()

# Execute to display plot
p

# Facet rows accoding to vore
p +
  facet_grid(vore ~ .)

# Specify scale and space arguments to free up rows
p +
  facet_grid(vore ~ ., 
             scale= 'free_y', 
             space = 'free_y')


# Starting point
z

# Plot 1: Change the plot background fill to myPink
z +
  theme(plot.background = element_rect(fill = myPink))

# Plot 2: Adjust the border to be a black line of size 3
z +
  theme(plot.background = element_rect(fill = myPink, colour='black', size=3))


# Theme to remove all rectangles
no_panels <- theme(rect = element_blank())

# Plot 3: Combine custom themes
z +
  no_panels +
  theme(plot.background = element_rect(fill = myPink)) # from plot 2




# Extend z using theme() function and 3 args
z+theme(panel.grid=element_blank(),
        axis.line=element_line(colour="red"),
        axis.ticks=element_line(colour='red'))


# Original plot, color provided
z
myRed

# Extend z with theme() function and 3 args
z +
  theme(strip.text = element_text(size = 16, color = myRed),
        axis.title = element_text(color = myRed, hjust = 0, face = "italic"),
        axis.text = element_text(color = "black"))


# Move legend by position
z +
  theme(legend.position = c(0.85, 0.85))

# Change direction
z +
  theme(legend.direction= 'horizontal')

# Change location by name
z +
  theme(legend.position = 'bottom')

# Remove legend entirely
z +
  theme(legend.position = 'none')


library(grid)

# Increase spacing between facets
z + theme(panel.spacing.x= unit(2, 'cm'))

# Adjust the plot margin
z + theme(panel.spacing.x= unit(2, 'cm'),
          plot.margin= unit(c(1,2,1,1), "cm"))


# Original plot
z2


# Theme layer saved as an object, theme_pink
theme_pink <- theme(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = myPink, color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "red"),
                    axis.ticks = element_line(color = "red"),
                    strip.text = element_text(size = 16, color = myRed),
                    axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")

# 1 - Apply theme_pink to z2
z2 + theme_pink

# 2 - Update the default theme, and at the same time
# assign the old theme to the object old.
old <- theme_update(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = myPink, color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "red"),
                    axis.ticks = element_line(color = "red"),
                    strip.text = element_text(size = 16, color = myRed),
                    axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")

# 3 - Display the plot z2 - new default theme used
z2

# 4 - Restore the old default theme
theme_set(old)

# Display the plot z2 - old theme restored
z2


# Original plot
z2

# Load ggthemes
library(ggthemes)

# Apply theme_tufte(), plot additional modifications
custom_theme <- theme_tufte() +
  theme(legend.position = c(0.9,0.9),
        legend.title = element_text(face='italic', size=12),
        axis.title = element_text(face='bold', size=14))

# Draw the customized plot
z2 + custom_theme

# Use theme set to set custom theme as default
theme_set(custom_theme)

# Plot z2 again
z2




