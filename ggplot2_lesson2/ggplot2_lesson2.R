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

# Base layers
m <- ggplot(mtcars, aes(x = cyl, y = wt))

# Draw dynamite plot
m +
  stat_summary(fun.y = mean, geom = 'bar', fill = 'skyblue') +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Base layers
m <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am))

# Plot 1: Draw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Plot 2: Set position dodge in each stat function
m +
  stat_summary(fun.y = mean, geom = "bar", position = 'dodge') +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.1, position = 'dodge')

# Set your dodge posn manually
posn.d <- position_dodge(0.9)

# Plot 3: Redraw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar", position = posn.d) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = posn.d)


mtcars.cyl

# Base layers
m <- ggplot(mtcars.cyl, aes(x = cyl, y = wt.avg))

# Plot 1: Draw bar plot with geom_bar
m + geom_bar(stat = 'identity', fill = 'skyblue')

# Plot 2: Draw bar plot with geom_col
m + geom_col(fill = 'skyblue')

# Plot 3: geom_col with variable widths.
m + geom_col(fill = 'skyblue',  width = mtcars.cyl$prop)

# Plot 4: Add error bars
m +
  geom_col(fill = 'skyblue',  width = mtcars.cyl$prop) +
  geom_errorbar(aes(ymin = wt.avg - sd,
                    ymax = wt.avg + sd), width = 0.1)



##pie charts

#categorical variable as the proportion of another categorical variable

# Bar chart
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "fill")

# Convert bar chart to pie chart
ggplot(mtcars, aes(x = factor(1), fill = am)) +
  geom_bar(position = "fill") +
  facet_grid(. ~ cyl)   + # Facets
  coord_polar(theta = 'y')  + # Coordinates
  theme_void() # theme


# Create color palette
myColors <- brewer.pal(9, "Reds")

# Build the heat map from scratch
ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() + # Geom layer
  facet_wrap( ~ site, ncol = 1) + # Facet layer
  scale_fill_gradientn(colors= myColors ) # Adjust colors


str(barley)

# The heat map we want to replace
# Don't remove, it's here to help you!
myColors <- brewer.pal(9, "Reds")

ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() +
  facet_wrap( ~ site, ncol = 1) +
  scale_fill_gradientn(colors = myColors)

# Line plot; set the aes, geom and facet

ggplot(barley, aes(x = year, y = yield, col = variety, group = variety)) +
  geom_line() +
  facet_wrap( ~ site, nrow = 1)


# Create overlapping ribbon plot from scratch
str(barley)


ggplot(barley, aes(x=year,
                   y=yield,
                   col=site,
                   group=site,
                   fill=site))+
  stat_summary(fun.y=mean,
               geom='line')+
  stat_summary(fun.data=mean_sdl,
               fun.args = list(mult=1),
               geom='ribbon', col=NA, alpha=0.1)


# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))


DF
# Create groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum

# The groupSum column needs to be removed; don't remove this line
DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)



# Melt the dataset
library(reshape2)
DF_melted <- melt(DF, id.vars = c('X', 'xmin', 'xmax'), variable.name = 'FILL')
DF_melted


# dplyr call to calculate ymin and ymax - don't change
library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

DF_melted

# Plot rectangles - don't change
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()



# Perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)
resid

# Change names of resid


colnames(resid) <- c('FILL', 'X', 'residual')
resid
# merge the two datasets:

head(resid)
head(DF_melted)

DF_all <- merge(DF_melted, resid)
head(DF_all)

# Update plot command
library(ggthemes)
ggplot(DF_all, aes(ymin = ymin,
                   ymax = ymax,
                   xmin = xmin,
                   xmax = xmax,
                   fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()


# Plot so far
p

# Position for labels on y axis (don't change)
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2


head(DF_all)
# Plot 1: geom_text for BMI (i.e. the fill axis)
p1 <- p %+% DF_all +
  geom_text(aes(x = max(xmax),
                y = yposn,
                label = FILL),
            size = 3, hjust = 1,
            show.legend  = FALSE)
p1

# Plot 2: Position for labels on x axis
DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2





# geom_text for ages (i.e. the x axis)
p1 %+% DF_all +
  geom_text(aes(x = xposn, label = X),
            y = 1, angle = 90,
            size = 3, hjust = 1,
            show.legend = FALSE)
