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

