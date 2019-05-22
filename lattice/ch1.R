library(lattice)
library(tidyverse)
library(dplyr)

airquality


?lattice
USAccDeaths

iri1 <- tbl_df(iris)


histogram(~Sepal.Length, data=iri1)


histogram(~ Ozone, data = airquality)

xyplot(Ozone ~ Solar.R, data = airquality)



histogram(~ Ozone, data = airquality, nint = 14, grid=TRUE)
# Use the 'airquality' dataset
str(airquality)

# Create the histogram
histogram(~Ozone, data = airquality, 
          # Specify number of bins
          nint = 15,
          # Specify quantity displayed on y-axis
          type = 'count' )


# Use the 'airquality' dataset
str(airquality)

# Create scatterplot
xyplot(Ozone ~ Temp, data = airquality,
       # Add main label
       main = 'Environmental conditions in New York City (1973)', 
       # Add axis labels
       xlab = 'Temperature (Fahrenheit)',
       ylab = 'Ozone (ppb)')


# Use the 'airquality' dataset
str(airquality)

# Create a density plot
densityplot(~ Ozone, data = airquality, 
            # Choose how raw data is shown
            plot.points = 'jitter')


USCancerRates <- mutate(USCancerRates, 
                          +     state.ordered = reorder(state, rate.male, median, na.rm = TRUE))


bwplot(state.ordered ~ rate.male, data = USCancerRates)

# 'USCancerRates' is pre-loaded
str(USCancerRates)

# Create reordered variable
library(dplyr)
USCancerRates <-
  mutate(USCancerRates, 
         state.ordered = reorder(state, rate.female, median, na.rm = TRUE))

# Create box and whisker plot
head(USCancerRates)

bwplot(state.ordered ~ rate.female, data = USCancerRates)


# 'USCancerRates' is pre-loaded
str(USCancerRates)

tbl_df(USCancerRates)
# Create box and whisker plot
bwplot(state.ordered ~ rate.female, data = USCancerRates, 
       # Change whiskers extent
       coef = 0)

