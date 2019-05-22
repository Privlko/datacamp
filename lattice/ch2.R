library(lattice)


# 'USCancerRates' is pre-loaded
str(USCancerRates)

tbl_df(USCancerRates)
# Create box and whisker plot
bwplot(state.ordered ~ rate.female, data = USCancerRates, 
       # Change whiskers extent
       coef = 0)


# The airquality dataset has been pre-loaded
str(airquality)

# Create a histogram
histogram(~Ozone | factor(Month),
          data = airquality, 
          # Define the layout
          outer=T, layout=c(2,3),
          # Change the x-axis label
          xlab="Ozone (ppb)")

# USCancerRates has been pre-loaded
str(USCancerRates)

# Create a density plot
densityplot(~rate.male+rate.female, data = USCancerRates, 
            outer = TRUE,
            # Suppress data points
            plot.points=FALSE,
            # Add a reference line
            ref=TRUE)


# USCancerRates has been pre-loaded
str(USCancerRates)

# Create a density plot
densityplot(~rate.male + rate.female, data = USCancerRates,
            # Set value of 'outer' 
            outer=FALSE,
            # Add x-axis label
            xlab="Rate (per 100,000)",
            # Add a legend
            auto.key=T,
            plot.points = FALSE)
            
            
            # The airquality data frame is pre-loaded
str(airquality)
            
xyplot(Ozone ~ Temp, airquality, groups = Month,
                   # Complete the legend spec 
                   auto.key = list(space = "right", 
                                   title = "Month", 
                                   text = month.name[5:9]))
