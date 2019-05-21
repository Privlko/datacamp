library(MASS)


# whiteside is in your workspace
str(whiteside)

# Plot whiteside data
plot(whiteside)

# Plot Gas vs. Temp

plot(whiteside$Temp, whiteside$Gas)


# Apply the plot() function to Insul
plot(whiteside$Insul)

# Cars93 is in your workspace
str(Cars93)

# Plot Max.Price vs. Price as red triangles
plot(Cars93$Price,Cars93$Max.Price, pch=17, col='red')

# Add Min.Price vs. Price as blue circles
points(Cars93$Price, Cars93$Min.Price,  pch=16, col='blue')


# Add an equality reference line with abline()
abline(a = 0, b = 1, lty = 2)

# Animals2 is in your workspace
str(Animals2)

# Set up the side-by-side plot array
par(mfrow=c(1,2))

# First plot: brain vs. body in its original form
plot(Animals2$body, Animals2$brain)
# Add the first title
title("Original representation")

# Second plot: log-log plot of brain vs. body
plot(Animals2$body, Animals2$brain, log='xy')


# Add the second title
title("Log-log plot")



## good exercise here

# dataCar is in your workspace
str(dataCar)

# Set up a side-by-side plot array
par(mfrow=c(1,2))

# Create a table of veh_body record counts and sort
tbl <- sort(table(dataCar$veh_body),
            decreasing = TRUE)

# Create the pie chart
pie(tbl)

# Give it a title
title('Pie chart')

# Create the barplot with perpendicular, half-sized labels
barplot(tbl, las = 2, cex.names = 0.5)

# Add a title
title('Bar chart')



# Set up a side-by-side plot array
par(mfrow=c(1,2))

# Create a histogram of counts with hist()
hist(Cars93$Horsepower, main='hist() plot')

# Create a normalized histogram with truehist()
truehist(Cars93$Horsepower, main = 'truehist() plot')





# Create index16, pointing to 16-week chicks
index16 <- which(ChickWeight$Time == 16)

# Get the 16-week chick weights
weights <- ChickWeight$weight[index16]

# Plot the normalized histogram
truehist(weights)

# Add the density curve to the histogram
lines(density(weights))


# Load the car package to make qqPlot() available
library(carData)
library(car)
# Create index16, pointing to 16-week chicks
index16 <- which(ChickWeight$Time == 16)
index16
# Get the 16-week chick weights
weights <- ChickWeight$weight[index16]
weights
# Show the normal QQ-plot of the chick weights
qqPlot(weights)



# Show the normal QQ-plot of the Boston$tax data
qqPlot(Boston$tax)



# Set up a side-by-side plot array
par(mfrow=c(1, 2))

# Create the standard scatterplot
plot(rad~zn, data=Boston)

# Add the title
title("Standard scatterplot")

# Create the sunflowerplot
sunflowerplot(rad~zn, data=Boston)

# Add the title
title("Sunflower plot")



# Create a variable-width boxplot with log y-axis & horizontal labels
boxplot(crim ~ rad, data=Boston, log='y', las=1, varwidth=TRUE)
# Add a title
title("Crime rate vs. radial highway index")


# Create a mosaic plot using the formula interface
mosaicplot(carb~cyl, data=mtcars)

# Load aplpack to make the bagplot() function available
library(aplpack)

# Create a side-by-side boxplot summary
boxplot(Cars93$Min.Price, Cars93$Max.Price)

# Create a bagplot for the same two variables
bagplot(Cars93$Min.Price, Cars93$Max.Price, cex = 1.2)

# Add an equality reference line
abline(a = 0, b = 1, lty = 2)

# Load the corrplot library for the corrplot() function
library(corrplot)

# Extract the numerical variables from UScereal
numericalVars <- UScereal[, 2:10]

# Compute the correlation matrix for these variables
corrMat <- cor(numericalVars)

# Generate the correlation ellipse plot
corrplot(corrMat, method = "ellipse")
