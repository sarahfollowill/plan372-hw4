# PLAN372 - HW4
# Sarah Followill

# First, we need to load libraries
library(tidyverse)
library(dplyr)
library(tidycensus)

# Next, we'll load our data
airport_pairs = read_csv("airport_pairs.csv")

# Q1
# Create a table of the existing flights to or from RDU, and the number of 
# passengers passenger traveling to each destination. Make sure to include both 
# flights departing RDU and those arriving RDU. What is the most popular non-stop destination from RDU?
# Filter your table to only include airport pairs with more 10,000 passengers. 
airport_pairs_RDU = filter(airport_pairs, origin == "RDU" | dest == "RDU")
# Just select origin, destination, and passenger columns
airport_pairs_RDU = airport_pairs_RDU[,1:3]
# Only show airport pairs with > 10,000 passengers
airport_pairs_RDU = filter(airport_pairs_RDU, passengers > 10000)

# Arrange by passengers from highest to lowest and view table
airport_pairs_RDU = arrange(airport_pairs_RDU, -passengers)
view(airport_pairs_RDU)



# Q2
# Access census data, total population for each cbsa
census = get_acs(
  geography="cbsa",  # could be tract, block group, etc.
  variables=c(
    total_pop="B01001_001",
    female_22_24="B01001_034"
  ),
  year=2019,
  survey="acs5",
  output="wide",
  geometry=T
)

# Take away margin of error columns 
# rename the cbsa column in in the census data to match what its called in airport_pairs
census = census[,-4]
colnames(census)[1] = "origin_cbsa"

# Merge airport_pairs and census data by cbsa
# Result will attach the total pop to each origin cbsa
origin_pop <- merge(airport_pairs, 
                       census, 
                       by = "origin_cbsa")

# Make a copy object of the census data to join destination data
census_dest = census
# Rename the cbsa column in in the census_dest data to match what its called in airport_pairs
colnames(census_dest)[1] = "dest_cbsa"

# Merge airport_pairs and census data by cbsa
# Result will attach the total pop to each destination cbsa
dest_pop <- merge(airport_pairs, 
                    census_dest, 
                    by = "dest_cbsa")

#Check for null values
sum(is.na(origin_pop$origin_cbsa))
sum(is.na(dest_pop$dest_cbsa))
# No null values

# Create table showing CBSA to CBSA passenger volumes
cbsa2cbsa = origin_pop[,c(1,9,4)]
view(cbsa2cbsa)

# Simple Scatterplot - Origin
attach(origin_pop)
plot(total_popE, passengers, main="Origin Total Population and Passengers",
     xlab="Total Origin Population ", ylab="Total Passengers ", pch=19, cex=0.3)

# Simple Scatterplot - Destination
attach(dest_pop)
plot(total_popE, passengers, main="Destination Total Population and Passengers",
     xlab="Total Destination Population ", ylab="Total Passengers ", pch=19, cex=0.3)

# Simple Scatterplot - Distance
attach(origin_pop)
plot(distancemiles, passengers, main="Flight Distance and Passengers",
     xlab="Total Distance Miles ", ylab="Total Passengers ", pch=19, cex=0.3)


# Extra Credit: add another variable --> Females 22-24 at Origin and Destination
# Added command to census data retrieval above

# Simple Scatterplot - Female 22-24 at Origin
attach(origin_pop)
plot(female_22_24E, passengers, main="Females Age 22-24 at Origin and Passengers",
     xlab="Females Age 22-24 ", ylab="Total Passengers ", pch=19, cex=0.3)

# Simple Scatterplot - Female 22-24 at Destination
attach(dest_pop)
plot(female_22_24E, passengers, main="Females Age 22-24 at Destination and Passengers",
     xlab="Females Age 22-24 ", ylab="Total Passengers ", pch=19, cex=0.3)


# Q3

# Making single variable models first to practice and visualize
# Origin CBSA and Passengers
# Evaluating effect of total pop on passengers
origin_svm = lm(passengers~total_popE, origin_pop)
# to see the results of our model, we can run summary()
summary(origin_svm)

# Destination CBSA and Passengers
# Evaluating effect of total pop on passengers
dest_svm = lm(passengers~total_popE, dest_pop)
# to see the results of our model, we can run summary()
summary(dest_svm)

# Distance of Flight and Passengers
# Evaluating effect of distance on passengers
distance_svm = lm(passengers~distancemiles, origin_pop)

# to see the results of our model, we can run summary()
summary(distance_svm)


# Create a multiple regression model with origin, destination, and distance
# Create new dataset that has origin pop, dest pop, and distance
origin_dest_dist = origin_pop
# Change origin total pop column name
colnames(origin_dest_dist)[12] = "origin_total_pop"
# Add destination total pop data
origin_dest_dist <- merge(origin_dest_dist, 
                  census_dest, 
                  by = "dest_cbsa")
# Change dest total pop column name
colnames(origin_dest_dist)[17] = "dest_total_pop"

# Create Multiple Regression Model
multiple_regression_m = lm(passengers~origin_total_pop+dest_total_pop+distancemiles, origin_dest_dist)

# to see the results of our model, we can run summary()
summary(multiple_regression_m)

# Create Extra Credit Multiple Regression Model
# Delete duplicate columns
origin_dest_dist_EC = origin_dest_dist

# Write the regression model
multiple_regression_EC = lm(passengers~origin_total_pop+dest_total_pop+distancemiles+female_22_24E.x, origin_dest_dist_EC)

# to see the results of our model, we can run summary()
summary(multiple_regression_EC)



# Q4
# Create new table of potential new routes, their origins/destinations/distances/total populations
new_routes = tibble(
  origin=c("RDU", "RDU","RDU","RDU", "PDX", "ELP", "TLH", "SAN"),
  destination=c("PDX", "ELP", "TLH", "SAN", "RDU", "RDU","RDU","RDU"),
  distancemiles=c(2363, 1606, 496, 2193, 2363, 1606, 496, 2193),
  origin_total_pop=c(1332311, 1332311, 1332311, 1332311, 2445761, 840477, 382197, 3316073),
  dest_total_pop=c(2445761, 840477, 382197, 3316073, 1332311, 1332311, 1332311, 1332311)
)

# Now, we can use predict() to create a new column in new_routes with the
# forecasted passenger totals. It should be close to what we found above (a little different
# because we used rounded coefficients above but predict() uses the original,
# un-rounded coefficients)
new_routes$forecast_passengers = predict(multiple_regression_m, new_routes)
view(new_routes)

