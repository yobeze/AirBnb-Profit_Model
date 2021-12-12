# Setting up ---------------------------------
setwd("C:/Users/vidal/Documents/college/CSCI 4502 data mining/project data")
airbnb <- read.csv("listings_cleaned.csv")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("usdm")
#install.packages("dummies")
#install.packages("car")
install.packages('MuMIn')
library(lme4)
library(lmerTest)
library("usdm")
library(tidyverse)
library(dummies)
library(MuMIn)
library(car)
library(ggplot2)


# More cleanup ---------------------------------

#subset
use_df <- load_df[c(1:3000),]

# log transforming very right skewed data to satisfy normality assumption
airbnb$log_price <- log(airbnb$price)


# reducing size of data frame
df <- airbnb[,-c(1:8, 10,11,13:17)]
df <- df[,-c(4:11,14)]
df <- df[,-c(12,11)]
drops <- c("license","calendar_updated", "has_availability", "calendar_last_scraped",
           "calculated_host_listings_count", "calculated_host_listings_count_entire_homes",
           "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms")
df2 <- df[ , !(names(df) %in% drops)]
str(df2)

# get just the year that the host joined
df2$host_since <- substr(df2$host_since, 1, 4)
df2$host_since <- as.integer(df2$host_since)
df2$host_since <- 2021 - df2$host_since
colnames(df2)[which(names(df2) == "host_since")] <- "host_yrs_exp"

# the number of amenities the listing has
df2$amenity_qty <- length(df2$amenities[1])
# need to count number of items in amenities column for each row -- fixing this later

# dummy coding
df2_dc <- dummy.data.frame(df2, 
                           names = c("shared_private", "host_is_superhost", "room_type"),
                           sep = ".")
colnames(df2_dc)[which(names(df2_dc) == "shared_private.private")] <- "bathroom_private"
colnames(df2_dc)[which(names(df2_dc) == "shared_private.shared")] <- "bathroom_shared"
colnames(df2_dc)[which(names(df2_dc) == "room_type.Entire home/apt")] <- "room_entire"
colnames(df2_dc)[which(names(df2_dc) == "room_type.Hotel room")] <- "room_hotel"
colnames(df2_dc)[which(names(df2_dc) == "room_type.Private room")] <- "room_private"
colnames(df2_dc)[which(names(df2_dc) == "room_type.Shared room")] <- "room_shared"
str(df2_dc)

# neighborhood factor
df2_dc$fNBHD <- factor(df2$neighbourhood_cleansed)

# renaming to make it easier to remember
use_df <- df2_dc

# coding property type into bigger categories bc that's easier
# 2 = entire [whatever], 1 = private [part/room of whatever], 0 = other
unique(use_df$property_type)
use_df$entire_prop <- ifelse(grepl("Entire", use_df$property_type, ignore.case = T), 2, 
                          ifelse(grepl("Private", use_df$property_type, ignore.case = T), 1, 0))

# amenities
use_df$has_wifi <- ifelse(grepl("Wifi", use_df$amenities, ignore.case = T), 1, 0)
use_df$has_parking <- ifelse(grepl(paste(c("free", "parking"), collapse="|"), use_df$amenities, ignore.case = T), 1, 0)
use_df$has_heating <- ifelse(grepl("heating", use_df$amenities, ignore.case = T), 1, 0)
use_df$has_ac <- ifelse(grepl("air conditioning", use_df$amenities, ignore.case = T), 1, 0)
use_df$has_tv <- ifelse(grepl("TV", use_df$amenities, ignore.case = F), 1, 0)
use_df$has_fridge <- ifelse(grepl("refrigerator", use_df$amenities, ignore.case = T), 1, 0)
use_df$has_pool <- ifelse(grepl("Pool", use_df$amenities, ignore.case = T), 1, 0)


# Checking assumptions ---------------------------------

#### normality of transformed y: ok
par(mfrow=c(1,2))
hist(use_df$price, 
     main="Before transformation",
     xlab = "Listing price")
hist(use_df$log_price, 
     main="After transformation",
     xlab = "Log listing price")
shapiro.test(airbnb$log_price) #says it's not normal, but lm robust to non-normality when n is large


# just looking at relationships b/w response and individual predictors
plot(log_price~accommodates, data = use_df)
plot(log_price~beds, data = use_df)
plot(log_price~bedrooms, data = use_df)
plot(log_price~n_bathrooms, data = use_df)
plot(log_price~n_bathrooms, data = use_df)
plot(log_price~number_of_reviews, data = use_df)
plot(price~host_yrs_exp, data = use_df)
plot(log_price~room_private, data = use_df)
plot(log_price~entire_prop, data = use_df)


# export csv
write.csv(use_df,"C:/Users/vidal/Documents/college/CSCI 4502 data mining/project data\\listings_proc2.csv", row.names = FALSE)
