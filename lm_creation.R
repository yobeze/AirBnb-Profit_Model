# Setting up ---------------------------------
setwd("C:/Users/vidal/Documents/college/CSCI 4502 data mining/project data")
getwd()
library(lme4)
library(lmerTest)
library("usdm")
library(tidyverse)
library(dummies)
library(MuMIn)
library(car)
library(ggplot2)
use_df <- read.csv("listings_proc2.csv") # the csv I saved after doing cleanup

# First round LMs: variable significance ---------------------------------
model_all <- lm(log_price ~ accommodates+bedrooms+beds+n_bathrooms+bathroom_private+
                  room_private+entire_prop+
                  has_ac+ has_fridge + has_wifi + has_heating + has_parking + has_tv+
                  host_yrs_exp+host_is_superhost.t+
                  number_of_reviews+review_scores_rating+
                  review_scores_location+review_scores_cleanliness,data = use_df)
sm_all <- summary(model_all)
sm_all

# including an interaction term for accommodates*bedrooms
# because usually single bedrooms are more desirable than having roommates
model_reduced <- lm(log_price ~ accommodates+bedrooms+(accommodates*bedrooms)+n_bathrooms+bathroom_private+
                      room_private+ room_hotel + room_entire+
                      has_ac+ has_heating + has_parking + has_tv+
                      host_yrs_exp+
                      number_of_reviews,data = load_df)
sm_reduced <- summary(model_reduced)
sm_reduced

# with neighborhood as a random effect (each neighborhood has its own intercept)
model_nbhd <- lmer(log_price ~ accommodates+bedrooms+n_bathrooms+(accommodates*bedrooms)+bathroom_private+
                     room_private+ room_hotel + room_entire+
                     has_ac+has_heating + has_parking + has_tv+
                     host_yrs_exp+
                     number_of_reviews +
                     (1|fNBHD), data = use_df, REML = FALSE)
sm_nbhd <- summary(model_nbhd)
sm_nbhd
# observations: beds is insignificant, most review scores are insignificant, heating and ac are important amenities
# superhost status doesn't matter but years experience does (negative coefficient, though) <- may change when
# some columns are removed

# Second round LMs: train-test split ---------------------------------
train <- read.csv("train_set.csv")

tr_mdl_red <- lm(log_price ~ accommodates+bedrooms+(accommodates*bedrooms)+n_bathrooms+bathroom_private+
                      room_private+
                      has_ac+ has_heating + has_parking + has_tv+
                      host_yrs_exp+
                      number_of_reviews,data = train)
sm_tr_rd <- summary(tr_mdl_red)
sm_tr_rd

# Evaluation  and diagnostics ---------------------------------

# model_all
hist(resid(tr_mdl_red))
{qqnorm(resid(tr_mdl_red))
  qqline(resid(tr_mdl_red))}
# some deviation from the line on QQ plot
res1 <- resid(tr_mdl_red, type = "pearson")
train[which(abs(res1) > 2.5),]

# Collinearity: VIF
# values greater than 5 might be a problem, but scores don't look too bad surprisingly
vif(model_all)
vif(model_reduced)
vif(tr_mdl_red)

# mean squared error
mse <- function(sm) mean(sm$residuals^2)
mSE_all <- mse(sm_all)
mSE_red <- mse(sm_reduced)
mSE_nbhd <- mse(sm_nbhd) #very very very bad
mSE_tr_red <- mse(tr_mdl_red)

# R^2 for how much variation in price is explained by model
MuMIn::r.squaredGLMM(model_nbhd)


#### visualization of model predictions versus actual prices
ggplot(load_df, aes(x=predict(model_reduced), y= log_price, color="coral1")) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', 
       y='Actual Values', 
       title='Predicted vs. Actual Log Listing Price')

