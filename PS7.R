# 41100 PS7

setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Homework 7/41100-PS7")

################## Question 1 ######################
#1 Can observational studies replicate experiments?

# Revisit the National Supported Work (NSW) and Panel Study of Income Dynamics (PSID) data, just as 
# in homework 5 (see there for description). We have treated observations from the NSW and control 
# observations from the PSID. Now we will use a more sophisticated (I didn’t say better) approach 
# to trying to use observational data to recover the experimental benchmark causal effect.

# What is the fundamental problem? We want to estimate the benefit that the 185 men got, on 
# average, from going through the treatment. In lecture 5 we used the difference between the 
# mean in the treated group and the mean in the control group to estimate this average treatment 
# effect:

# bt = Bt = Yt=1 - Yt=0

# We can still find the mean in the treated group, Y ̄T=1, but we lack a control group to fill the
# second piece. In homework 5 we saw that the PSID control sample was not a good stand-in for a 
# real randomized control group. (Most formal research refers to the PSID as a “comparison” group 
# for just this reason.)

# The key insight we exploit here is that we don’t have to generate a perfect control sample, we 
# just need a good stand-in for Y ̄T=0, which is an average. More precisely, the number we need is
# the average income the 185 men would have had if they were instead assigned to the control group.

# In the following steps we will re-weight the PSID data so that it acts like a good control 
# sample on average using the information in the X variables. We will use a technique called 
# weighted least squares. Least squares linear regression solves

# min 1/m  (Yi − Xi′b)^2

# If each observation has a weight Wi attached, then we can use weighted least squares, which solves

# min 1/n Wi(Yi−Xi′b)^2

# (a) Estimate P[T = 1|X] (called the “propensity score”) using the full data. Justify your 
# choice of control variables. Note: the outcome income.after can not be used to predict treatment.

labor <- read.csv("nsw_psid.csv")

# Do a logistic regression

# Create a null and full model for stepwise selection
labor.prob.null <- glm(treat ~ 1, family=binomial, data=labor)
labor.prob.full <- glm(treat ~ age + black + hispanic + education + hsdegree, family=binomial, data=labor)

# Run stepwise selection
labor.prob.AIC <- step(labor.prob.null, scope=formula(labor.prob.full), direction="forward", k=2)
labor.prob.BIC <- step(labor.prob.null, scope=formula(labor.prob.full), direction="forward", k=log(nrow(labor)))

summary(labor.prob.AIC)
summary(labor.prob.BIC)

# Both AIC and BIC pick black, age, hsdegree and hispanic.  Use those.

# (b) Define weights for each observation as
# Wi = 1/(1−P[T=1|X])
# and run weighted least squares on the PSID data. (Look at the weights option in lm.)

# Add predicted probabilities to dataset
labor$prob <- labor.prob.AIC$fitted.values

# Run regression using predicted probabilities as weights.
# Use the X variables we selected in PS5.
labor.reg1 <- lm(income.after ~ 
                   treat + income.before1 + income.before2 + education + black + married + age, 
                 data = labor, weights = prob)

# Run a regression without weights for comparison.
labor.reg2 <- lm(income.after ~ 
                   treat + income.before1 + income.before2 + education + black + married + age, 
                 data = labor)

summary(labor.reg1)
summary(labor.reg2)

# (c) Use your regression to predict, for each treated observation, what their income would 
# have been had they been assigned to control.

# Create subset of data with only treated individuals
labor.t <- labor[which(labor$treat == 1),]

# Manually set treat equal to zero
labor.t$treat <- 0

# Use predict what income would have been without treatment
labor.t$income.after.wo.t <- predict(labor.reg1, newdata=labor.t)

# Calculate average treatment effect
mean(labor.t$income.after) - mean(labor.t$income.after.wo.t)

# Average treatment effect is $1,368.  This compares to an ATE of $978 we found in homework 5.

###################### Question 2 #################################
# 2 Predicting Restaurant Ratings
# From the website Yelp we have star ratings of 15,668 restaurants (file yelp.csv), total number 
# of reviews, and various characteristics of the restaurant taken from the reviews themselves 
# (e.g. type of parking, types of meals the place is good for, etc). The goal is to predict how 
# many stars a restaurant will get based on characteristics.

yelp <- read.csv("yelp.csv")

# (a) Is the outcome variable stars suited for our method for count data? Why or why not? If not, 
# can you transform it so it is well-suited?

# No, it is not suited to count data.  It is ordered, but not a count. Restaurants don't start
# with one star and work their way up to 5.  A restaruant can have 5 stars on its opening day.
# To transform it, we could create a new variable for total number of stars given,
# where Total number of stars given = Average Rating * Number of Reviews

# (b) Model stars as a function of all the characteristics of the restaurant using the 
# appropriate generalized linear model, both with and without the total number of reviews 
# (review_count) as a control variable. Comment on any differences in the conclusions from these 
# two models.

# install.packages("nnet")
library(MASS)
library(nnet)

# Do Ordered logistic regression
yelp.reg1 <- polr(stars ~ Price.Range + Delivery, data=yelp, Hess=TRUE)
