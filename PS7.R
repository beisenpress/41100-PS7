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

#install.packages("nnet")
library(MASS)
library(nnet)

# Convert Stars variable from numeric to character
yelp$stars1 <- factor(yelp$stars)

# Create new dataset without numeric stars variable
yelp1 <- yelp[,2:ncol(yelp)]

# Do Ordered logistic regression
yelp.reg1 <- polr(stars1 ~ ., data=yelp1, Hess=TRUE)
yelp.reg2 <- polr(stars1 ~ . - review_count, data=yelp1, Hess=TRUE)

summary(yelp.reg1)
summary(yelp.reg2)

# We cannot run a logistic regression using the number of reviews because that uniquely 
# identifies restaurants.

# (c) Using your favorite model selection technique construct a model to predict star ratings, 
# considering main effects and interactions, but excluding review count. Use a test/training 
# approach to validate your model and compare it to the others you’ve fit (use MSE to compare 
# models). Interpret your findings.

# Remove review count from dataset
yelp2 <- yelp1[,2:ncol(yelp1)]

# Set seed so the results are replicable 
set.seed(94)

# Select a random sample of rows
samples <- sort(sample.int(nrow(yelp2), 0.80*nrow(yelp2)))

# Subset the data into training and test datasets.
yelp2.train <- yelp2[samples,] 
yelp2.test <- yelp2[-samples,]

# Use forward stepwise selection under BIC to select a model.

#Create the null regression
yelp.reg.null <- polr(stars1 ~ 1, data=yelp2.train, Hess=TRUE)

#Create the full regression
yelp.reg.full <- polr(stars1 ~ . + .^2, data=yelp2.train, Hess=TRUE)

# Run forward stepwise BIC
yelp.reg.BIC <- step(yelp.reg.null, scope=formula(yelp.reg.full), direction="forward", k=log(nrow(yelp2)))

# Forward stepwise regression takes a long time to run.

# Final regression is: 

# Call:
#   polr(formula = stars1 ~ Parking.street + Parking.lot + accepts.credit + 
#          Good.For.latenight + Take.out + open + Outdoor.Seating + 
#          Good.For.brunch + Parking.valet + Good.For.dessert + Parking.garage + 
#          Good.For.breakfast + accepts.credit:Take.out + Parking.street:Outdoor.Seating + 
#          Parking.lot:Outdoor.Seating + Parking.street:Parking.lot + 
#          Parking.street:Parking.valet + Take.out:Good.For.breakfast + 
#          Outdoor.Seating:Good.For.breakfast + Parking.lot:Good.For.brunch + 
#          Parking.street:open + Parking.lot:open, data = yelp2.train, 
#        Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error t value
# Parking.streetTRUE                          0.63620    0.12774  4.9805
# Parking.lotTRUE                             0.36859    0.09412  3.9163
# accepts.creditTRUE                          0.43678    0.25677  1.7011
# Good.For.latenightTRUE                     -0.58981    0.06718 -8.7797
# Take.outTRUE                                1.02711    0.27044  3.7979
# openTRUE                                   -0.05003    0.08006 -0.6249
# Outdoor.SeatingTRUE                         0.38376    0.05971  6.4274
# Good.For.brunchTRUE                         0.61282    0.10177  6.0215
# Parking.valetTRUE                           0.59066    0.09689  6.0960
# Good.For.dessertTRUE                        0.51660    0.12404  4.1647
# Parking.garageTRUE                         -0.21898    0.06700 -3.2682
# Good.For.breakfastTRUE                     -1.39697    0.24963 -5.5962
# accepts.creditTRUE:Take.outTRUE            -1.55111    0.27720 -5.5956
# Parking.streetTRUE:Outdoor.SeatingTRUE     -0.53183    0.09466 -5.6183
# Parking.lotTRUE:Outdoor.SeatingTRUE        -0.33494    0.07198 -4.6532
# Parking.streetTRUE:Parking.lotTRUE         -0.35813    0.12467 -2.8725
# Parking.streetTRUE:Parking.valetTRUE       -0.79777    0.22520 -3.5425
# Take.outTRUE:Good.For.breakfastTRUE         1.08206    0.25387  4.2622
# Outdoor.SeatingTRUE:Good.For.breakfastTRUE  0.42599    0.11217  3.7978
# Parking.lotTRUE:Good.For.brunchTRUE        -0.50496    0.13873 -3.6399
# Parking.streetTRUE:openTRUE                 0.59302    0.12530  4.7328
# Parking.lotTRUE:openTRUE                    0.44020    0.09466  4.6503
# 
# Intercepts:
#   Value    Std. Error t value 
# 1|1.5  -6.7333   0.4100   -16.4232
# 1.5|2  -4.3608   0.2796   -15.5989
# 2|2.5  -2.8745   0.2664   -10.7913
# 2.5|3  -1.5702   0.2635    -5.9587
# 3|3.5  -0.3688   0.2630    -1.4024
# 3.5|4   0.9330   0.2631     3.5464
# 4|4.5   2.6773   0.2642    10.1334
# 4.5|5   5.1269   0.2761    18.5688
# 
# Residual Deviance: 40664.20 
# AIC: 40724.20 

# Predict star ratings for the test data
# This gives a matrix of the probabilities for each star rating for each restaurant

# First predict the BIC model
yelp.predBIC <- predict(yelp.reg.BIC, newdata=yelp2.test, type="probs")

# Then predict the model with all variabels
yelp.pred2 <- predict(yelp.reg2, newdata=yelp1[-samples,], type="probs")


# Now, select the probabilities of each restaurant getting the star rating it did.
# For example, if a restaurant got 3 stars, we want to extract the predicted probabliy of 3 stars
# Store these predictions in the "pred" variable
for(i in 1:nrow(yelp2.test)){
  yelp2.test[i,"pred.BIC"] <- yelp.predBIC[i,yelp2.test[i,"stars1"]]
  yelp2.test[i,"pred2"] <- yelp.pred2[i,yelp2.test[i,"stars1"]]
}

# Count number of predictions where our prediction is "good"
# Define good as better than random chance, i.e. better than 1/9
sum(yelp2.test$pred.BIC > (1/9))
sum(yelp2.test$pred2 > (1/9))

# Of 3134 observations in the test data, the BIC regression predicts 2,638 better than random chance.
# The regression with all variables only predicts 2,616 better than random chance.
# Therefore, the BIC regression is better.

