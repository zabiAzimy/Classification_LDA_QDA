# performing LDA and QDA on Diabetes dataset

# load the required libraries
library(MASS)

# load the diabetes dataset
load("Diabetes.Rda")


# Split the data into a train/test with 2000 observations in the test data set
set.seed(50)
n <- dim(Diabetes)[1]
n
testidx <- sample(n, 2000)
test <- Diabetes[testidx, ]
train <- Diabetes[-testidx, ]


# linear discriminant analysis
lda.fit1 <- lda(YN ~ Age, data = train)

# assess the above model
library("pROC")

# using the test set let's make some predictions
pr1test <- predict(lda.fit1, newdata = test)

# we can access the posterior probability 
pr1test$posterior

roc.obj1 <- roc(test$YN, pr1test$posterior[, 2])
# plot the roc curve
ggroc(roc.obj1, lwd = 0.8, col = "darkgreen")

# find the area under the curve
auc(roc.obj1) #=======


# we can now fit models using other predictors
# using BMI this time
lda.fit2 <- lda(YN ~ BMI, data = train)

# using the test set let's make some predictions
pr2test <- predict(lda.fit2, newdata = test)

# we can access the posterior probability 
pr2test$posterior

roc.obj2 <- roc(test$YN, pr2test$posterior[, 2])
# plot the roc curve
ggroc(roc.obj2, lwd = 0.8, col = "orange")

# find the area under the curve
auc(roc.obj2)

# let's fit the LDA model again using Age and BMI
lda.fit3 <- lda(YN ~ BMI + Age, data = train)

# make predictions using the test set
pr3test <- predict(lda.fit3, newdata = test)

# again, posterior probabilities can be accessed
pr3test$posterior


# create roc object
roc.obj3 <- roc(test$YN, pr3test$posterior[, 2])

# using the roc object we can plot the ROC curve
ggroc(roc.obj3, lwd = 0.8, col = "red")

# the above plot looks good, it is close to the idea curve
# now let's find the area under the curve
auc(roc.obj3)

# ============== Quadratic discriminant analysis
qda.obj <- qda(YN ~ Age + BMI, data = train)

# test on the test set
pr4test <- predict(qda.obj, newdata = test)

# We can check the posterior probabilities
pr4test$posterior

# create the ROC object for it
roc.obj4 <- roc(test$YN, pr4test$posterior[, 2])

# Having the ROC object, we can plot the ROC curve
ggroc(roc.obj4, lwd = 0.8, col = "purple")

# Check the Area under the curve
auc(roc.obj4)


# ====== Which model gives the best AUC ====
# I will keep a note of the AUC for each of the above model:


# Linear discriminant analysis using Age as the predictor
# AUC : 0.8128

# Linear discriminant analysis using BMI as predictor
# AUC: 0.7488

# Linear discriminant analysis using BMI and Age 
# AUC: 0.8525

# Quadratic discriminant analysis using Age and BMI
# AUC: 0.8347


# storing all the AUC values in a vector
AUC_all <- c(0.8128, 0.7488, 0.8525, 0.8347)

# which value is the max?
max(AUC_all)


# The highest AUC belongs to linear quadratic analysis using Age and BMI
# Thus that is the best model so far

























