library(MASS)
library(ISLR)# install.packages('ISLR') if you don't have it
data(Boston)

lm.fit <- lm(medv ~ rm + crim + age, data=Boston)
# inference:
summary(lm.fit)

plot(Boston$rm,Boston$medv,pch=16)
abline(lm.fit,col='red')

###
trainingloc <- sample(1:nrow(Boston), 0.8*nrow(Boston))
training <- Boston[trainingloc,]

testloc <- setdiff(1:nrow(Boston),trainingloc) # this picks observations not in trainingloc
test <- Boston[testloc,]

# train the model on the training sample
trained_model <- lm(medv~rm,data=training)

# predict home values in the test sample
preds <- trained_model$coefficients[2]*test$rm + trained_model$coefficients[1]

# calculate "error squared". Larger number means we are "off" more.
errorsq <- (test$medv - preds)^2

# calculcate Mean Squared Error (MSE) which is a single number that you want to be as small as possible
mse <- sum(errorsq)/nrow(test)
mse


# add a second variable?
# check MSE see if it goes down
# train the model on the training sample
trained_model <- lm(medv~rm + crim,data=training)

# predict home values in the test sample
preds <- trained_model$coefficients[3]*test$crim + trained_model$coefficients[2]*test$rm + trained_model$coefficients[1]

# calculate "error squared". Larger number means we are "off" more.
errorsq <- (test$medv - preds)^2

# calculcate Mean Squared Error (MSE) which is a single number that you want to be as small as possible
mse <- sum(errorsq)/nrow(test)
mse

# scatterplot with predicted ys and actual ys
plot(test$rm,test$medv,pch=16,col='black')
par(new=TRUE)
plot(test$rm,preds,pch=16,col='red')
