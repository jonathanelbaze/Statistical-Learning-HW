
setwd("~/Desktop/HEC/Session4/Advanced Statistical Learning/devoir_H2020/Statistical-Learning-HW")

train <- read.table('datrain.txt', header = TRUE, sep = ' ')
test <- read.table('dateststudent.txt', header = TRUE, sep = ' ')

summary(train)


train$y1 <- rep(0, nrow(train))
train$y2 <- rep(0, nrow(train))
train$y3 <- rep(0, nrow(train))

train$y1[train$y == 1] <- 1
train$y2[train$y == 2] <- 1
train$y3[train$y == 3] <- 1

train$y1 = factor(train$y1)
train$y2 = factor(train$y2)
train$y3 = factor(train$y3)

################################      Data Split       ##############################

# Splitting the data into a training (ntrain=1600) and a test (ntest=400) set

set.seed(123)
ntrain=1600
nval=nrow(train)-ntrain
idtrain=sample(1:nrow(train),ntrain,replace=FALSE)


winetrain=train[idtrain,]
wineval=train[-idtrain,]

winetrain$y = factor(winetrain$y)
wineval$y = factor(wineval$y)


###########################       Logistic Regression        ########################


# y1
LR_1_empty <- glm(formula = y1 ~ 1, family = "binomial", data = winetrain)

LR_1_full <- glm(formula = y1 ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
                 + totalsulfurdioxide + density + pH + sulphates + alcohol, family = "binomial", data = winetrain)
summary(LR_full)

LR_1 <- stepAIC(object = LR_1_full, scope = LR_1_empty, direction = "both", trace=FALSE)


summary(LR_1)

# y2
LR_2_empty <- glm(formula = y2 ~ 1, family = "binomial", data = winetrain)

LR_2_full <- glm(formula = y2 ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
                 + totalsulfurdioxide + density + pH + sulphates + alcohol, family = "binomial", data = winetrain)
summary(LR_2_full)

LR_2 <- stepAIC(object = LR_2_full, scope = LR_2_empty, direction = "both", trace=FALSE)


summary(LR_2)

# y3
LR_3_empty <- glm(formula = y3 ~ 1, family = "binomial", data = winetrain)

LR_3_full <- glm(formula = y3 ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
                 + totalsulfurdioxide + density + pH + sulphates + alcohol, family = "binomial", data = winetrain)
summary(LR_3_full)

LR_3 <- stepAIC(object = LR_3_full, scope = LR_3_empty, direction = "both", trace=FALSE)


summary(LR_3)

## Predictions


pred1 <- predict.glm(LR_1, wineval[,1:12], type = 'response')
pred2 <- predict.glm(LR_2, wineval[,1:12], type = 'response')
pred3 <- predict.glm(LR_3, wineval[,1:12], type = 'response')
pred <- as.data.frame(cbind(pred1, pred2, pred3))

pred$Results[pred1>pred2 & pred1>pred3] <- 1
pred$Results[pred2>pred1 & pred2>pred3] <- 2
pred$Results[pred3>pred2 & pred3>pred1] <- 3
head(pred)

mse <- sum(pred$Results == wineval$y)/nval
mse

##############################      Ordinal Forest      #############################
library(rpart)
library(rpart.plot)
library(ordinalForest)
library(ordinal)

set.seed(123)

ordforres <- ordfor(depvar="y", data=winetrain[,1:12], nsets=1000, ntreeperdiv=100, ntreefinal=5000, perffunction = "equal")
ordforres$perffunctionvalues
# Study variable importance values:
sort(ordforres$varimp, decreasing=TRUE)

# Predict values of the ordinal target variable in the test dataset:
preds <- predict(ordforres, newdata=wineval[,1:12])

# Compare predicted values with true values: 
table(data.frame(true_values=wineval$y, predictions=preds$ypred))
mse <- sum(preds$ypred == wineval$y)/nval
mse






