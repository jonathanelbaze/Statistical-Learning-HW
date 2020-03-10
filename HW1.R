
setwd("~/Desktop/HEC/Session4/Advanced Statistical Learning/devoir_H2020/Statistical-Learning-HW")

train <- read.table('datrain.txt', header = TRUE, sep = ' ')
test <- read.table('dateststudent.txt', header = TRUE, sep = ' ')

summary(train)
table(unlist(train$y))

names(train)

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

set.seed(1234)
ntrain=1600
nval=nrow(train)-ntrain
idtrain=sample(1:nrow(train),ntrain,replace=FALSE)


winetrain=train[idtrain,]
wineval=train[-idtrain,]

winetrain$y = factor(winetrain$y)
wineval$y = factor(wineval$y)


###########################       Logistic Regression        ########################
library(MASS)

# y1
LR_1_empty <- glm(formula = y1 ~ 1, family = "binomial", data = winetrain)

LR_1_full <- glm(formula = y1 ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
                 + totalsulfurdioxide + density + pH + sulphates + alcohol, family = "binomial", data = winetrain)
summary(LR_1_full)

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

## Predictions by factor

pred1 <- predict.glm(LR_1, wineval[,1:12], type = 'response')
pred2 <- predict.glm(LR_2, wineval[,1:12], type = 'response')
pred3 <- predict.glm(LR_3, wineval[,1:12], type = 'response')
pred <- as.data.frame(cbind(pred1, pred2, pred3))

# Factor 1
pred$Result1[pred1 > 0.55] <- 1
pred$Result1[pred1 < 0.55] <- 0
rate1 <- sum(pred$Result1 == wineval$y1)/nval
rate1

# Factor 2
pred$Result2[pred2 > 0.4] <- 1
pred$Result2[pred2 < 0.4] <- 0
rate2 <- sum(pred$Result2 == wineval$y2)/nval
rate2

# Factor 3
pred$Result3[pred3 > 0.55] <- 1
pred$Result3[pred3 < 0.55] <- 0
rate3 <- sum(pred$Result3 == wineval$y3)/nval
rate3


## Predictions global

pred$Results[0.4*pred1>0.55*pred2 & pred1>pred3] <- 1
pred$Results[pred2*0.55>pred1*0.4 & pred2*0.55>pred3*0.4] <- 2
pred$Results[pred3*0.4>pred2*0.55 & pred3>pred1] <- 3

pred$Results[0.55*pred1>0.4*pred2 & pred1>pred3] <- 1
pred$Results[pred2*0.4>pred1*0.55 & pred2*0.4>pred3*0.55] <- 2
pred$Results[pred3*0.55>pred2*0.4 & pred3>pred1] <- 3

pred$Results[pred1>pred2 & pred1>pred3] <- 1
pred$Results[pred2>pred1 & pred2>pred3] <- 2
pred$Results[pred3>pred2 & pred3>pred1] <- 3

rate <- sum(pred$Results == wineval$y)/nval
rate

pred$Results[pred1>0.55] <- 1
pred$Results[pred3>0.55] <- 3
pred$Results[pred$Results == Na] <-2
head(pred)

rate <- sum(pred$Results == wineval$y)/nval
rate


###############################    Decision Tree     ################################
library(rpart)

tree = rpart(formula = y ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
             + totalsulfurdioxide + density + pH + sulphates + alcohol, data = winetrain, method='class')
plotcp(tree)
plot(tree, uniform=TRUE, 
     main="Wine Tree")
text(tree, use.n=TRUE, all=TRUE, cex=.8)

pred_tree <- predict(tree, wineval, type = 'class')
rateTree <- sum(pred_tree == wineval$y)/nval
rateTree


###############################    Random Forest     ################################

library(randomForest)

RF1 <- randomForest(winetrain[,1:11], winetrain$y1)
RF2 <- randomForest(winetrain[,1:11], winetrain$y2)
RF3 <- randomForest(winetrain[,1:11], winetrain$y3)

pred_RF1 <- predict(RF1, wineval, type = 'prob')
pred_RF2 <- predict(RF2, wineval, type = 'prob')
pred_RF3 <- predict(RF3, wineval, type = 'prob')
pred_RF <- as.data.frame(cbind(pred_RF1[,2], pred_RF2[,2], pred_RF3[,2]))

pred_RF$Results[pred_RF$V1>=pred_RF$V2 & pred_RF$V1>pred_RF$V3] <- 1
pred_RF$Results[pred_RF$V2>=pred_RF$V1 & pred_RF$V2>pred_RF$V3] <- 2
pred_RF$Results[pred_RF$V3>=pred_RF$V2 & pred_RF$V3>pred_RF$V1] <- 3
(pred_RF$Results)


# Factor 1
pred_RF$Result1[pred_RF1[,2] > 0.55] <- 1
pred_RF$Result1[pred_RF1[,2] < 0.55] <- 0
rateRF1 <- sum(pred_RF$Result1 == wineval$y1)/nval
rateRF1

# Factor 2
pred_RF$Result2[pred_RF2[,2] > 0.4] <- 1
pred_RF$Result2[pred_RF2[,2] < 0.4] <- 0
rateRF2 <- sum(pred_RF$Result2 == wineval$y2)/nval
rateRF2

# Factor 3
pred_RF$Result3[pred_RF3[,2] > 0.55] <- 1
pred_RF$Result3[pred_RF3[,2] < 0.55] <- 0
rateRF3 <- sum(pred_RF$Result3 == wineval$y3)/nval
rateRF3

# Consolidation
rateRF <- sum(pred_RF$Results == wineval$y)/nval
rateRF
library(stargazer)
table(data.frame(true_values=wineval$y, predictions=pred_RF$Results))



##########   Combination on different models : y1 and y3 LR // Y2 : RF    ###########

pred_comb <- as.data.frame(cbind(pred1, pred_RF2[,2], pred3))
pred_comb$Results[pred_comb$pred1>=pred_comb$V2 & pred_comb$pred1>pred_comb$pred3] <- 1
pred_comb$Results[pred_comb$V2>=pred_comb$pred1 & pred_comb$V2>pred_comb$pred3] <- 2
pred_comb$Results[pred_comb$pred3>=pred_comb$V2 & pred_comb$pred3>pred_comb$pred1] <- 3
(pred_comb$Results)

ratecomb <- sum(pred_comb$Results == wineval$y)/nval
ratecomb



##############################      Ordinal Forest      #############################
library(rpart)
library(rpart.plot)
library(ordinalForest)
library(ordinal)


ordforres <- ordfor(depvar="y", data=winetrain[,1:12], nsets=1000, ntreeperdiv=100, ntreefinal=5000, perffunction = "equal")
ordforres$perffunctionvalues
# Study variable importance values:
sort(ordforres$varimp, decreasing=TRUE)

# Predict values of the ordinal target variable in the test dataset:
preds <- predict(ordforres, newdata=wineval[,1:12])

# Compare predicted values with true values: 
table(data.frame(true_values=wineval$y, predictions=preds$ypred))
rate <- sum(preds$ypred == wineval$y)/nval
rate



#########################      Ordinal Cumulative Link Models      ########################

POM = clm(formula = y ~ fixedacidity + volatileacidity + citricacid + residualsugar + chlorides  + freesulfurdioxide 
          + totalsulfurdioxide + density + pH + sulphates + alcohol, data = winetrain)


