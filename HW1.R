train <- read.table('/Users/jonathanelbaze/Desktop/HEC/Session4/Advanced Statistical Learning/devoir_H2020/datrain.txt', header = TRUE, sep = ' ')


summary(train)
# to apply it to a data frame "mat" and get a data frame as the result
# data.frame(lapply(mat,comblev,nmin=2))


##################################
# Splitting the data into a training (ntrain=1600) and a 
#  test (ntest=400) set

set.seed(123)
ntrain=1600
nval=nrow(train)-ntrain
indtrain=sample(1:nrow(train),ntrain,replace=FALSE)


winetrain=train[indtrain,]
wineval=train[-indtrain,]





##################################
# Variable selection examples, with all 161 covariates 
#  (including the dummies for the categorical covariates)


# function to apply glmnet and get the predictions and 
# coefficients for the tuning parameter with minimum CV error,
# and with the 1 SE rule. It also computes the MAE and MSE if
# ytest is available

library(glmnet)

wrapglmnet=function(xtrain,ytrain,xtest,ytest=NULL,alpha)
{
  library(glmnet)
  par(mfrow=c(2,2))
  plot(glmnet(x=xtrain,y=ytrain,alpha=alpha),xvar = "lambda", label = TRUE)
  cv=cv.glmnet(x=xtrain,y=ytrain, alpha=alpha)
  plot(cv)
  pred=predict(cv,new=xtest,s="lambda.min")
  pred1se=predict(cv,new=xtest,s="lambda.1se")
  err=NA
  if(!is.null(ytest))
  {
    plot(ytest,pred)
    plot(ytest,pred1se)
    err=data.frame(mean(abs(pred-ytest)),mean((pred-ytest)^2),
                   mean(abs(pred1se-ytest)),mean((pred1se-ytest)^2))
    names(err)=c("MAE", "MSE", "MAE_1SE","MSE_1SE")
  }
  
  co=predict(cv,s="lambda.min",type="coefficients")
  co=as.matrix(co)
  co=co[co[,1] != 0,,drop=FALSE]
  co1se=predict(cv,s="lambda.1se",type="coefficients")
  co1se=as.matrix(co1se)
  co1se=co1se[co1se[,1] != 0,,drop=FALSE]
  
  out=list(err,co,co1se,pred,pred1se)
  names(out)=c("error","coef","coef1se","pred","pred1se")
  out
}

# basic lasso
las=wrapglmnet(winetrain[,c(1,2,3)],winetrain$y,wineval[,c(1,2,3)],wineval$y,1)
dim(las$coef1se)
dim(las$coef)
las$coef1se
las$coef
las$err


# basic ridge
rid=wrapglmnet(xdumtrain,amesdumtrain$Sale_Price,xdumtest,amesdumtest$Sale_Price,0)
dim(rid$coef1se)
dim(rid$coef)
rid$err






# Ordinary OLS regression
lmfit=lm(y~.,data=winetrain)
predlmfit=predict(lmfit,newdata=wineval)
errlmfit=data.frame(mean(abs(predlmfit-wineval$y)),
                    mean((predlmfit-wineval$y)^2))
names(errlmfit)=c("MAE","MSE")
errlmfit







# Fit an OLS with the lasso variables only
namlas=rownames(las$coef)[-1]
laslm=lm(Sale_Price~.,data=amesdumtrain[,c(namlas,"Sale_Price")])
predlaslm=predict(laslm,newdata=amesdumtest)
errlaslm=data.frame(mean(abs(predlaslm-amesdumtest$Sale_Price)),
                    mean((predlaslm-amesdumtest$Sale_Price)^2))
names(errlaslm)=c("MAE","MSE")
errlaslm

# Apply the lasso again to the lasso variables only
laslas=wrapglmnet(xdumtrain[,namlas],amesdumtrain$Sale_Price,
                  xdumtest[,namlas],amesdumtest$Sale_Price,1)
# check if all the variables are kept
rownames(laslas$coef)==rownames(las$coef)
# proportion of parameters greater in magnitude
# in the lasso-lasso solution compared to the lasso
mean(abs(laslas$coef)>abs(las$coef))
laslas$err


# relaxed lasso
library(relaxnet)
relaxlassocv=cv.relaxnet(xdumtrain, amesdumtrain$Sale_Price, family = "gaussian",nlambda=100,alpha=1)
coefrla=as.matrix(predict(relaxlassocv,new=xdumtest,type="coefficient"))
# extract and count the number of non-zero coeficients
length(coefrla[coefrla[,1] != 0 ,])
predrla=predict(relaxlassocv,new=xdumtest)
errrla=data.frame(mean(abs(predrla-amesdumtest$Sale_Price)),
                  mean((predrla-amesdumtest$Sale_Price)^2))
names(errrla)=c("MAE","MSE")
errrla


# relaxed elastic net
relaxnetcv=cv.alpha.relaxnet(xdumtrain, amesdumtrain$Sale_Price, family = "gaussian",nlambda=100)
coefrnet=as.matrix(predict(relaxnetcv,new=xdumtest,type="coefficient"))
# extract and count the number of non-zero coeficients
length(coefrnet[coefrnet[,1] != 0 ,])
predrnet=predict(relaxnetcv,new=xdumtest)
errrnet=data.frame(mean(abs(predrnet-amesdumtest$Sale_Price)),
                   mean((predrnet-amesdumtest$Sale_Price)^2))
names(errrnet)=c("MAE","MSE")
errrnet


# Putting all the results together and sorting them according to the MAE and MSE
allres=rbind(las$err[,1:2],rid$err[,1:2],errlmfit,errlaslm,laslas$err[,1:2],errrla,errrnet)
row.names(allres)=c("lasso","ridge","OLS","lasso-OLS","lasso-lasso","relaxed lasso","relaxed elastic net")
allres[order(allres[,1]),]
allres[order(allres[,2]),]


# For some models, a few pedicted values are negative which is impossible for this response.
#	We might try to model log(sale_price) instead.

summary(las$pred)
sort(las$pred)[1:10]
length(las$pred)



########################################################################

# Treating the dummies of a categorical variable as a group

# function to create the variable "group", as consecutive integers,
# (as recommended) for grpreg.

creategroup=function(dat)
{
  # dat must be created by the function dummy.data.frame
  # the target should not be there
  
  p=NCOL(dat)
  gr=rep(0,p)
  att=attributes(dat)$dummies
  pd=length(att)
  curvar=1
  minind=min(att[[1]])
  maxind=max(att[[1]])
  crg=0
  for(i in 1:p)
  {
    if(i<=minind){crg=crg+1}
    if(i==maxind){
      curvar=curvar+1
      if(curvar<=pd)
      {
        minind=min(att[[curvar]])
        maxind=max(att[[curvar]])
      }
      else
      {
        minind=Inf
        maxind=Inf
      }
    }
    gr[i]=crg
  }
  gr
}

# the vector group will be the argument to grpreg
tames=ames
tames$Sale_Price=NULL
#library(dummies)
tamesdum=dummy.data.frame(tames)
group=creategroup(tamesdum)
group=group[-c(unlist(lapply(attributes(tamesdum)$dummies,max)))]

# check that group has been constructed properly
cbind(group,colnames(xdumtrain))

library(grpreg)

grlassofit=grpreg(xdumtrain, amesdumtrain$Sale_Price, group, penalty="grLasso")
plot(grlassofit)

# Group lasso
grlassofitcv=cv.grpreg(xdumtrain, amesdumtrain$Sale_Price, group,seed=474659,penalty="grLasso")
coefgrlasso=predict(grlassofitcv,type="coefficients")
predgrlasso=predict(grlassofitcv,X=xdumtest)
errgrlasso=data.frame(mean(abs(predgrlasso-amesdumtest$Sale_Price)),
                      mean((predgrlasso-amesdumtest$Sale_Price)^2))
names(errgrlasso)=c("MAE","MSE")
row.names(errgrlasso)=c("group lasso")
errgrlasso
allres=rbind(allres,errgrlasso)



# Exponential lasso
gelcv=cv.grpreg(xdumtrain, amesdumtrain$Sale_Price, group,seed=474659,penalty="gel")
coefgel=predict(gelcv,type="coefficients")
predgel=predict(gelcv,X=xdumtest)
errgel=data.frame(mean(abs(predgel-amesdumtest$Sale_Price)),
                  mean((predgel-amesdumtest$Sale_Price)^2))
names(errgel)=c("MAE","MSE")
row.names(errgel)=c("exponential lasso")
errgel
allres=rbind(allres,errgel)


# Best methods so far
allres[order(allres[,1]),]
allres[order(allres[,2]),]



########################################################################

########  Tree-based methods

library(rpart)
library(rpart.plot)

set.seed(37569)
rptree=rpart(Sale_Price~.,data=amestrain,method="anova",control = rpart.control(xval = 10, minsplit=10, minbucket = 3, cp = 0))
#rpart.plot(rptree)
rptree$cp

rptreepruned=prune(rptree,cp=rptree$cp[which.min(rptree$cp[,"xerror"]),"CP"])
#rpart.plot(rptreepruned)
predrpart=predict(rptreepruned,newdata=amestest)
errrpart=data.frame(mean(abs(predrpart-amesdumtest$Sale_Price)),
                    mean((predrpart-amesdumtest$Sale_Price)^2))
names(errrpart)=c("MAE","MSE")
row.names(errrpart)=c("single tree (rpart)")
errrpart
allres=rbind(allres,errrpart)


rptreepruned1se=prune(rptree,cp=5.459169e-03)
rpart.plot(rptreepruned1se)
predrpart1se=predict(rptreepruned1se,newdata=amestest)
errrpart1se=data.frame(mean(abs(predrpart1se-amesdumtest$Sale_Price)),
                       mean((predrpart1se-amesdumtest$Sale_Price)^2))
names(errrpart1se)=c("MAE","MSE")
errrpart1se


####### Random forest


set.seed(33967)
library(randomForest)
rf=randomForest(Sale_Price~.,data=amestrain,ntree=500,mtry=40)
predrf=predict(rf,newdata=amestest)
errrf=data.frame(mean(abs(predrf-amesdumtest$Sale_Price)),
                 mean((predrf-amesdumtest$Sale_Price)^2))
names(errrf)=c("MAE","MSE")
row.names(errrf)=c("random forest")
errrf
allres=rbind(allres,errrf)


# Best methods so far
allres[order(allres[,1]),]
allres[order(allres[,2]),]


virf=importance(rf)
varImpPlot(rf)

# sort the absolute value correlations between 
#   the target and the other variables
#   using the dummy variables for the categorical
#   variables
library(corrr)
co=correlate(amesdumtrain)
coy=as.data.frame(focus(co,Sale_Price))
coy[,2]=abs(coy[,2])
coys=coy[order(-coy[,2]),,drop=FALSE]
coys[1:30,]



########################################################################

########  Boosting


## with gbm

set.seed(33967)
library(gbm)

gbmgc=gbm(Sale_Price~.,data=amestrain,distribution="gaussian",
          n.trees=100,interaction.depth = 5,shrinkage =0.1)
predgbm=predict(gbmgc,newdata=amestest,n.trees=100)
errgbm=data.frame(mean(abs(predgbm-amesdumtest$Sale_Price)),
                  mean((predgbm-amesdumtest$Sale_Price)^2))
names(errgbm)=c("MAE","MSE")
row.names(errgbm)=c("Tree boosting with gbm")
errgbm
allres=rbind(allres,errgbm)


## with mboost

set.seed(4756)
library(mboost)

# fit the model with all variables and 1000 iterations
glmboostgc=glmboost(Sale_Price~.,data=amesdumtrain,family=Gaussian(),
                    control = boost_control(mstop = 2000))
# to select the number of iterations with bootstrap
glmboostgccv=cvrisk(glmboostgc)
plot(glmboostgccv)
# value of the best number of iterations
bestm=mstop(glmboostgccv)
bestm
# coefficients of the model when we stop at this number of iterations
coef(glmboostgc[bestm])
# number of parameters (including intercept)
length(coef(glmboostgc[bestm]))

# computing predictions
predglmboost=predict(glmboostgc[bestm],new=amesdumtest)
errglmboost=data.frame(mean(abs(predglmboost-amesdumtest$Sale_Price)),
                       mean((predglmboost-amesdumtest$Sale_Price)^2))
names(errglmboost)=c("MAE","MSE")
row.names(errglmboost)=c("LS Boosting with glmboost")
errglmboost
allres=rbind(allres,errglmboost)

# Best methods so far
allres[order(allres[,1]),]
allres[order(allres[,2]),]


########################################################################
########  Prediction intervals

covlen=function(matpi)
{
  # Function that computes the coverage rate and the mean length
  #	of prediction intervals
  # input: matrix with 3 columns
  # 	col1 = true y
  #	col2 = lower bound of the PI
  #	col3 = upper bound of the PI
  
  # output: List with 2 elements:
  #	1 = coverage rate; 2 = mean length
  
  list(mean(apply(matpi,1,function(a){(a[1]>=a[2])*(a[1]<=a[3])})),
       mean(matpi[,3]-matpi[,2]))
}

# Ordinary OLS regression 

lmfit=lm(Sale_Price~.,data=amesdumtrain)
pilmfit=predict(lmfit,newdata=amesdumtest,
                type="response",interval="prediction")
pilmfit[1:5,]

cllmfit=covlen(cbind(amesdumtest$Sale_Price,pilmfit[,2:3]))
cllmfit


# Split Conformal PI with the LASSO

library(conformalInference)
library(glmnet)

cvglmnet=cv.glmnet(x=xdumtrain,y=amesdumtrain$Sale_Price,alpha=1)
lambda=cvglmnet$lambda.min
funs = lasso.funs(lambda=lambda)
lassoconf = conformal.pred.split(x=xdumtrain, y=amesdumtrain$Sale_Price, x0=xdumtest, alpha=0.05,
                                 train.fun=funs$train, predict.fun=funs$predict)
pilassoconf=cbind(lassoconf$lo,lassoconf$up)
cllassoconf=covlen(cbind(amesdumtest$Sale_Price,pilassoconf))
cllassoconf


#  PI with RF with With quantRegforest

library(quantregForest)

temptrain=amestrain
temptrain$Sale_Price=NULL
temptest=amestest
temptest$Sale_Price=NULL

qrf=quantregForest(x=temptrain,y=amestrain$Sale_Price,ntree=500,mtry=40)

# Get the Pis with the 0.025 and 0.975 quantiles
alp=.025
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf


# The PIs are conservative, try other values for the quantiles
alp=.05
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf

alp=.06
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf

alp=.07
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf

alp=.08
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf

alp=.09
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf

alp=.1
piqrf=predict(qrf,temptest,what=c(alp,1-alp))
clqrf=covlen(cbind(amesdumtest$Sale_Price,piqrf))
clqrf






