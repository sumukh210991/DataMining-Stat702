########################################################
#         CART TO CLASSIFY CASES OF DIABETES           #  
########################################################

# Using CART with and without 1SE rule to classify positive/Negative 
# diabetes outcomes based rest of the predictors

library(rpart)
library(rpart.plot)

# Read in the data in CSV
diab.data = read.csv('pima-indians-diabetes.data', header = F)
colnames(diab.data) = c("No.Preg","Pl.Gl.Conc","BP","Tric.Thick"
              ,"Insulin","BMI","Dia.Pedig.Func","Age","is.positive")
diab.data = as.data.frame(diab.data)

# convert the output variable to factor type
diab.data$is.positive = factor(diab.data$is.positive, levels=0:1, 
                               labels=c("no", "yes"))
is.factor(diab.data$is.positive)

head(diab.data)

set.seed(1001)

# Specify the control for RPART with number of cross-validations
my.control <- rpart.control(xval=10, cp=0)

#------------------------------------------------------
#     Large tree is grown using the loss matrix to 
#     penalize the false negative misclassifications
#------------------------------------------------------
lmat = matrix(c(0,1,4,0), nrow=2, byrow=T)
cfit1 = rpart(formula = is.positive ~ ., data = diab.data, 
      method = "class", control = my.control, parms=list(loss=lmat))
#------------------------------------------------------
#     This large tree is grown using no loss matrix
#------------------------------------------------------
# Grow alarge tree
cfit1 = rpart(formula = is.positive ~ ., data = diab.data, 
              method = "class", control = my.control)


unpruned.tree = printcp(cfit1)
unpruned.tree = as.data.frame(unpruned.tree)

# Find the optimal Complexity Parameter of a subtree based on 
# 1SE and minimum crossvalidation error 
min.xerr = min(unpruned.tree$xerror)
one.SErr = min(unpruned.tree$xerror) + 
  unpruned.tree$xstd[unpruned.tree$xerror == min(unpruned.tree$xerror)]
# optim_cp = max(unpruned.tree$CP[unpruned.tree$xerror < one.SErr])
optim_cp = max(unpruned.tree$CP[unpruned.tree$xerror == min.xerr])

# Prune bases on the optimal CP found above
pruned.tree = prune(cfit1,cp = optim_cp)
prp(pruned.tree, extra=6,
    box.col=c("pink", "palegreen3")[pruned.tree$frame$yval])
text(pruned.tree)

# Plot to compare between cross-validation error and training error
numsplits <- cfit1$cptable[, 2]
trainerror <- cfit1$cptable[, 3]
xerror <- cfit1$cptable[, 4]
xstd <- cfit1$cptable[, 5]

plot(numsplits, trainerror, ylim=c(0.2, 1.0), type="l")
lines(numsplits, xerror, lty=2)
lines(numsplits, xerror-xstd, lty=2)
lines(numsplits, xerror+xstd, lty=2)
title("Cross-validation Error Estimates and Training Error")
legend(.02, .4, c("trainerror", "xerror"), lty=c(1,2))

# Calculate the misclassification error, False Positive 
# and False negative error
pred.diab = predict(pruned.tree, type="vector") - 1
obsv.diab = as.numeric(diab.data$is.positive) - 1
diab.misclass = dim(diab.data[(pred.diab != obsv.diab),])[1]
diab.misclass
total_obs = dim(diab.data)[1]
total_obs
missclass_rate = (diab.misclass / total_obs)* 100
missclass_rate
false_positive = dim(diab.data[pred.diab == 1 & obsv.diab == 0,])[1]
false_positive
false_negative = dim(diab.data[pred.diab == 0 & obsv.diab == 1,])[1]
false_negative
class.yes = dim(diab.data[diab.data$is.positive == "yes",])[1]
class.yes
class.no = dim(diab.data[diab.data$is.positive == "no",])[1]
class.no
false_positive_rate = false_positive / class.no
false_positive_rate
false_negative_rate = false_negative / class.yes
false_negative_rate


########################################################
#         BAGGING TO CLASSIFY CASES OF DIABETES        #  
########################################################

# Using Bootstrap AGGregation (BAGGING) with 500 iterations to classify  
# positive/Negative diabetes outcomes based on rest of the predictors

detach("package:ipred", unload=TRUE)
library(adabag)
set.seed(1001)

# perform bagging with 500 bootstrap samples
diab.bag = bagging(is.positive ~ ., data= diab.data ,mfinal=500)

# Derive variable importance
three_bagVar = sort(diab.bag$importance, decreasing = TRUE)[1:3]
three_bagVar

detach("package:adabag", unload=TRUE)
library(ipred)
set.seed(1001)
diab.bag1 = bagging(is.positive ~ ., data= diab.data ,nbag=500, coob=TRUE)

# Calculate the out of bag errors
oob_errBag = diab.bag1$err
oob_errBag


# Calculate misclassification error
smp_size<-floor(.75*nrow(diab.data))
set.seed(1001)
train_ind<-sample(seq_len(nrow(diab.data)),size=smp_size)
diabBag.train<-diab.data[train_ind,]
diabBag.test<-diab.data[-train_ind,]
dim(diabBag.test)
dim(diabBag.train)

diabBag.model <- bagging(is.positive ~ ., data= diabBag.train ,
                         nbag=500, coob=TRUE) # use adabag

diabBag.predc<-predict(diabBag.model,newdata=diabBag.test)

table(diabBag.predc,diabBag.test$is.positive)



########################################################
#   RANDOM FOREST TO CLASSIFY CASES OF DIABETES        #  
########################################################

library(randomForest)

set.seed(1001)

diabRF = randomForest(is.positive ~ ., data= diab.data, 
                      importance=TRUE, ntree=500)

var_importance = as.data.frame(round(importance(diabRF), 2))
var_importance

a = as.numeric(var_importance$MeanDecreaseAccuracy)
names(a) = row.names(var_importance)
three_imp_var_RF = sort(a, decreasing = T)[1:3]
three_imp_var_RF

diabRF.oob = diabRF$err.rate[500,1]
diabRF.oob

varImpPlot(diabRF)
