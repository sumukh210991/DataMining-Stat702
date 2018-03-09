# Assignment 3
# Author : SUMUKH SAGAR MANJUNATH


# QUESTION 1
library(rpart)

col_names = read.csv("names.csv",header=F)
spam = read.csv("spamdata.txt",header=F)
names(spam) = sapply((1:nrow(col_names)),function(i) toString(col_names[i,1]))

head(spam)

spam$is_spam = factor(spam$is_spam, levels=0:1, labels=c("not_spam", "spam"))
is.factor(spam$is_spam)

set.seed(1)

my.control = rpart.control(xval=10,cp=0) 
cfit = rpart(is_spam ~ ., data=spam, method="class", control = my.control)

plot(cfit, margin=0.1)
text(cfit, use.n=T)

unpruned_spam = printcp(cfit)

unpruned_spam = as.data.frame(unpruned_spam)
oneSE_xerr = min(unpruned_spam$xerror) + unpruned_spam$xstd[unpruned_spam$xerror == min(unpruned_spam$xerror)]

oneSE_xerr

optim_cp = max(unpruned_spam$CP[unpruned_spam$xerror < oneSE_xerr])

optim_cp



# QUESTION 2

set.seed(1)

lmat = matrix(c(0,1,10,0), nrow=2, byrow=F)

my.control = rpart.control(xval=10,cp=0) 
cfit1 = rpart(is_spam ~ ., data=spam, method="class", control = my.control,parms=list(loss=lmat))

plot(cfit1, margin=0.1)
text(cfit1, use.n=T)

unpruned_spam = printcp(cfit1)

unpruned_spam = as.data.frame(unpruned_spam)
oneSE_xerr = min(unpruned_spam$xerror) + unpruned_spam$xstd[unpruned_spam$xerror == min(unpruned_spam$xerror)]

optim_cp = max(unpruned_spam$CP[unpruned_spam$xerror < oneSE_xerr])

optim_cp


# (2_a)
pruned_spam = prune(cfit1, cp = optim_cp)
predicted_spam = predict(pruned_spam, type="vector") - 1
observed_spam = as.numeric(spam$is_spam) - 1
missclass = dim(spam[(predicted_spam != observed_spam),])[1]
missclass
total_obs = dim(spam)[1]
total_obs
missclass_rate = (missclass / total_obs)* 100
missclass_rate
false_positive = dim(spam[predicted_spam == 1 & observed_spam == 0,])[1]
false_positive
false_negative = dim(spam[predicted_spam == 0 & observed_spam == 1,])[1]
false_negative
yes_spam = dim(spam[spam$is_spam == "spam",])[1]
yes_spam
no_spam = dim(spam[spam$is_spam == "not_spam",])[1]
no_spam
false_positive_rate = false_positive / no_spam
false_positive_rate
false_negative_rate = false_negative / yes_spam
false_negative_rate

# (2_b)
terminal_nodes = unpruned_spam$nsplit[unpruned_spam$CP==optim_cp] + 1
terminal_nodes

# (2_c)
pruned_at_8 = prune(cfit1, cp=unpruned_spam$CP[unpruned_spam$nsplit==7])
plot(pruned_at_8, margin=0.1)
text(pruned_at_8, use.n=T)
title("Pruned subtree with 8 terminal nodes")



# (2_d)
printcp(cfit1)



# (2_e)
splits <- cfit1$cptable[, 2]
terr <- cfit1$cptable[, 3]
xerr <- cfit1$cptable[, 4]

plot(splits, terr, ylim=c(0.2, 10), type="l")
lines(splits, xerr, lty=2)
title("Cross-validation Error Estimates and Training Error")
legend(45, 10, c("trainerror", "xerror"), lty=c(2,1))






