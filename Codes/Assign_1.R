# SUMUKH SAGAR MANJUNATH
# STAT 702 : Data Mining
# ASSIGNMENT : 1

library(MASS)
head(Boston)
names(Boston)
summary(Boston)


# (1.a.)
size = c("rows"=nrow(Boston),"cols"=ncol(Boston))
size
sapply(Boston,class)
       


# (1.b) 
cor(Boston[2:length(Boston)],Boston$crim,method="pearson")
crim_cor = function()
{
  sig_cnt = 1
  res_corr = c()
  p_val = c()
  ind_var_name = c()
  assoc = c()
  ind_var = Boston[,names(Boston)!="crim"]
  for(i in 1:length(Boston))
  {
    if(cor.test(Boston[[i]],Boston$crim,method="pearson")$p.value < 0.05 && names(Boston[i])!="crim")
    {
      res_corr[sig_cnt] = cor(Boston[[i]],Boston$crim,method="pearson")
      ind_var_name[sig_cnt] = names(Boston[i])
      p_val[sig_cnt] = cor.test(Boston[[i]],Boston$crim,method="pearson")$p.value
      if(res_corr[[sig_cnt]]<0)
      {
        assoc = c(assoc,"neg")
      }
      else
      {
        assoc = c(assoc,"pos")
      }
      sig_cnt = sig_cnt + 1
    }  
  }
  names(res_corr) = ind_var_name
  res_corr = rbind(res_corr,p_val)
  res_corr = rbind(res_corr,assoc)
  return (res_corr)
  # print(res_corr)
}



#1.c.
par(mfrow=c(2,2))
plot(Boston$crim,Boston$zn,main="Correlation between crime-rate and proportion of land zoned",xlab="Crim", ylab="Zone") 
abline(lm(Boston$zn ~ Boston$crim))
plot(Boston$crim,Boston$indus,main="Correlation between crime-rate and proportion of proportion of non business retails",xlab="Crim", ylab="Industrial") 
abline(lm(Boston$indus ~ Boston$crim))
plot(Boston$crim,Boston$nox,main="Correlation between crime-rate and nitro-oxide conc",xlab="Crim", ylab="NOX") 
abline(lm(Boston$nox ~ Boston$crim))
plot(Boston$crim,Boston$rm,main="Correlation between crime-rate and Avg. No. of rooms of dwelling",xlab="Crim", ylab="rooms/dwelling") 
abline(lm(Boston$rm ~ Boston$crim))

par(mfrow=c(2,2))
plot(Boston$crim,Boston$age,main="Correlation between crime-rate and Age of house",xlab="Crim", ylab="Age") 
abline(lm(Boston$age ~ Boston$crim))
plot(Boston$crim,Boston$dis,main="Correlation between crime-rate and Distance to office",xlab="Crim", ylab="Distance to office") 
abline(lm(Boston$dis ~ Boston$crim))
plot(Boston$crim,Boston$rad,main="Correlation between crime-rate and Accessibility to radial highways",xlab="Crim", ylab="rad") 
abline(lm(Boston$rad ~ Boston$crim))
plot(Boston$crim,Boston$tax,main="Correlation between crime-rate and Property tax",xlab="Crim", ylab="P_Tax") 
abline(lm(Boston$tax ~ Boston$crim))

par(mfrow=c(2,2))
plot(Boston$crim,Boston$ptratio,main="Correlation between crime-rate and Pupil-Teacher ratio",xlab="Crim", ylab="P_T Ratio") 
abline(lm(Boston$ptratio ~ Boston$crim))
plot(Boston$crim,Boston$black,main="Correlation between crime-rate and Proportion of Black",xlab="Crim", ylab="Black") 
abline(lm(Boston$black ~ Boston$crim))
plot(Boston$crim,Boston$lstat,main="Correlation between crime-rate and Lower status",xlab="Crim", ylab="lstat") 
abline(lm(Boston$lstat ~ Boston$crim))
plot(Boston$crim,Boston$medv,main="Correlation between crime-rate and Property value",xlab="Crim", ylab="P_Value") 
abline(lm(Boston$medv ~ Boston$crim))
 



#1.d.
head(with(Boston, subset(Boston, Boston$chas==1 )))
tail(with(Boston, subset(Boston, Boston$chas==1 )))
nrow(with(Boston, subset(Boston, Boston$chas==1 )))



#1.e.
median(as.numeric(Boston$ptratio))
median(Boston$ptratio)


#1.f.
Boston[Boston$medv == min(as.numeric(Boston$medv)),]

plot(Boston$crim,ylab="crime rate | Residential Zone | industrial zone")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$crim[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
points(Boston$zn,lwd=1,cex=1,pch=22)
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$zn[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2,pch=22)
points(Boston$indus,lwd=1,cex=1,pch=24)
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$indus[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2,pch=24)
legend(x=630,y=95,c("crim","Res_Zone","Ind_zone"),pch=c(21,22,24),cex=1,pt.cex = 1,xjust=1,bty="n",x.intersp=0.3,y.intersp=0.3)
legend(x=440,y=89,c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
title("Comparision of crime rate, Residential zone and industrial zone of suburbs having least home value with others")


plot(Boston$nox,ylab="nitrogen oxide content")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$nox[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("nitrogen conc"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$nox ~ as.numeric(rownames(Boston)) ))
title("Comparision of Nitrogen Ox. Conc of suburbs having least home value with others")


plot(Boston$rm,ylab="Rooms per Dwelling")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$rm[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("Room/home"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$rm ~ as.numeric(rownames(Boston)) ))
title("Comparision of rooms per home of suburbs having least home value with others")


plot(Boston$age,ylab="Aged homes")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$age[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend(x=440,y=22,c("Aged home"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend(x=440,y=20,c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$age ~ as.numeric(rownames(Boston)) ))
title("Comparision of Aged Homes of suburbs having least home value with others")


plot(Boston$dis,ylab="Dist. to office")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$dis[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("Dist. to office"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$dis ~ as.numeric(rownames(Boston)) ))
title("Comparision of distance to office from home of suburbs having least home value with others")


plot(Boston$rad,ylab="Access to Rad. HWay")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$rad[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("Access to Rad. HWay"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$rad ~ as.numeric(rownames(Boston)) ))
title("Comparision of Access to Radial HighWay from home of suburbs having least home value with others")


plot(Boston$tax,ylab="Property Tax")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$tax[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("Property Tax"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$tax ~ as.numeric(rownames(Boston)) ))
title("Comparision of Property Tax of homes of suburbs having least home value with others")


plot(Boston$ptratio,ylab="Pupil Teacher ratio")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$ptratio[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend(x=420,y=14,c("Pupil Teacher ratio"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend(x=420,y=15,c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$ptratio ~ as.numeric(rownames(Boston)) ))
title("Comparision of Pupil Teacher ratio of suburbs having least home value with others")


plot(Boston$black,ylab="Proportion of Black")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$black[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend(x=-20,y=50,c("Proportion of Black"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend(x=-20,y=50,c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$black ~ as.numeric(rownames(Boston)) ))
title("Comparision of Proportion of Black in suburbs having least home value with others")


plot(Boston$lstat,ylab="% of low. status")
points(rownames(Boston[Boston$medv == min(as.numeric(Boston$medv)),]),Boston$lstat[Boston$medv == min(as.numeric(Boston$medv))],col="#FF0000",lwd=2,cex=2)
legend('topleft',c("% of low. status"),pch=21,bty="n",x.intersp=0.3,y.intersp=0.3)
legend('topleft',c("Least val home"),pch=19,col="red",cex=1,bty='n',x.intersp=0.3,pt.cex = 1.5)
abline(lm(Boston$lstat ~ as.numeric(rownames(Boston)) ))
title("Comparision of percentage of lower status in suburbs having least home value with others")

#1.g.
nrow(Boston[Boston$rm > 7,])
nrow(Boston[Boston$rm > 8,])




#2.a.
x= matrix(c(2,10,3,3,7,2),nrow=3)
x
y=matrix(c(1,3,5,15,8,16,4,3,7,2,2,4,33,7),ncol=2)
y
mean_x = matrix(c(mean(x[,1]),mean(x[,2])),nrow=2)
mean_x
mean_y = matrix(c(mean(y[,1]),mean(y[,2])),nrow=2)
mean_y
x1=matrix(c(x[,1]-mean(x[,1]),c(x[,2]-mean(x[,2]))),ncol=2)
x1
y1=matrix(c(y[,1]-mean(y[,1]),c(y[,2]-mean(y[,2]))),ncol=2)
y1
c1 = (1/nrow(x1))*(t(x1)%*%x1)
c1
c2 = (1/nrow(y1))*(t(y1)%*%y1)
c2
s = (nrow(x1)/(nrow(x1)+nrow(y1)))*c1 + (nrow(y1)/(nrow(x1)+nrow(y1)))*c2
s
solve(s)
Mahalanobis_dist = sqrt(t(mean_x - mean_y) %*% solve(s) %*% (mean_x - mean_y))
Mahalanobis_dist



# 2.b.

Mahalanobis_dist = function(x,y)
{
  if(ncol(x)!=ncol(y))
  {
    print("ERROR! Number of columns in two matrices must be same!")
    return()
  }
  mean_x = matrix(c(mean(x[,1]),mean(x[,2])),nrow=2)
  mean_y = matrix(c(mean(y[,1]),mean(y[,2])),nrow=2)
  x1=matrix(c(x[,1]-mean(x[,1]),c(x[,2]-mean(x[,2]))),ncol=2)
  y1=matrix(c(y[,1]-mean(y[,1]),c(y[,2]-mean(y[,2]))),ncol=2)
  c1 = (1/nrow(x1))*(t(x1)%*%x1)
  c2 = (1/nrow(y1))*(t(y1)%*%y1)
  s = (nrow(x1)/(nrow(x1)+nrow(y1)))*c1 + (nrow(y1)/(nrow(x1)+nrow(y1)))*c2
  dist = sqrt(t(mean_x - mean_y) %*% solve(s) %*% (mean_x - mean_y))
  return(dist)
}