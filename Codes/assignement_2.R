#Question . 7
url = 'http://www.davidzeleny.net/anadat-r/data-download/DoubsSpe.csv'
data = read.csv(url)
head(data)
desc_stat_id =c("min", "median", "mean", "sd", "max")
data_desc = NULL
for(i in 1:5)
{
  data_desc = rbind(data_desc,round(apply(data,2,desc_stat_id[i]),digits = 1))
}
as.data.frame(data_desc,row.names = desc_stat_id)
set.seed (99999)

#Question 8
data1 = scale(data[-1])
dis = dist(data1, method="euclidean")
hc = hclust(dis,method = "single")
plot(hc,hang=-1)


#Question 9
# dis = dist(data[-1], method="euclidean")
dis = dist(data1, method="euclidean")
hc = hclust(dis,method = "complete")
plot(hc,hang=-1)



#Question 10
# dis = dist(data[-1], method="euclidean")
dis = dist(data1, method="euclidean")
hc = hclust(dis,method = "ward.D2")
plot(hc,hang=-1)

dis = dist(data1, method="euclidean")
hc = hclust(dis,method = "average")
plot(hc,hang=-1)


#Question 11
library("cluster", lib.loc="C:/Program Files/R/R-3.2.3/library")

dis = dist(data1, method="euclidean")
hc = agnes(dis,diss=TRUE, method="single")
hc$ac
plot(hc)

dis = dist(data1, method="euclidean")
hc = agnes(dis,diss=TRUE, method="complete")
hc$ac
plot(hc)

dis = dist(data1, method="euclidean")
hc = agnes(dis,diss=TRUE, method="ward")
hc$ac
plot(hc)

dis = dist(data1, method="euclidean")
hc = agnes(dis,diss=TRUE, method="average")
hc$ac
plot(hc)


# Question 12
library("cluster")

url = 'http://www.davidzeleny.net/anadat-r/data-download/DoubsSpe.csv'
data = read.csv(url)

data1 = scale(data[-1])

hc = agnes(data1,method="ward")

memb1 <- cutree(hc, k = 2)
memb2 <- cutree(hc, k = 4)

table(memb1)
table(memb2)

aggregate(data1,list(memb1),median)

aggregate(data1,list(memb2),median)

aggregate(data1,list(memb1),median)[2,] == aggregate(data1,list(memb2),median)[4,]

