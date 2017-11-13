library("dtree")
library(evtree)
library(rpart.utils)
library(ggplot2)

data_dtree <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1month.csv",head=TRUE,sep=";",stringsAsFactors=F)
data_dtree <- transform(data_dtree, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
data_dtree <- transform(data_dtree, Date = as.numeric(Date))
data_dtree <- transform(data_dtree, Consumption = as.numeric(Consumption))

#Split data to data_training and data_test
split = 0.7
corte = floor(split*nrow(data_dtree))
data_training = data_dtree[1:corte,]
data_test = data_dtree[(corte+1):nrow(data_dtree),]


#Split data_training to data_traininVal and  data_validation
corte.val = floor(split*nrow(data_training))
data_trainingVal = data_training[1:corte.val,]
data_validation = data_training[(corte.val+1):nrow(data_training),]

print(Sys.time())

dt<- dtree(Consumption ~ Date, data_dtree, methods = c("lm", "rpart", "ctree", "evtree"),
      samp.method = "repeatedcv", tuneLength = 3, bump.rep = 50,
      subset = FALSE, perc.sub = 0.75, weights = NULL, verbose = TRUE)

# perc.sub :
# What fraction of data_dtree to put into train dataset.1-frac.sub is allocated to test dataset.

print(Sys.time())
plot(dt$evtree.out)
pr1 <- predict(dt$evtree.out)
pr2 <- predict(dt$lm)
pr3 <- predict(dt$ctree.out)
pr4 <- predict(dt$rpart.out)

require(ggplot2)
x<-c(1:1340)
y1 <- data_test$Consumption
y2 <- pr1[1:1340]
y3 <- pr2[1:1340]
y4 <- pr3[1:1340]
y5 <- pr4[1:1340]


df<-data.frame(x,y1,y2, y3, y4, y5)

g <- ggplot(df, aes(x))
g <- g + geom_line(aes(y=y1), color="red")
g <- g + geom_line(aes(y=y2), colour="green")
g <- g + geom_line(aes(y=y3), colour="blue")
g <- g + geom_line(aes(y=y4), colour="black")
g <- g + geom_line(aes(y=y5), colour="orange")
g <- g + theme(legend.position="top")
g
