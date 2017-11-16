library("dtree")
library(evtree)
library(rpart.utils)
library(ggplot2)

data_dtree <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1week.csv",head=TRUE,sep=";",stringsAsFactors=F)
data_dtree <- transform(data_dtree, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
data_dtree <- transform(data_dtree, Date = as.numeric(Date))
data_dtree <- transform(data_dtree, Consumption = as.numeric(Consumption))

#Split data to data_training and data_test
split = 0.75
corte_dtree = floor(split*nrow(data_dtree))
data_dtree_training = data_dtree[1:corte_dtree,]
data_dtree_test = data_dtree[(corte_dtree+1):nrow(data_dtree),]


#Split data_training to data_traininVal and  data_validation
corte.val_dtree = floor(split*nrow(data_dtree_training))
data_dtree_trainingVal = data_dtree_training[1:corte.val_dtree,]
data_dtree_validation = data_dtree_training[(corte.val_dtree+1):nrow(data_dtree_training),]

print(Sys.time())

dt<- dtree(Consumption ~ Date, data_dtree, methods = c("lm", "rpart", "ctree", "evtree"),
      samp.method = "boot", tuneLength = 3, bump.rep = 50,
      subset = TRUE, perc.sub = 0.75, weights = NULL, verbose = TRUE)

# perc.sub :
# What fraction of data_dtree to put into train dataset.1-frac.sub is allocated to test dataset.

print(Sys.time())
plot(dt$evtree.out)
pr1 <- predict(dt$evtree.out,data_dtree_test)
pr2 <- predict(dt$lm,data_dtree_test)
pr3 <- predict(dt$ctree.out,data_dtree_test)
pr4 <- predict(dt$rpart.out, data_dtree_test)

mreDTREE <- (1/nrow(data_dtree_test))*(sum(abs(data_dtree_test$Consumption - pr1))/data_dtree_test$Consumption)*100
Taux_error <- mean(mseDTREE)
require(ggplot2)
x_dtree<-c(1:nrow(data_dtree_test))
y1_dtree <- data_dtree_test$Consumption
y2_dtree <- pr1
y3_dtree <- pr2
y4_dtree <- pr3
y5_dtree <- pr4


df<-data.frame(x_dtree,y1_dtree,y2_dtree, y3_dtree, y4_dtree, y5_dtree)

g_dtree <- ggplot(df, aes(x_dtree))
g_dtree <- g_dtree + geom_line(aes(y=y1_dtree), color="red")
g_dtree <- g_dtree + geom_line(aes(y=y2_dtree), colour="green")
g_dtree <- g_dtree + geom_line(aes(y=y3_dtree), colour="blue")
g_dtree <- g_dtree + geom_line(aes(y=y4_dtree), colour="black")
g_dtree <- g_dtree + geom_line(aes(y=y5_dtree), colour="orange")
g_dtree <- g_dtree + theme(legend.position="top")
g_dtree
