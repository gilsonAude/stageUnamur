#library use
library(evtree)
library(rpart.plot)
require(reshape)
library(hydroGOF)

#preparation of data
data_evtree <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1week.csv",head=TRUE,sep=";",stringsAsFactors=F)
#transform character into date
data_evtree <- transform(data_evtree, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
#transform date into numeric
data_evtree <- transform(data_evtree, Date = as.numeric(Date))

#Split data_evtree to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_evtree))
data_evtree_training = data_evtree[1:corte,]
data_evtree_test = data_evtree[(corte+1):nrow(data_evtree),]

sub <- nrow(data_evtree)*0.75

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha = 0.25,
                            operatorprob = list(pmutatemajor = 0.5, pmutateminor = 0.5,
                                                pcrossover = 0.5, psplit = 0.5, pprune = 0.5),
                            seed = NULL)

 
print(Sys.time())

Evtree_modelV1 <- evtree(Consumption ~ Date, data = data_evtree_training, control = controlEv)
 
print(Sys.time())
plot(Evtree_modelV1)
predictValue_evtreeV1 <- predict(Evtree_modelV1,data_evtree_test)
nbRow <- nrow(data_evtree_test)
mseEVTREE <- sum(data_evtree_test$Consumption - predictValue_evtreeV1)^2
mreEVTREE <- ((1/nbRow)*(sum(abs(data_evtree_test$Consumption - predictValue_evtreeV1))/data_evtree_test$Consumption))*100
Taux_error_Evtree <- mean(mreEVTREE)

#Do the plot real and predict data
require(ggplot2)
x_evtree<-c(1:nbRow)
y1_evtree <- data_evtree_test$Consumption
y2_evtree <- predictValue_evtreeV1

df<-data.frame(x_evtree,y1_evtree,y2_evtree)

g_evtree <- ggplot(df, aes(x_evtree))
g_evtree  <- g_evtree  + geom_line(aes(y=y1_evtree), colour="red")
g_evtree  <- g_evtree  + geom_line(aes(y=y2_evtree), colour="green")
g_evtree 


