#library use
library(evtree)
library(rpart.plot)
require(reshape)
library(hydroGOF)

#preparation of data
data_evtree <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1month.csv",head=TRUE,sep=";",stringsAsFactors=F)
Hours <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%H:%M")
Dates <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%d-%m-%y")
data_evtree$Date <- Dates
data_evtree$Hour <- Hours
data_evtree <- transform(data_evtree, Date = as.POSIXct(Date,"%d-%m-%y",tz = "UTC"))
data_evtree <- transform(data_evtree, Hour = as.POSIXct(Hour,"%H:%M",tz = "UTC"))
data_evtree <- transform(data_evtree, Date = as.numeric(Date))
data_evtree <- transform(data_evtree, Hour = as.numeric(Hour))



#Split data_evtree to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_evtree))
data_evtree_training = data_evtree[1:corte,]
data_evtree_test = data_evtree[(corte+1):nrow(data_evtree),]

sub <- nrow(data_evtree)*0.75

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha =0.25,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                pcrossover = 0.2, psplit= 0.2, pprune = 0.4),
                            seed = NULL)

 
print(Sys.time())

Evtree_modelV2 <- evtree(Consumption ~ Date + Hour, 
                       data = data_evtree,subset = 1:sub, control = controlEv)
print(Sys.time())

plot(Evtree_modelV2)
predictValue_evtreeV2 <- predict(Evtree_modelV2,data_evtree_test)
nbRow <- nrow(data_evtree_test)

mseEVTREEV2 <- sum(data_evtree$Consumption - predictValue_evtreeV2[1:nbRow])^2
mreEVTREEV2 <- (1/nbRow)*(sum(abs(data_evtree_test$Consumption - predictValue_evtreeV2))/data_evtree_test$Consumption)*100
Taux_error_EvtreeV2 <- mean(mreEVTREEV2)
#Do the plot real and predict data
require(ggplot2)
x_evtreeV2<-c(1:nbRow)
y1_evtreeV2 <- data_evtree_test$Consumption
y2_evtreeV2 <- predictValue_evtreeV2

df<-data.frame(x_evtreeV2,y1_evtreeV2,y2_evtreeV2)

g_evtreeV2 <- ggplot(df, aes(x_evtreeV2))
g_evtreeV2  <- g_evtreeV2  + geom_line(aes(y=y1_evtreeV2), colour="red")
g_evtreeV2  <- g_evtreeV2  + geom_line(aes(y=y2_evtreeV2), colour="green")
g_evtreeV2 


