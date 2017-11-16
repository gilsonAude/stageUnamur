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


#Split data_training to data_traininVal and  data_validation
corte.val_evtree = floor(split*nrow(data_evtree_training))
data_evtree_trainingVal = data_evtree_training[1:corte.val_evtree,]
data_evtree_validation = data_evtree_training[(corte.val_evtree+1):nrow(data_evtree_training),]

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha =0.25,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                pcrossover = 0.2, psplit= 0.2, pprune = 0.4),
                            seed = NULL)

 
print(Sys.time())

Evtree_model <- evtree(Consumption ~ Date + Hour, 
                       data = data_evtree,subset = TRUE, control = controlEv)
print(Sys.time())

plot(Evtree_model)
predictValue_evtree <- predict(Evtree_model,data_evtree_test)
nbRow <- nrow(data_evtree_test)

mseEVTREE <- sum(data_evtree$Consumption - predictValue_evtree[1:nbRow])^2
mreEVTREE <- (1/nrow(data_evtree_test))*(sum(abs(data_evtree_test$Consumption - predictValue_evtree[1:nbRow]))/data_evtree_test$Consumption)*100
Taux_error_Evtree <- mean(mreEVTREE)
#Do the plot real and predict data
require(ggplot2)
x_evtree<-c(1:nbRow)
y1_evtree <- data_evtree_test$Consumption
y2_evtree <- predictValue_evtree[1:nbRow]

df<-data.frame(x_evtree,y1_evtree,y2_evtree)

g_evtree <- ggplot(df, aes(x_evtree))
g_evtree  <- g_evtree  + geom_line(aes(y=y1_evtree), colour="red")
g_evtree  <- g_evtree  + geom_line(aes(y=y2_evtree), colour="green")
g_evtree 


