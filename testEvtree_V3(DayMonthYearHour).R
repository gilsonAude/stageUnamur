#library use
library(evtree)
library(rpart.plot)
require(reshape)
library(hydroGOF)

#preparation of data
data_evtree <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1week.csv",head=TRUE,sep=";",stringsAsFactors=F)
Hours <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%H:%M")
Dates <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%d-%m-%y")
data_evtree$Date <- Dates
data_evtree$Hour <- Hours
year <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%y")
month <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%m")
day <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%d")
data_evtree$Year <- year
data_evtree$Month <- month
data_evtree$Day <- day
Day_month <- paste(data_evtree$Day,data_evtree$Month)
data_evtree$Day_month <- Day_month
data_evtree <- transform(data_evtree, Hour = as.POSIXct(Hour,"%H:%M",tz = "UTC"))
data_evtree <- transform(data_evtree, Day_month = as.POSIXct(Day_month,"%d%m",tz = "UTC"))
data_evtree <- transform(data_evtree, Year = as.POSIXct(Year,"%y",tz = "UTC"))
data_evtree <- transform(data_evtree, Hour = as.numeric(Hour))
data_evtree <- transform(data_evtree, Day_month = as.numeric(Day_month))
data_evtree <- transform(data_evtree, Year = as.numeric(Year))

data_evtree$Date <- NULL
data_evtree$Month <- NULL
data_evtree$Day <- NULL


#Split data_evtree to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_evtree))
data_evtree_training = data_evtree[1:corte,]
data_evtree_test = data_evtree[(corte+1):nrow(data_evtree),]


controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha =0.25,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                pcrossover = 0.2, psplit= 0.2, pprune = 0.4),
                            seed = NULL)

 
print(Sys.time())
sub <- nrow(data_evtree)*0.75
Evtree_model <- evtree(Consumption ~., 
                       data = data_evtree, subset = 1:sub, control = controlEv)
print(Sys.time())

plot(Evtree_model)
predictValue_evtree <- predict(Evtree_model,data_evtree_test)
nbRow <- nrow(data_evtree_test)

mseEVTREE <- sum(data_evtree$Consumption - predictValue_evtree)^2
mreEVTREE <- (1/nrow(data_evtree_test))*(sum(abs(data_evtree_test$Consumption - predictValue_evtree))/data_evtree_test$Consumption)*100
Taux_error_Evtree <- mean(mreEVTREE)
#Do the plot real and predict data
require(ggplot2)
x_evtree<-c(1:nbRow)
y1_evtree <- data_evtree_test$Consumption
y2_evtree <- predictValue_evtree

df<-data.frame(x_evtree,y1_evtree,y2_evtree)

g_evtree <- ggplot(df, aes(x_evtree))
g_evtree  <- g_evtree  + geom_line(aes(y=y1_evtree), colour="red")
g_evtree  <- g_evtree  + geom_line(aes(y=y2_evtree), colour="green")
g_evtree 


