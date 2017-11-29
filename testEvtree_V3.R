#library use
library(evtree)
library(rpart.plot)
require(reshape)
library(hydroGOF)

#preparation of data

data_evtree <- read.csv(file="./dataset.csv",head=TRUE,sep=";",stringsAsFactors=F)
Hours <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%H:%M")
Dates <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%d-%m-%y")
data_evtree$Date <- Dates
data_evtree$Hour <- Hours

Ho <- format(as.POSIXct(strptime(Hours,"%H:%M",tz="")) ,format = "%H")
Min <- format(as.POSIXct(strptime(Hours,"%H:%M",tz="")) ,format = "%M")
Ho <- as.numeric(Ho)
Min <- as.numeric(Min)
Hos <- c()
Mins <-c()
for(h in Ho){
  ts <- h*3600
  Hos <- c(Hos,ts)
}
for(m in Min){
  ts <- m*60
  Mins <- c(Mins,ts)
}
data_evtree$HourInSec <- Hos + Mins

year <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%y")
month <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%m")
day <- format(as.POSIXct(strptime(data_evtree$Date,"%d-%m-%y",tz="")) ,format = "%d")


data_evtree$Year <- year
data_evtree$Month <- month
data_evtree$Day <- day
Day_month <- paste(data_evtree$Day,data_evtree$Month)
data_evtree$Day_month <- Day_month

data_evtree <- transform(data_evtree, Day_month = as.POSIXct(Day_month,"%d%m",tz = "UTC"))
data_evtree <- transform(data_evtree, Year = as.POSIXct(Year,"%y",tz = "UTC"))
data_evtree <- transform(data_evtree, Day_month = as.numeric(Day_month))
data_evtree <- transform(data_evtree, Year = as.numeric(Year))

data_evtree$Date <- NULL
data_evtree$Month <- NULL
data_evtree$Day <- NULL
data_evtree$Hour <- NULL


#Split data_evtree to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_evtree))
data_evtree_training = data_evtree[1:corte,]
data_evtree_test = data_evtree[(corte+1):nrow(data_evtree),]

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 1000L, ntrees = 100L, alpha =0.25,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                pcrossover = 0.8, psplit= 0.2, pprune = 0.4),
                            seed = NULL)

res <- c()
res <- c(format(Sys.time(), "%d-%b-%Y %H.%M"))
sub <- nrow(data_evtree)*0.75
Evtree_modelV3 <- evtree(Consumption ~., 
                       data = data_evtree, subset = 1:sub, control = controlEv)

res <- c(res,(format(Sys.time(), "%d-%b-%Y %H.%M")))



plot(Evtree_modelV3)
predictValue_evtreeV3 <- predict(Evtree_modelV3,data_evtree_test)
nbRow <- nrow(data_evtree_test)

mseEVTREEV3 <- sum(data_evtree_test$Consumption - predictValue_evtreeV3)^2
mreEVTREEV3 <- (1/nrow(data_evtree_test))*(sum(abs(data_evtree_test$Consumption - predictValue_evtreeV3))/data_evtree_test$Consumption)*100
Taux_error_EvtreeV3 <- mean(mreEVTREEV3)

res <- c(res,Taux_error_EvtreeV3)
write.table(res, sep =" ", eol="\r\n", "./result.txt")
#Do the plot real and predict data
require(ggplot2)
x_evtreeV3<-c(1:nbRow)
y1_evtreeV3 <- data_evtree_test$Consumption
y2_evtreeV3 <- predictValue_evtreeV3

dfV3<-data.frame(x_evtreeV3,y1_evtreeV3,y2_evtreeV3)

g_evtreeV3 <- ggplot(dfV3, aes(x_evtreeV3))
g_evtreeV3  <- g_evtreeV3  + geom_line(aes(y=y1_evtreeV3), colour="red")
g_evtreeV3  <- g_evtreeV3  + geom_line(aes(y=y2_evtreeV3), colour="green")
 
ggsave("evtree_plot", plot = g_evtreeV3, device = "pdf", path = "./evtree_plot.pdf",
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)




