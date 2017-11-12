
library(evtree)
library(rpart.plot)
require(reshape)
library(hydroGOF)

data <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\6month.csv",head=TRUE,sep=";",stringsAsFactors=F)
#transform character into date
data <- transform(data, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
#transform date into numeric
data <- transform(data, Date = as.numeric(Date))

#Split data to data_training and data_test
split = 0.7
corte = floor(split*nrow(data))
data_training = data[1:corte,]
data_test = data[(corte+1):nrow(data),]


#Split data_training to data_traininVal and  data_validation
corte.val = floor(split*nrow(data_training))
data_trainingVal = data_training[1:corte.val,]
data_validation = data_training[(corte.val+1):nrow(data_training),]

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha = 1,
                            operatorprob = list(pmutatemajor = 0.5, pmutateminor = 0.5,
                                                pcrossover = 0.5, psplit = 0.5, pprune = 0.5),
                            seed = NULL)

 
print(Sys.time())

model <- evtree(Consumption ~ Date, data = data_training, control = controlEv)
 
print(Sys.time())
plot(model)
predictValue <- predict(model)

mc <- hydroGOF::mse(as.vector(predictValue[1:7820]), as.numeric(data_test$Consumption))

#Do the plot real and predict data
require(ggplot2)
x<-c(1:7820)
y1 <- data_test$Consumption
y2 <- predictValue[1:7820]

df<-data.frame(x,y1,y2)

g <- ggplot(df, aes(x))
g <- g + geom_line(aes(y=y1), colour="red")
g <- g + geom_line(aes(y=y2), colour="green")
g


