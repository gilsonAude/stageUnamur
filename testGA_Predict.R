library(GA)
library(evtree)
library(parallel)
library(doParallel)
print (Sys.time())
#preparation of data
data_GA <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\6month.csv",head=TRUE,sep=";",stringsAsFactors=F)
#Get Date and Hour split
Hours <- format(as.POSIXct(strptime(data_GA$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%H:%M")
Dates <- format(as.POSIXct(strptime(data_GA$Date,"%d-%m-%y %H:%M",tz="")) ,format = "%d-%m-%y")
#Put Date and Hour in the data.frame
data_GA$Date <- Dates
data_GA$Hour <- Hours
#transform Date ans Hour to date format then to numeric format
data_GA <- transform(data_GA, Date = as.POSIXct(Date,"%d-%m-%y",tz = "UTC"))
data_GA <- transform(data_GA, Hour = as.POSIXct(Hour,"%H:%M",tz = "UTC"))
data_GA <- transform(data_GA, Date = as.numeric(Date))
data_GA <- transform(data_GA, Hour = as.numeric(Hour))


#Split data_GA to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_GA))
data_GA_training = data_GA[1:corte,]
data_GA_test = data_GA[(corte+1):nrow(data_GA),]


#Split data_training to data_traininVal and  data_validation
corte.val_GA = floor(split*nrow(data_GA_training))
data_GA_trainingVal = data_GA_training[1:corte.val_GA,]
data_GA_validation = data_GA_training[(corte.val_GA+1):nrow(data_GA_training),]
nbRow <- nrow(data_GA_test)


richards <- function(x, theta) theta[1] * (1 - exp(-theta[2] * x))^theta[3]
fitnessL1 <- function(theta, x, y) -sum(abs(y - richards(x, theta)))
fitnessL2 <- function(theta, x, y) 
#process
Fitness_function <- function (x,y,z){
  -sum((y - richards(x, z))^2)
} 

GA <- ga(type = "real-valued",fitness = Fitness_function, 
         x = data_GA_training$Hour
         y = data_GA_training$Consumption,
         min = min(data_GA_training[,2]), 
         max = max(data_GA_training[,2]),
         popSize =500, crossover = gareal_blxCrossover, maxiter = 5000,
        run = 2000)

alpha <- slot(GA, "solution")
summary(GA)
plot(GA)
print (Sys.time())
