#install.packages(GA)
#install.packages(evtree)
library(GA)
library(evtree)
####################################################################
#In this script i try to find the best alpha for the evtree process#
####################################################################

#preparation of data
data_GA_Alpha <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1week.csv",head=TRUE,sep=";",stringsAsFactors=F)
#transform character into date
data_GA_Alpha <- transform(data_GA_Alpha, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
#transform date into numeric
data_GA_Alpha <- transform(data_GA_Alpha, Date = as.numeric(Date))

#Split data_GA_Alpha to data_GA_Alpha_training and data_GA_Alpha_test
split = 0.75
corte = floor(split*nrow(data_GA_Alpha))
data_GA_Alpha_training = data_GA_Alpha[1:corte,]
data_GA_Alpha_test = data_GA_Alpha[(corte+1):nrow(data_GA_Alpha),]

nbRow <- nrow(data_GA_Alpha_training)

print(Sys.time())
res <- c()
res <- c(format(Sys.time(), "%d-%b-%Y %H.%M"))


#process
controlEvFunc <- function (x) {
  (evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                  niterations = 10000L, ntrees = 100L, alpha = x,
                  operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                      pcrossover = 0.2, psplit = 0.2, pprune = 0.4),
                  seed = NULL))
}
evtreeFuc <- function (x){
  print("start evtree")
  evtree(Consumption ~ Date, data = data_GA_Alpha_training, control = controlEvFunc(x))
} 
predictFunc <- function (x){
  predict(evtreeFuc(x))
} 
Fitness <- function (x){
  -(1/nbRow)*(sum(abs(data_GA_Alpha_test$Consumption - predictFunc(x)[1:nbRow]))/data_GA_Alpha_test$Consumption)*100
} 

GA2 <- ga(type = "real-valued", fitness = function (x) Fitness(x[1]),
          min = c(0), max = c(1), popSize = 3, crossover = gareal_blxCrossover, maxiter = 2,
          run = 2)

alpha <- slot(GA2, "solution")
summary(GA2)
plot(GA2)
res <- c(res,alpha)
res <- c(res,(format(Sys.time(), "%d-%b-%Y %H.%M")))
print(Sys.time())
write.table(res, sep =" ", eol="\r\n", "C:\\AUDE\\Unamur\\STAGE\\result.txt")
