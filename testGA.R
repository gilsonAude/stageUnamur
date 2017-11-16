library(GA)
library(evtree)
library(parallel)
library(doParallel)

#preparation of data
data_GA_Alpha_Alpha <- read.csv(file="C:\\AUDE\\Unamur\\STAGE\\DATA\\1week.csv",head=TRUE,sep=";",stringsAsFactors=F)
#transform character into date
data_GA_Alpha_Alpha <- transform(data_GA_Alpha_Alpha, Date = as.POSIXct(Date,"%d-%m-%y %H:%M",tz = "UTC"))
#transform date into numeric
data_GA_Alpha_Alpha <- transform(data_GA_Alpha_Alpha, Date = as.numeric(Date))

#Split data_evtree to data_training and data_test
split = 0.75
corte = floor(split*nrow(data_GA_Alpha_Alpha))
data_GA_Alpha_Alpha_training = data_GA_Alpha_Alpha[1:corte,]
data_GA_Alpha_Alpha_test = data_GA_Alpha_Alpha[(corte+1):nrow(data_GA_Alpha_Alpha),]


#Split data_training to data_traininVal and  data_validation
corte.val_GA = floor(split*nrow(data_GA_Alpha_Alpha_training))
data_GA_Alpha_trainingVal = data_GA_Alpha_training[1:corte.val_GA,]
data_GA_Alpha_validation = data_GA_Alpha_training[(corte.val_GA+1):nrow(data_GA_Alpha_training),]
nbRow <- nrow(data_evtree_test)

print (Sys.time())

#process
controlEvFunc <- function (x) {
  (evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 9L,
                            niterations = 10000L, ntrees = 100L, alpha = x,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                  pcrossover = 0.2, psplit = 0.2, pprune = 0.4),
                                                seed = NULL))
  print (Sys.time())
  print("evtreeControl")
}
evtreeFuc <- function (x){
  
  print("evtree-debut")
  evtree(Consumption ~ Date, data = data_evtree_training, control = controlEvFunc(x))
  print("evtree-fin")
  print (Sys.time())
} 
predictFunc <- function (x){
  predict(evtreeFuc(x))
  print (Sys.time())
  print(x)
} 
Fitness <- function (x){
  -(1/nbRow)*(sum(abs(data_evtree_test$Consumption - predictFunc(x)[1:nrow(data_evtree_test)]))/data_evtree_test$Consumption)*100
  print (Sys.time())
  print("fitness")
  
  print(i)
} 

GA2 <- ga(type = "real-valued", fitness = function (x) Fitness(x[1]),
          min = c(0), max = c(1), popSize = 3, crossover = gareal_blxCrossover, maxiter = 2,
          run = 2)

print("------------"+Sys.time() + "------------")
alpha <- slot(GA2, "solution")
summary(GA2)
plot(GA2)
