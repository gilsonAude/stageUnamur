#library use
library(evtree)
library(rpart.plot)
require(reshape)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied.\n", call.=FALSE)
} else{
  if(args[1]>24 || args[1] < 1)
    stop("The argument must be the number of the problem you want to proceed (1 to 24).\n", call.=FALSE) 
}



#preparation of data
data_evtree <- read.csv(file="./dataset.csv",head=TRUE,sep=";",stringsAsFactors=F)
m.all<- data_evtree$Consumption

w = 168
h = 24
ncols = w+h
matrix = matrix(m.all[1:ncols], ncol=ncols)
i=1
while(!any(is.na(m.all[(i+h):(ncols+i-1+h)]))){
  matrix = rbind(matrix, m.all[(i+h):(ncols+i-1+h)])
  i= i+h
}

colnames(matrix) <- c(paste("X", 1:ncol(matrix),sep = ""))

numProb = as.integer(args[1])
print(numProb)


#Split data_evtree to data_training and data_test
split = 0.7
corte = floor(split*nrow(matrix))
data_evtree_training = matrix[1:corte,]
data_evtree_test = matrix[(corte+1):nrow(matrix),]

data_evtree_training <- as.data.frame(data_evtree_training)
data_evtree_test <- as.data.frame(data_evtree_test)

data_p_training <- data_evtree_training[,1:168] 
data_p_test <- data_evtree_test [,1:168]


##################################define the column to predict##################################


if(numProb == 1){
  data_p_training$X169 <- data_evtree_training$X169
  data_p_test$X169 <- data_evtree_test$X169
}
if(numProb == 2){
  data_p_training$X169 <- data_evtree_training$X170
  data_p_test$X169 <- data_evtree_test$X170
}
if(numProb == 3){
  data_p_training$X169 <- data_evtree_training$X171
  data_p_test$X169 <- data_evtree_test$X171
}
if(numProb == 4){
  data_p_training$X169 <- data_evtree_training$X172
  data_p_test$X169 <- data_evtree_test$X172
}
if(numProb == 5){
  data_p_training$X169 <- data_evtree_training$X173
  data_p_test$X169 <- data_evtree_test$X173
}
if(numProb == 6){
  data_p_training$X169 <- data_evtree_training$X174
  data_p_test$X169 <- data_evtree_test$X174
}
if(numProb == 7){
  data_p_training$X169 <- data_evtree_training$X175
  data_p_test$X169 <- data_evtree_test$X175
}
if(numProb == 8){
  data_p_training$X169 <- data_evtree_training$X176
  data_p_test$X169 <- data_evtree_test$X176
}
if(numProb == 9){
  data_p_training$X169 <- data_evtree_training$X177
  data_p_test$X169 <- data_evtree_test$X177
}
if(numProb == 10){
  data_p_training$X169 <- data_evtree_training$X178
  data_p_test$X169 <- data_evtree_test$X178
}
if(numProb == 11){
  data_p_training$X169 <- data_evtree_training$X179
  data_p_test$X169 <- data_evtree_test$X179
}
if(numProb == 12){
  data_p_training$X169 <- data_evtree_training$X180
  data_p_test$X169 <- data_evtree_test$X180
}
if(numProb == 13){
  data_p_training$X169 <- data_evtree_training$X181
  data_p_test$X169 <- data_evtree_test$X181
}
if(numProb == 14){
  data_p_training$X169 <- data_evtree_training$X182
  data_p_test$X169 <- data_evtree_test$X182
}
if(numProb == 15){
  data_p_training$X169 <- data_evtree_training$X183
  data_p_test$X169 <- data_evtree_test$X183
}
if(numProb == 16){
  data_p_training$X169 <- data_evtree_training$X184
  data_p_test$X169 <- data_evtree_test$X184
}
if(numProb == 17){
  data_p_training$X169 <- data_evtree_training$X185
  data_p_test$X169 <- data_evtree_test$X185
}
if(numProb == 18){
  data_p_training$X169 <- data_evtree_training$X186
  data_p_test$X169 <- data_evtree_test$X186
}
if(numProb == 19){
  data_p_training$X169 <- data_evtree_training$X187
  data_p_test$X169 <- data_evtree_test$X187
}
if(numProb == 20){
  data_p_training$X169 <- data_evtree_training$X188
  data_p_test$X169 <- data_evtree_test$X188
}
if(numProb == 21){
  data_p_training$X169 <- data_evtree_training$X189
  data_p_test$X169 <- data_evtree_test$X189
}
if(numProb == 22){
  data_p_training$X169 <- data_evtree_training$X190
  data_p_test$X169 <- data_evtree_test$X190
}
if(numProb == 23){
  data_p_training$X169 <- data_evtree_training$X191
  data_p_test$X169 <- data_evtree_test$X191
}
if(numProb == 24){
  data_p_training$X169 <- data_evtree_training$X192
  data_p_test$X169 <- data_evtree_test$X192
}
##################################define the column to predict##################################

controlEv <- evtree.control(minbucket = 8L, minsplit = 20L, maxdepth = 15L,
                            niterations = 1000L, ntrees = 100L, alpha =0.25,
                            operatorprob = list(pmutatemajor = 0.2, pmutateminor = 0.2,
                                                pcrossover = 0.8, psplit= 0.2, pprune = 0.4),
                            seed = NULL)

res <- c()
res <- c(format(Sys.time(), "%d-%b-%Y %H.%M"))


Evtree_model_p <- evtree(X169 ~.,data = data_p_training, control = controlEv)

res <- c(res,(format(Sys.time(), "%d-%b-%Y %H.%M")))



Evtree_prediction_p <- predict(Evtree_model_p,data_p_test)
nbRow <- nrow(data_p_test)



mreEVTREE_p <- (sum(abs(data_p_test$X169 - Evtree_prediction_p) / data_p_test$X169)) / nrow(data_p_test)*100

nameModel <- paste("./model_evtree_p",numProb,".rda", sep ="")
save(Evtree_model_p, Evtree_prediction_p,mreEVTREE_p, file=nameModel)

res <- c(res,mreEVTREE_p)
nameResult <- paste("./result_p",numProb,".txt",sep = "")
write.table(res, sep =" ", eol="\n", nameResult)
#Do the plot real and predict data
require(ggplot2)
x_evtreeV3<-c(1:nbRow)
y1_evtreeV3 <- data_p_test$X169
y2_evtreeV3 <- Evtree_prediction_p

dfV3<-data.frame(x_evtreeV3,y1_evtreeV3,y2_evtreeV3)

g_evtreeV3 <- ggplot(dfV3, aes(x_evtreeV3))
g_evtreeV3  <- g_evtreeV3  + geom_line(aes(y=y1_evtreeV3), colour="blue")
g_evtreeV3  <- g_evtreeV3  + geom_line(aes(y=y2_evtreeV3), colour="green",alpha=0.8)

plot(g_evtreeV3)




