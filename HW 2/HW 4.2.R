library(caret)
data <- read.delim(file= "data 4.2/iris.txt", header = TRUE, sep = "", dec = ".")
# split train and test sets, 8:2
datasplit <- createDataPartition(data[,5], 1, p=0.8, list=FALSE)
training_dataset  <- data[datasplit, ] 
testing_dataset <- data[-datasplit, ] 

#rep(0, nc-1)
elbowplot <- function(data1, nc, seed){
  tss <- (nrow(data1)-1)*sum(var(data1)*var(data1))
  for (i in 1:nc){
    set.seed(seed)
    tss[i] <- sum(kmeans(data1, centers=i, nstart=20)$withinss)}
  plot(1:nc, tss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")
  print(tss)}

elbowplot(training_dataset[,1,4], nc = 20, seed=5)

#par(mfrow=c(2,2))
#elbowplot(training_dataset[,1,3], nc = 20, seed=5)
#elbowplot(training_dataset[,2,4], nc = 20, seed=5)
#elbowplot(training_dataset[,1,2], nc = 20, seed=5)
#elbowplot(training_dataset[,2,3], nc = 20, seed=5)

set.seed(5)
clustering_training <- kmeans(training_dataset[,2,4], centers = 3, nstart = 20)
clustering_training

set.seed(5)
clustering_testing <- kmeans(testing_dataset[,2,4], centers = 3, nstart = 20)
clustering_testing