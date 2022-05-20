library(kknn)
library(caret)
set.seed(5)
data <- read.table("data 3.1/credit_card_data.txt", 
                   stringsAsFactors = FALSE, header = FALSE)

# CV knn
check_accuracy = function(X){
  predicted <- rep(0,(nrow(data)))
  for (i in 1:nrow(data)){
    fit = cv.kknn(V11~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data, kcv = 10, 
                  k=X, distance = 2, kernel = "optimal",ykernel = NULL, scale = TRUE)
    fit_df <- as.data.frame(fit)
    predicted[i] <- as.integer(fit_df[i,2]+0.5)
  }
  print(predicted)
  accuracy = sum(predicted == data[,11]) / nrow(data)
  return(accuracy)
}
acc <- rep(0,20) 
for (X in 1:20){
  acc[X] = check_accuracy(X) 
}
plot(acc)
print(acc)

# split data
datasplit <- createDataPartition(data[,11], 1, p=0.7, list=FALSE)
training_dataset  <- data[datasplit, ] 
testing_validation <- data[-datasplit, ] 
testing_validation_split <- createDataPartition(testing_validation[,11], 
                                                1, p=0.5, list=FALSE)
validation_dataset <- testing_validation[testing_validation_split, ] 
testing_dataset <- testing_validation[-testing_validation_split,] 

# fit knn in training - validation set
check_accuracy2 = function(y){
  predicted2 <- rep(0,(nrow(validation_dataset)))
  knn_model = kknn(V11~., training_dataset, validation_dataset, 
                      k=y, scale=TRUE)
  predicted2 = round(fitted(knn_model))
  print(predicted2)
  accuracy2 = sum(predicted2 == validation_dataset[,11]) / nrow(validation_dataset)
  return(accuracy2)
}
acc2 <- rep(0,20) 
for (y in 1:20){
  acc2[y] = check_accuracy2(y) 
}
plot(acc2)
print(acc2)

# fit knn in training - testing set
check_accuracy3 = function(z){
  predicted3 <- rep(0,(nrow(testing_dataset)))
  knn_model3 = kknn(V11~., training_dataset, testing_dataset, 
                   k=z, scale=TRUE)
  predicted3 = round(fitted(knn_model3))
  print(predicted3)
  accuracy3 = sum(predicted3 == testing_dataset[,11]) / nrow(testing_dataset)
  return(accuracy3)
}
acc3 <- rep(0,20) 
for (z in 1:20){
  acc3[z] = check_accuracy3(z) 
}
plot(acc3)
print(acc3)
