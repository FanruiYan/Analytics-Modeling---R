library(caret)
library(MASS)
library(dplyr)
library('e1071')
library(ROCR)

set.seed(416)

# read data
df <- read.table("germancredit.txt", sep = "", stringsAsFactors = FALSE, header = FALSE)
head(df)
summary(df)

# draw qq plot 
par(mfrow=c(1,3))
qqnorm(df$V2, pch = 1, frame = FALSE)
qqnorm(df$V5, pch = 1, frame = FALSE)
qqnorm(df$V13, pch = 1, frame = FALSE)

# draw transformed data
par(mfrow=c(1,3))
qqnorm(log(df$V2), pch = 1, frame = FALSE)
qqnorm(log(df$V5), pch = 1, frame = FALSE)
qqnorm(log(df$V13), pch = 1, frame = FALSE)

# log transform of V2 V5 V13
head(df)
df$V2=log(df$V2)
df$V5=log(df$V5)
df$V13=log(df$V13)
df$V21=df$V21-1
head(df)

# split data into train and test
datasplit <- createDataPartition(df[,21], 1, p=0.7, list=FALSE)
training_dataset <- df[datasplit, ]
testing_dataset <- df[-datasplit, ]

# fit log regression
logfit <- glm(V21~.,data = training_dataset, family=binomial)
summary(logfit)

# Stepwise regression model
AIC_fit <- stepAIC(logfit, direction = "both", trace = FALSE)
summary(AIC_fit)

#check accuracy
yhat <- predict(AIC_fit ,newdata=training_dataset, type = "response")
training_dataset$yhat = yhat
head(training_dataset)

# Set threshold to 0.5
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.5) + 0)
cm_train5 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train5 = cm_train5$table[1,2]+cm_train5$table[2,1]*5
cost_train5
cm_train5

# Set threshold to 0.1
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.1) + 0)
cm_train1 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train1 = cm_train1$table[1,2]+cm_train1$table[2,1]*5
cost_train1
cm_train1

# Set threshold to 0.2
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.2) + 0)
cm_train2 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train2 = cm_train2$table[1,2]+cm_train2$table[2,1]*5
cost_train2
cm_train2

# Set threshold to 0.3
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.3) + 0)
cm_train3 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train3 = cm_train3$table[1,2]+cm_train3$table[2,1]*5
cost_train3
cm_train3

# Set threshold to 0.4
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.4) + 0)
cm_train4 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train4 = cm_train4$table[1,2]+cm_train4$table[2,1]*5
cost_train4
cm_train4


# Set threshold to 0.6
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.6) + 0)
cm_train6 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train6 = cm_train6$table[1,2]+cm_train6$table[2,1]*5
cost_train6
cm_train6

# Set threshold to 0.7
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.7) + 0)
cm_train7 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train7 = cm_train7$table[1,2]+cm_train7$table[2,1]*5
cost_train7
cm_train7

# Set threshold to 0.8
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.8) + 0)
cm_train8 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train8 = cm_train8$table[1,2]+cm_train8$table[2,1]*5
cost_train8
cm_train8

# Set threshold to 0.9
training_dataset <- training_dataset  %>% mutate(predict = 1*(yhat > 0.9) + 0)
cm_train9 <- confusionMatrix(data = factor(training_dataset$predict), 
                             reference = factor(training_dataset$V21))
# False Positive cm$table[2,1]  &  False Negative cm$table[1,2]
cost_train9 = cm_train9$table[1,2]+cm_train9$table[2,1]*5
cost_train9
cm_train9



# predicting & accuaracy in testing set
yhat_test <- predict(AIC_fit ,newdata=testing_dataset, type = "response")
testing_dataset <- testing_dataset  %>% mutate(predict = 1*(yhat_test  > 0.8) + 0)
cm_test <- confusionMatrix(data = factor(testing_dataset$predict), 
                           reference = factor(testing_dataset$V21))
cost_test = cm_test$table[1,2]+cm_test$table[2,1]*5
cost_test
cm_test
