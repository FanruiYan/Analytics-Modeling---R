library(MASS)
set.seed(416)

df <- read.table("uscrime.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)
head(df)

#fit
fit <- lm(Crime~., data=df)
summary(fit)
# Stepwise regression model
AIC_fit <- stepAIC(fit, direction = "both", trace = FALSE)
summary(AIC_fit)


# Scale data
scale1 <- scale(df[,1], center = TRUE, scale = TRUE)
scale2 <- scale(df[,3:15], center = TRUE, scale = TRUE)
scaledf<- cbind(scale1, df[,2], scale2, df[,16]) 
colnames(scaledf) <- c("M","So","Ed","Po1","Po2","LF","M.F","Pop","NW",
                       "U1","U2","Wealth","Ineq","Prob","Time","Crime")
head(scaledf)

# LASSO
library(glmnet)
X <- as.matrix(scaledf[, 1:15])
Y <- as.matrix(scaledf[,16])
LASSO <- cv.glmnet(x=X,y=scaledf[,16],nfolds=5,alpha=1, family = 'gaussian', type.measure='mse') 
c<-coef(LASSO,s='lambda.min',exact=TRUE)
c
LASSO$glmnet.fit$dev.ratio[which(LASSO$glmnet.fit$lambda == LASSO$lambda.min)]


# Elastic Net
library(caret)
Al = seq(0, 1, by = .01)
set.seed(416)
model <- train(Crime~., data = scaledf, method = "glmnet",
               trControl = trainControl("cv", number = 10),tuneLength = 10)
# Best tuning parameter
model$bestTune
coef(model$finalModel, model$bestTune$lambda)
predictions <- predict(model, X)
R2 <- cor(Y, predictions)^2
R2


