library(tree)
library(MASS)
library("randomForest")
set.seed(416)

df <- read.table("uscrime.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)
head(df)

newdf <- data.frame (M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0,
                     Po2 = 15.5, LF = 0.640, M.F = 94.0,
                     Pop = 150, NW = 1.1, U1 = 0.120,
                     U2 = 3.6, Wealth = 3200,
                     Ineq = 20.1, Prob = 0.04, Time = 39.0)

# Regression Tree model
reg.tree <- tree(Crime ~., data=df)
plot(reg.tree)
text(reg.tree, cex=.75)
summary(reg.tree)

# prune tree, see if model imporved
prune.tree=prune.tree(reg.tree ,best=5)
plot(prune.tree)
text(prune.tree , pretty =0)
summary(prune.tree)

# prediction
yhat.tree = predict (reg.tree ,newdata=newdf)
yhat.tree


# Random Forrest model
rdm.frt <- randomForest(Crime ~., data=df, importance=TRUE, do.trace=100, ntree=100)
print(rdm.frt)

# prediction
yhat.rf = predict (rdm.frt ,newdata=newdf)
yhat.rf