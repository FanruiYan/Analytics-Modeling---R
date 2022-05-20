library(GGally)

df <- read.table("uscrime.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)

#head(df)
#summary(df)

pca <- prcomp(df[,1:15], retx = TRUE, center = TRUE, scale. = TRUE)
summary(pca)

pca$rotation

screeplot(pca, type="lines", col="blue", nps=15)

# since this is ordered by importance, and the screeplot recommends 5 components
pcadf5 <- pca$x[,1:5]
head(pcadf5)

pcadata5 <- cbind(pcadf5, df[,16])
head(pcadata5)

fit5 <- lm(V6~., data=as.data.frame(pcadata5))
summary(fit5)

# try 4 components
pcadf4 <- pca$x[,1:4]
head(pcadf4)

pcadata4 <- cbind(pcadf4, df[,16])
head(pcadata4)

fit4 <- lm(V5~., data=as.data.frame(pcadata4))
summary(fit4)

# try 3 components
pcadf3 <- pca$x[,1:3]
head(pcadf3)

pcadata3 <- cbind(pcadf3, df[,16])
head(pcadata3)

fit3 <- lm(V4~., data=as.data.frame(pcadata3))
summary(fit3)

# PC5 gives the best R squared
beta.Z <- as.matrix(fit$coefficients[2:6])
V <- as.matrix(pca$rotation[,1:5])
beta.X <- V %*% beta.Z
beta.X
t(t(pca$x %*% t(pca$rotation)) * pca$scale + pca$center)
