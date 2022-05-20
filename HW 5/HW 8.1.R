library(MASS)
df <- read.table("uscrime.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)
str(df[,"Crime"])
head(df)
summary(df)
plot(df)

df[,12] <- log(df[,12])

#fit
fit <- lm(Crime~., data=df)
summary(fit)
# Stepwise regression model
AIC_fit <- stepAIC(fit, direction = "both", trace = FALSE)
summary(AIC_fit)

# predict
newdf <- data.frame (M = 14.0,
                     So = 0,
                     Ed = 10.0,
                     Po1 = 12.0,
                     Po2 = 15.5,
                     LF = 0.640,
                     M.F = 94.0,
                     Pop = 150,
                     NW = 1.1,
                     U1 = 0.120,
                     U2 = 3.6,
                     Wealth = log(3200),
                     Ineq = 20.1,
                     Prob = 0.04,
                     Time = 39.0)
newdf
crime_hat <- predict(AIC_fit, newdata=newdf)
crime_hat