library(MASS)
set.seed(416)

df <- read.table("breast-cancer-wisconsin.data.txt", sep = ',', 
                 stringsAsFactors = FALSE, header = FALSE)
head(df)
summary(df)
missing_value <- which(df$V7 == "?")
df[df == "?"] <- NA
# review missing value
for (i in c(2,3,4,5,6,7,8,9,10,11)){
  print(unique(df[,i]))
}

sum(is.na(df$V7))

# fill in with mode
library(modeest)
Mode = mlv(df[,7], method = "mfv")

df[,7] <- as.integer(replace(df[,7], is.na(df[,7]), Mode))
  
# fill in with regression
set.seed(416)
df2 <- read.table("breast-cancer-wisconsin.data.txt", sep = ',', 
                 stringsAsFactors = FALSE, header = FALSE)
head(df2)
summary(df2)
df2[df2 == "?"] <- NA
sum(is.na(df2$V7))
fit <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10,df2)
summary(fit)
AIC_fit <- stepAIC(fit, direction = "both", trace = FALSE)

df2[,7] <- as.integer(replace(df2[,7], is.na(df2[,7]), 
                              as.integer(predict(AIC_fit,df2[is.na(df2$V7),])+0.5)))
df2[missing_value,]

# fill in with regression with perturbation
set.seed(416)
df3 <- read.table("breast-cancer-wisconsin.data.txt", sep = ',', 
                  stringsAsFactors = FALSE, header = FALSE)
missing_value3 <- which(df3$V7 == "?")
df3[df3 == "?"] <- NA
fit3 <- lm(V7~V2+V3+V4+V5+V6+V8+V9+V10,df3)
AIC_fit3 <- stepAIC(fit3, direction = "both", trace = FALSE)
perturb <- rnorm(nrow(df3[missing_value3,]),0,0.7)
df3[,7] <- as.integer(replace(df3[,7], is.na(df3[,7]), 
                              predict(AIC_fit3,df3[is.na(df3$V7),])+perturb+0.5))
df3[missing_value3,]
