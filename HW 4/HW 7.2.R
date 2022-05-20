library(forcats)
df <- read.table("temps.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)
dt <- as.vector(unlist(df[,2:21]))
tsdf <- ts(dt, start=1996, frequency = 123)
plot(decompose(tsdf))

# Triple exponential with seasonal "add" and trend
# reason for additive is temperature is absolute terms
fit3 <- HoltWinters(tsdf)
par(mfrow = c(2,2))
plot(fitted(fit3)[,1])
plot(fitted(fit3)[,2])
plot(fitted(fit3)[,3])
plot(fitted(fit3)[,4])
head(fitted(fit3))
# write data into csv, 123 data points are missingfilled with original data
table3 <- matrix(fitted(fit3)[,4], nrow=123)
head(table3)
write.csv(data.frame(table3), "xhat.csv")

# Simple exponential
fit <- HoltWinters(df[,2:21], beta = FALSE, gamma = FALSE)
  # write data into csv, 1 data point is missing filled with original data
table <- matrix(, nrow=123, ncol=20)
for (i in 1:20){
  for (j in 1:123){
    if (i==1 & j==1){table[,i][j] = df[,(i+1)][j]}
    else{table[,i][j] = fitted(fit)[,2][(123*(i-1)+j-1)]}
  }}
write.csv(data.frame(table), "xhat.csv")


#Double exponential with trend
fit2 <- HoltWinters(df[,2:21], gamma = FALSE)
  # write data into csv, 2 data points are missing filled with original data
table2 <- matrix(, nrow=123, ncol=20)
for (i in 1:20){
  for (j in 1:123){
    if (i==1 & j==1){table2[,i][j] = df[,(i+1)][j]}
    else if (i==1 & j==2){table2[,i][j] = df[,(i+1)][j]}
    else{table2[,i][j] = fitted(fit2)[,2][(123*(i-1)+j-2)]}
  }}
write.csv(data.frame(table2), "xhat2.csv")




     