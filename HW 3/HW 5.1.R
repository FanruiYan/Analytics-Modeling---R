library(outliers)
df <- read.table("data 5.1/uscrime.txt", sep = '\t', stringsAsFactors = FALSE, header = TRUE)
str(df[,"Crime"])
# there are 47 data points, it's small sample, so we can use grubbs.test to check outliers
boxplot(df[,"Crime"])
plot(df[,"Crime"])
# test largest point
one_outlier_h <- grubbs.test(df[,"Crime"], type = 10, opposite = FALSE)
# test smallest point
one_outlier_l <- grubbs.test(df[,"Crime"], type = 10, opposite = TRUE)
