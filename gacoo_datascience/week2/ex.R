da <- read.csv("gacoo_datascience/week2/dummydata_A.csv", stringsAsFactors=FALSE)
da$食費 <- as.numeric(sub(",","",da$食費))

# 2-1. 食費
(食費の平均値 <- mean(da$食費))
(食費の分散 <- var(da$食費))
(食費の標準偏差 <- sd(da$食費))

# 2-2. 度数分布表
hist(da$食費, breaks = seq(0,250000,10000))

# 2-5.
da$住居費 <- as.numeric(sub(",","",da$住居費))
da$保健医療費 <- as.numeric(sub(",","",da$保健医療費))
da$交通通信費 <- as.numeric(sub(",","",da$交通.通信費))

(食費_標準偏差 <- sd(da$食費, na.rm=TRUE))
(住居費_標準偏差 <- sd(da$住居費, na.rm=TRUE))
(保健医療費_標準偏差 <- sd(da$保健医療費, na.rm=TRUE))
(交通通信費_標準偏差 <- sd(da$交通通信費, na.rm=TRUE))
