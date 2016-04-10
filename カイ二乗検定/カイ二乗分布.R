# 標準正規分布から自由度dfの数だけサンプルを抽出し、それらの二乗和を求める
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}

# カイ二乗分布
df <- 100
x <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(x, freq=FALSE, breaks=30, ylim=c(0, 0.03))
curve(dchisq(x, df), add=TRUE)


