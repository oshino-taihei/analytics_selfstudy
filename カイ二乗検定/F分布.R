# 標準正規分布から自由度dfの数だけサンプルを抽出し、それらの二乗和を求める
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}

# F値(2つのカイ二乗値を自由度で割った値の比)を求める
f_value <- function(df1, df2) {
  (kai2(df1) / df1) / (kai2(df2) / df2)
}

# F分布
df1 <- 10
df2 <- 20
x <- sapply(c(1:10000), function(x) { f_value(df1, df2) } )
hist(x, freq=FALSE, breaks=30, main="F分布" )
curve(df(x, df1, df2), add=TRUE)
