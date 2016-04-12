# 中心極限定理の実験

# サイコロを100回振ったときの目の平均を求める実験を10000回繰り返す
par(mfrow=c(1,2))
## 元のデータの分布
sai.data = sample(6,100, replace=TRUE)
hist(sai.data)
## 平均の分布
sai.means = sapply(c(1:10000), function(x) {mean(floor(runif(100, 1, 7)))} )
hist(sai.means)



