# 中心極限定理の実験

# 表(1)が2/3、裏(0)が1/3の確率で出るコインを100回投げたときの
# 表、裏それぞれの出た回数の平均を求める実験を10000回繰り返す
par(mfrow=c(1,2))
# n回コインを投げ、その結果のリストを返す
flip_coin <- function(n) {
  table(sample(c("表","裏"), n, prob=c(2:1), replace=TRUE))
}
# データの分布
coin.data = flip_coin(100)
coin.data

# 平均の分布
coin.dataset = sapply(c(1:10000), function(x) {flip_coin(100)} )
coin.means.omote = coin.dataset["表",]
coin.means.ura = coin.dataset["裏",]
hist(coin.means.omote)
hist(coin.means.ura)
