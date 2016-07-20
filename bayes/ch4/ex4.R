# 1,2) モンテカルロ
n <- 10000
x <- numeric(n)
for (i in 1:n) {
  x[i] <- (runif(n=1, min=0, max=1)^2 + runif(n=1, min=0, max=1)^2 <1) * 4
}
mean(x)

# 3-5) マルコフ連鎖
l <- matrix(c(0.2,0.2,0.6, 0.1,0.6,0.3, 0.3,0.5,0.2), nrow = 3, ncol = 3, byrow = T)
p <- c(0.3, 0.2, 0.5)
n <- 10
for (i in 1:n) {
  p <- p %*% l
  print(p)
  if (i == n) { pn <- p }
}
pn #-> 定常分布は (0.1828, 0.4946, 0.3226)

# 6-7) 波平釣果問題の独立MH法

# size: サンプルサイズ
# burnin: バーンイン期間
# qme: 提案分布(正規分布)の期待値
# qme: 提案分布(正規分布)のSD
tsuri.ind.mh <- function(size, burnin, qme = 1.0, qsd = sqrt(0.5)) {
  set.seed(1234)     #乱数の種
  x<-numeric(size)
  x[1]<-rnorm(1, mean=qme, sd=qsd)
  co <- 0 # 採択数
  for (t in 2:size){
    a <- rnorm(1, mean=qme, sd=qsd)
    r <- (dnorm(x[t-1], mean=qme, sd=qsd)*dgamma(a,shape=11,rate=13))/
      (dnorm(a, mean=qme, sd=qsd)*dgamma(x[t-1],shape=11,rate=13))
    if (runif(1) < r) {
      x[t] <- a
      co <- co + 1
    }else{
      x[t] <- x[t-1]
    }
  }
  cat("採択率")
  print(round(co/size,2))
  
  x[(burnin+1):size]
}
mean(tsuri.ind.mh(10, 0))
mean(tsuri.ind.mh(100, 10))
mean(tsuri.ind.mh(1000, 100))
mean(tsuri.ind.mh(10000, 1000))
mean(tsuri.ind.mh(100000, 10000))

# グラフ
hist(tsuri.ind.mh(100000, 10000), breaks =50, xlab='', xlim=c(0,2.5), ylim=c(0,2.0), freq=F, main="", cex.axis=1.5)
par(new=T)
curve(dgamma(x,shape=11,rate=13),0,2.5,ylab='',xlab='',xlim=c(0,2.5),ylim=c(0,2.0),lwd=2.0,cex.axis=1.5)

## 平均3.0, 分散0.5の正規分布として計算
mean(tsuri.mh(100000, 10000, qme = 3.0, qsd = sqrt(0.5)))

# ランダムウォークMH
tsuri.rw.mh <- function(size, burnin, qme = 1.0, qsd = sqrt(0.5)) {
  set.seed(1234)     #乱数の種
  x<-numeric(size)
  x[1]<-rnorm(1, mean=qme, sd=qsd)
  co <- 0 # 採択数
  for (t in 2:size){
    a <- rnorm(1, x[t-1], sd=qsd)
    r <- (dgamma(a,shape=11,rate=13))/(dgamma(x[t-1],shape=11,rate=13))
    if (runif(1) < r) {
      x[t] <- a
      co <- co + 1
    }else{
      x[t] <- x[t-1]
    }
  }
  cat("採択率")
  print(round(co/size,2))
  x[(burnin+1):size]
}

mean(tsuri.rw.mh(100000, 1000))
mean(tsuri.rw.mh(100000, 1000, qsd = sqrt(0.001)))

# 9) 正選手問題のランダムウォークMH
sensyu.rw.mh <- function(size, burnin, qme = 1.0, qsd = sqrt(0.1)) {
  set.seed(1234)     #乱数の種
  x<-numeric(size)
  x[1] <- runif(1)
  co <- 0 # 採択数
  for (t in 2:size){
    a <- rnorm(1, x[t-1], sd=qsd)
    r <- dbeta(a, 10.2, 5.8) / dbeta(x[t-1], 10.2, 5.8)
    if (runif(1) < r) {
      x[t] <- a
      co <- co + 1
    }else{
      x[t] <- x[t-1]
    }
  }
  cat("採択率")
  print(round(co/size,2))
  x
}
mean(sensyu.rw.mh(100000, 1000)[1001:100000])

## トレースライン
par(mfrow=c(2,1))
plot(x[0:1000],type="l",ylab='',xlab='',lwd=0.5,cex.axis=1.5)
plot(x,type="l",ylab='',xlab='',lwd=0.5,cex.axis=1.5,ylim=c(3,3))
par(mfrow=c(1,1))




