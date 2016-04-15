# B(n,p)の二項分布に従う試行をしたときの成功回数
bin <- function(n, p = 0.5) {
  sum(sample(0:1, size = n, replace = TRUE, prob = c(p, 1 - p)))
}

# B(n,p)のヒストグラムを描画
hist_bin <- function(n = 20, p = 0.5) {
  x <- sapply(c(1:10000), function(x) { bin(n, p)} )
  main <- paste("二項分布 B(", n, ", ", p, ")", seq="")
  hist(x, breaks=0:n, freq=FALSE, main=main, ylim=c(0,0.30))
}
hist_bin(20,0.5)
curve(dbinom(x, 20, 0.5), 0, 20, 21, add=TRUE)

# パラメータを連続して変更させながら描画

# B(n=20, p=[0.1,0.9])
for (i in seq(0.1, 0.9, 0.1)) {
  .file <- paste("out/plot", i, ".png", sep="")
  png(.file)
  hist_bin(20, i)
  dev.off()
}

# B(n=[10,100], p=0.5)
for (i in seq(10, 100, 10)) {
  .file <- paste("out/plot", sprintf("%03d", i), ".png", sep="")
  png(.file)
  hist_bin(i, 0.5)
  dev.off()
}
