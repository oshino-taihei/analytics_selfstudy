# animationパッケージ
## カイ二乗分布 自由度1~5
library(animation)
saveGIF({
  ani.options(loop = TRUE)
  for (i in 1:5) {
    .main = paste("カイ二乗分布 自由度=", i, seq="")
    curve(dchisq(x, df=i), xlim=c(0,10), ylim=c(0, 0.3), main=.main)
  }
}, movie.name="chi.gif", interval=0.5)

# ファイル出力
## カイ二乗分布 自由度10~100
for (i in seq(10, 100, 10)) {
  .main = paste("カイ二乗分布 自由度=", i, seq="")
  .file <- paste("out/chi", sprintf("%03d", i), ".png", sep="")
  png(.file)
  curve(dchisq(x, df=i), xlim=c(0,200), main=.main)
  dev.off()
}
