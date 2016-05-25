da <- read.csv("gacoo_datascience/week3/dummydata_A.csv", stringsAsFactors=FALSE)
da$教養娯楽費 <- as.numeric(gsub(",","",da$教養娯楽費))
da$食費 <- as.numeric(gsub(",","",da$食費))
da$年間収入 <- as.numeric(gsub(",","",da$年間収入))

# 散布図
plot(da$食費, da$教養娯楽費)

# 相関係数