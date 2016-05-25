da <- read.csv("gacoo_datascience/week4/dummydata_A.csv", stringsAsFactors=FALSE)
da$食費 <- as.numeric(gsub(",","",da$食費))
da$年間収入 <- as.numeric(gsub(",","",da$年間収入))
da$光熱.水道費 <- as.numeric(gsub(",","",da$光熱.水道費))

# 散布図
plot(da$年間収入, da$食費)

# 食費と年間収入を回帰分析
da.lm <- lm(食費 ~ 年間収入, data = da)
abline(da.lm) # 回帰直線を引く
summary(da.lm) #=> Multiple R-squared:  0.3934

# 年間収入600万の場合の食費を予測
predict(da.lm, newdata = data.frame(年間収入 = 6000000))
#=> 63685.36

# 光熱・水道費と年間収入を回帰分析
da.lm2 <- lm(光熱.水道費 ~ 年間収入, data = da)
abline(da.lm2) # 回帰直線を引く
summary(da.lm2) #=> Multiple R-squared:  0.1846
