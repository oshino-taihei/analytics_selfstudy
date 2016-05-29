da <- read.csv("gacoo_datascience/week5/dummydata_C.csv", stringsAsFactors=FALSE)
da$`１ヶ月平均CM視聴率` <- as.numeric(gsub(",","",da$`１ヶ月平均CM視聴率`))
da$`１ヶ月平均CM視聴率` <- as.numeric(gsub(",","",da$`１ヶ月平均CM視聴率`))

# 統計量
summary(da$`１ヶ月平均CM視聴率`)

# ヒストグラム
hist(da$`１ヶ月平均CM視聴率`, breaks = 40)

# クロス集計
ct <- table(da$CM視聴率の高低, da$居住地域)
ct["高視聴率層",] / sum(ct["高視聴率層",])

table(da$CM視聴率の高低, da$居住地域)
table(da$CM視聴率の高低, da$企業規模)
table(da$CM視聴率の高低, da$子ども有無)

#####
da2 <- read.csv("gacoo_datascience/week5/dummydata_D.csv", stringsAsFactors=FALSE)
da2$購入意向 <- as.numeric(gsub(",","",da2$購入意向))

ct2 <- table(da2$割付.A.B., da2$購入意向)
ct2
ct2["A",] / sum(ct2["A",])

mean_a <- (76+2*61+3*76+4*40+5*47) / sum(c(76,61,76,40,47))
mean_a
mean_b <- (21+2*43+3*91+4*68+5*77) / sum(c(21,43,91,68,77))
mean_b

#####
da3 <- read.csv("gacoo_datascience/week5/dummydata_E.csv", stringsAsFactors=FALSE)
da3$年間収入 <- as.numeric(gsub(",","",da3$年間収入))
da3$教養娯楽費 <- as.numeric(da3$教養娯楽費)

# ヒストグラム
hist(da3$年間収入)

# 回帰分析
da3.lm <- lm(年間収入 ~ . - ID - その他の消費支出, data = da3)
summary(da3.lm) #=> Multiple R-squared:  0.7959

# 中央値
median(da3$教養娯楽費)

# 散布図
plot(da3$年間収入, da3$教養娯楽費)

# 回帰分析
da3.lm2 <- lm(教養娯楽費 ~ 年間収入, data = da3)
summary(da3.lm2)

# 外れ値除外
da4 <- da3[da3$教養娯楽費 < 500000,]
da4.lm <- lm(教養娯楽費 ~ 年間収入, data = da4)
summary(da4.lm)

# 予測
predict(da4.lm, newdata = data.frame(年間収入 = 10000000))

