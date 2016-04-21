df <- read.csv("introduction_to_statistics/chapter03/ex3-1.csv")
df

# 散布図
plot(df$自民得票率, df$持ち家比率)

# 相関係数
自民得票率平均 <- mean(df$自民得票率)
持ち家比率平均 <- mean(df$持ち家比率)
r分子 <- sum((df$自民得票率 - 自民得票率平均) * (df$持ち家比率 - 持ち家比率平均))
r分母 <- sqrt(sum((df$自民得票率 - 自民得票率平均)^2)) * sqrt(sum((df$持ち家比率 - 持ち家比率平均)^2))
r <- r分子 / r分母
r

cor(df$自民得票率, df$持ち家比率)

# 回帰分析
回帰分析 <- lm(持ち家比率 ~ 自民得票率, df)
summary(回帰分析)

# 回帰直線
abline(回帰分析)

