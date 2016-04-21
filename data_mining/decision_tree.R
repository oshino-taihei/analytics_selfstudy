library(rpart)
library(rpart.plot)

# タイタニック号の生存データ準備
tmp <- data.frame(Titanic)
df_titanic <- data.frame(
  Class = rep(tmp$Class, tmp$Freq),
  Sex = rep(tmp$Sex, tmp$Freq),
  Age = rep(tmp$Age, tmp$Freq),
  Survived = rep(tmp$Survived, tmp$Freq)
)

# 決定木作成
dt <- rpart(Survived ~ ., data = df_titanic, method = "class")

# 決定木描画
rpart.plot(dt, type=1, uniform = T, extra = T, under = 1, faclen = 0)
