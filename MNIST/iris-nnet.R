# 150件のirisを100件のトレーニングデータと50件のテストデータにランダム・サンプリング
training.index <- sample(1:nrow(iris), 50)
iris.train <- iris[training.index,]
iris.test <- iris[-training.index,]

# トレーニングデータをニューラルネットで学習
iris.nnet <- nnet(Species ~ ., size=3, data=iris.train)

# テストデータを使って評価
iris.result <- predict(iris.nnet, iris.test, type="class")

# 結果を表示
result <- table(iris.test$Species, iris.result, dnn = c("Actual", "Predicted"))
accuracy <- sum(diag(result)) / nrow(iris.test)

result
accuracy
