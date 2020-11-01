title: "SVM"
output:
  html_document:
    df_print: paged
---


```{r}
set.seed(456)
```

```{r}
x <-  matrix(rnorm(100), 50, 2)
x
```

```{r}
y <-  factor(rep(c(-1, 1), c(25, 25)))
y
```

```{r}
x[y == 1, ] <- x[y == 1, ] + 3
x
```

```{r}
newdata <- data.frame(x,y)
head(newdata)
```

```{r}
colnames(newdata) <- c("panjangSepal", "lebarSepal", "Iris")
newdata
```

```{r}
plot(newdata$panjangSepal,newdata$lebarSepal, col = ifelse(y=="1", "blue", "red"), pch = 19)
```

```{r}
library(e1071)
modelSVM1 <- svm(Iris ~ ., data = newdata, kernel = "linear")
print(modelSVM1)
```

```{r}
plot(modelSVM1, newdata)
```

```{r}
library(caret)
set.seed(1)
trainIndex <- createDataPartition(newdata$Iris, p = 0.6)$Resample1
trainingSet <- newdata[trainIndex, ]
testingSet <- newdata[-trainIndex, ]
trainingSet
testingSet
```

```{r}
summary(newdata)
```

```{r}
library(caret)
train_control <- trainControl(method="cv", number=20)
modelNB <- train(Iris ~ lebarSepal + panjangSepal, method = "nb", trControl = train_control, data = newdata)
modelNB$resample
confusionMatrix(modelNB)
```

```{r}
train_control <- trainControl(method="repeatedcv", number=20, repeats=100)
modelNB <- train(Iris ~ lebarSepal + panjangSepal, method = "nb", trControl = train_control, data = newdata)
modelNB$resample
confusionMatrix(modelNB)
```

```{r}
train_control <- trainControl(method="LOOCV")
modelNB <- train(Iris ~ lebarSepal + panjangSepal, method = "nb", trControl = train_control, data = newdata)
modelNB$results
```