# K-fold cross validation example

library(lattice)
library(ggplot2)
library(caret) # has the train() function

data("mtcars")
set.seed(42)

partitionRule <- createDataPartition(mtcars$mpg, p=0.8, list=F)
trainingSet <- mtcars[partitionRule,]
testingSet <- mtcars[-partitionRule,]

splitRule <- trainControl(method="cv", number=5)

lm1train <- train(mpg ~ ., data = mtcars, trControl = splitRule, method="lm", preProc=c("center", "scale"))

summary(lm1train)

plot(lm1train)

lm1test <- predict(lm1train, newdata = testingSet)

confusionMatrix(data = lm1test, testingSet$mpg)


lm2 <- lm(mpg ~ ., data = mtcars)

summary(lm2)

par(mfrow=c(2,2))
plot(lm2)
