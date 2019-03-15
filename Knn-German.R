ger <- read.csv("D:\\MSBA\\Spring 2019\\MSBX5415\\germancredit.csv")

nrow(ger)

library(class)

str(ger)

nums <- unlist(lapply(ger, is.numeric)) 

ger.1 <- ger[,nums]
ger.1$Default <- as.factor(ger.1$Default)

set.seed(200)

str(ger.1)

train <- sample(nrow(ger.1), nrow(ger.1)/(5/3))
ger.train <- ger.1[train, ]
ger.test <- ger.1[-train, ]

ger.knn <- knn(scale(ger.train[, sapply(ger.1, is.numeric)]), 
                scale(ger.test[, sapply(ger.1, is.numeric)]), 
                ger.train$Default, k = 5)

table(ger.test$Default, ger.knn)
#accuracy <- mean(ger.test$Default == ger.knn)

ger.knn <- knn(scale(ger.train[, sapply(ger.1, is.numeric)]), 
               scale(ger.test[, sapply(ger.1, is.numeric)]), 
               ger.train$Default, k =11)

table(ger.test$Default, ger.knn)

vk <- seq(1, 31, 2)
accuracy <- vk
for (i in 1:length(vk)) {
  ger.knn <- knn(scale(ger.train[, sapply(ger.1, is.numeric)]), 
                  scale(ger.test[, sapply(ger.1, is.numeric)]), 
                  ger.train$Default, k = vk[i])
  accuracy[i] <- mean(ger.test$Default == ger.knn)
}
plot(vk, accuracy, xlab = "k", ylab = "test accuracy", col = "blue")

ger.knn <- knn(scale(ger.train[, sapply(ger.1, is.numeric)]), 
               scale(ger.test[, sapply(ger.1, is.numeric)]), 
               ger.train$Default, k =21)

table(ger.test$Default, ger.knn)