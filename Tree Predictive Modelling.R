flight <- read.csv("D:\\MSBA\\Spring 2019\\MSBX5415\\delay-UA-cleaned.csv")

flight$ARR_DEL15 <- as.factor(flight$ARR_DEL15)

set.seed(88)

lmod <- lm(ARR_DELAY~DAY_OF_MONTH, data=flight)

summary(lmod)

lmod <- lm(ARR_DELAY~CRS_DEP_TIME, data=flight)

summary(lmod)


lmod <- lm(ARR_DELAY~ACTUAL_ELAPSED_TIME, data=flight)

summary(lmod)

lmod <- lm(ARR_DELAY~DISTANCE, data=flight)

summary(lmod)

lmod <- lm(ARR_DELAY~log(DAY_OF_MONTH), data=flight)

summary(lmod)

lmod <- lm(ARR_DELAY~sqrt(DAY_OF_MONTH), data=flight)

summary(lmod)

lmod <- lm(ARR_DELAY~(DAY_OF_MONTH)^2, data=flight)

summary(lmod)

flight$ORIGIN <- factor(flight$ORIGIN)
flight$DEST <- factor(flight$DEST)

lmod <- lm(ARR_DELAY~. - ARR_DEL15, data=flight)

summary(lmod)

str(flight)

train <- sample(nrow(flight), 0.6 * nrow(flight))
flight.train <- flight[train, ]
flight.test <- flight[-train, ]


lmod <- lm(ARR_DELAY~. - ARR_DEL15, data=flight.test)

sm <- summary(lmod)

mean(sm$residuals^2)
sm$sigma^2 * sm$fstatistic[3]/(1+sum(sm$fstatistic[2:3]))

lmod <- glm(ARR_DEL15~. -ARR_DELAY, data=flight, family = binomial)

summary(lmod)
                
lmod <- glm(ARR_DEL15~. -ARR_DELAY, data=flight.train, family = binomial)
delay.prob <- predict(lmod, newdata = flight.test, type = "response")
delay.pred <- as.numeric(delay.prob >0.5)
table(flight.test$ARR_DEL15, delay.pred)
summary(lmod)   

library(tree)

tree.train <- tree(ARR_DEL15 ~ . -ARR_DELAY, data = flight.train, 
                   control = tree.control(nobs = length(train), mindev = 0.001))
# confusion matrix using test data
tree.pred <- predict(tree.train, newdata = flight.test, type = "class")
table(flight.test$ARR_DEL15, tree.pred)

library(randomForest)
bag.appt <- randomForest(ARR_DEL15 ~ . - ARR_DELAY, data = flight, mtry = 9, importance = TRUE)
pred.bag <- predict(bag.appt)
table(flight$ARR_DEL15, pred.bag)

importance(bag.appt)