library(tidyverse)
library(caret)
library(e1071)
library(randomForest)

HR <- read_csv("D:/Projects/Studies/Kursinis/HR_comma_sep.csv")

HR <- as_tibble(cbind(HR[1:5], sapply(HR[6:10], as.factor)))
HR <- HR %>%
  mutate(average_monthly_hours = average_montly_hours, work_accident = Work_accident) %>%
  select(-average_montly_hours, -Work_accident)

HR1 <- HR %>%
  mutate(satisfaction_level = satisfaction_level*100,
         last_evaluation = last_evaluation*100) %>%
  select(left, satisfaction_level:work_accident)
HR_train <- HR1 %>%
  sample_n(nrow(HR1)*0.7)

HR_test <- HR1 %>%
  anti_join(HR_train)

#LOGISTIC MODEL BASIC
library(caret)

glm_fit <- glm(left ~., data = HR_train, family = binomial(link = "logit"))

predictions <- predict(glm_fit, newdata = HR_test, type = "response")
confusionMatrix(ifelse(predictions > 0.5, 1, 0), HR_test$left)

#Odds ratios
data.frame(exp(glm_fit$coefficients))

#LOGISTIC MODEL MOST ACCURATE

glm_fit_best <- glm(left ~ . - promotion_last_5years +
                      I(satisfaction_level^-1) + I(satisfaction_level^-2) + I(satisfaction_level^-3) +I(satisfaction_level^-5) +
                      I(last_evaluation^-2) + I(last_evaluation^-3) + I(last_evaluation^-4) + I(last_evaluation^-5)+ I(last_evaluation^-6)+
                      I(last_evaluation^-7) + I(last_evaluation^-8) + I(last_evaluation^-9) + I(last_evaluation^-10) +
                      I(number_project^2) + I(number_project^3) + I(number_project^4) +
                      I(time_spend_company^2) + I(time_spend_company^3) + I(time_spend_company^4) +#Sitas ir (3,4)
                      I(average_monthly_hours^-2) + I(average_monthly_hours^-3) + I(average_monthly_hours^-4),# (4) Sitas duoda warningus
                    data = HR_train, family = binomial())

predictions <- predict(glm_fit_best, newdata = HR_test, type = "response")
confusionMatrix(ifelse(predictions > 0.5, 1, 0), HR_test$left)

#SVM MODEL

svm_fit <- svm(left ~., data = HR_train, cost = 10, gamma = 0.1)

predictions <- predict(svm_fit, newdata = HR_test)
confusionMatrix(predictions, HR_test$left)

tune_results <- tune(svm, train.x=left ~ ., data = HR_train,
                     ranges=list(cost=10^(-1), gamma=10^(-1)))

#------------------------------
#### STORE INTERMEDIATE RESULTS
#### 
#------------------------------
costs <- 10^(-10:9)
gammas <- 10^(-17:2)

(t_0 <- Sys.time())
tune_results <- list()
for (i in c(5,10,15,20)){
  tune_results[[i]] <- tune(svm, train.x=left ~ ., data = HR_train,
                            ranges=list(cost=costs[i-4:i], gamma=gammas[i-4:i]))
  beepr::beep()
  print (Sys.time() - t_0)
}


for (i in 1:5) beepr::beep()


### New results:

#6-10
#  cost gamma
#  1e-05 1e-12
#This is shit

# 11-15
#  cost gamma
#     1 1e-07
#This is also shit

svm_fit <- svm(left ~., data = HR_train, cost = 10, gamma = 0.1)

predictions <- predict(svm_fit, newdata = HR_test)
confusionMatrix(predictions, HR_test$left)
#Old results were the best results - I've tried to reach the sun, yet burned my wings and fucking died.

### Old results:

#- sampling method: 10-fold cross validation 
#
#- best parameters:
#  cost gamma
#   10   0.1

#- best performance: 0.02790749 
#0.9588 - 0.9742 accuracy increasae


#NAIVE BAYES

naive_bayes_fit <- naiveBayes(left ~., data = HR_train)

predictions <- predict(naive_bayes_fit, newdata = HR_test)
confusionMatrix(predictions, HR_test$left)

#RANDOM FOREST

rf_fit <- randomForest(left ~ .,
                       data = HR_train)

predictions <- predict(rf_fit, newdata = HR_test)
confusionMatrix(predictions, HR_test$left)

#Fucking niggers ass, THIS SHIT TAKES MATRICES ONLY WHAT THE FUCK IS THIS 2001.
x <- as.data.frame(HR_train[,2:10])
x[[8]] <- as.numeric(x[[8]])
y <- pull(as.data.frame(HR_train[,1]))

ntrees <- c(100, 200, 500, 1000, 1500, 2000, 2500)
tune_rf_res <- list()
for (tr in ntrees){
  temp_res <- tuneRF(x, y, stepFactor=1.5, improve=1e-10, ntree=tr)
  tune_rf_res <- c(tune_rf_res, temp_res)
}

for (tr in ntrees){
  temp_res <- tuneRF(x, y, stepFactor=2, improve=1e-10, ntree=tr)
  tune_rf_res <- c(tune_rf_res, temp_res)
}

#Results:
#Optimal values are ntrees = 500, and mtry = 6. Increasing ntrees isn't viable
rf_fit <- randomForest(left ~ .,
                       data = HR_train, ntree = 500, mtry = 6)

predictions <- predict(rf_fit, newdata = HR_test)
confusionMatrix(predictions, HR_test$left)

#Holyshit, 99% accuracy.

## ROC Curves

library(pROC)
plot.roc()