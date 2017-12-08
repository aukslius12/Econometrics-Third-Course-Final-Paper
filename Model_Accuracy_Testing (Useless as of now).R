#Model accuracy testing.
acc <- as.numeric()
for (i in 1:1000){
  HR_train <- HR1 %>%
    sample_n(nrow(HR1)*0.7)
  
  HR_test <- HR1 %>%
    anti_join(HR_train)
  
  fit_best <- glm(left ~ . +
                    -satisfaction_level + 
                    I(satisfaction_level^-1) + I(satisfaction_level^-2) + 
                    I(last_evaluation^2) + I(last_evaluation^3) + I(last_evaluation^4) + 
                    log(number_project) +
                    I(time_spend_company^2) + I(time_spend_company^3) + I(time_spend_company^4) +
                    I(average_monthly_hours^-2) + I(average_monthly_hours^-3) + I(average_monthly_hours^-4),
                  data = HR_train, family = binomial())
  
  predictions <- predict(fit_best, newdata = HR_test, type = "response")
  conf_matrix <- table(HR_test$left, predictions > 0.5)
  #HEY THAT'S PREDDY GOOD WTF
  
  #Model accuracy
  
  acc[i] <- (conf_matrix[2,2] + conf_matrix[1,1]) / (sum(conf_matrix))
}
