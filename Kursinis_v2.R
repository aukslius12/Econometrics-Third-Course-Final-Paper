library(tidyverse)
HR <- read_csv("D:/Projects/Studies/Kursinis/HR_comma_sep.csv")

HR <- as_tibble(cbind(HR[1:5], sapply(HR[6:10], as.factor)))
HR <- HR %>%
  mutate(average_monthly_hours = average_montly_hours, work_accident = Work_accident) %>%
  select(-average_montly_hours, -Work_accident)

HR1 <- HR %>%
  mutate(satisfaction_level = satisfaction_level*100,
         last_evaluation = last_evaluation*100) %>%
  select(left, satisfaction_level:work_accident)

#Plots

library(gridExtra)

left_1 <- 
  HR1 %>% 
  filter(left == 1) %>%
  ggplot() +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation), color = "darkblue") +
  theme_light() +
  ggtitle("Isejo is darbo")

left_0 <- 
  HR1 %>% 
  filter(left == 0) %>%
  ggplot() +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation), color = "darkblue") +
  theme_light() +
  ggtitle("Liko kompanijoje")

grid.arrange(left_1, left_0, nrow = 1)


HR_left_2 <- HR_left %>%
  mutate(class = ifelse(
    last_evaluation > 75 & satisfaction_level > 70, "Gerai dirba - Gerai jauciasi", ifelse(
      last_evaluation > 75 & satisfaction_level < 15, "Gerai dirba - Blogai jauciasi", ifelse(
        last_evaluation < 60 & satisfaction_level %in% c(35:47), "Blogai dirba - Blogai jauciasi", "Kiti"
      )
    )
  ))

ggplot(data = HR_left_2) +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation, color = class)) +
  theme_light()
  

 
proj_1 <- HR1 %>% 
  filter(left == 1) %>%
  ggplot() +
  geom_point(mapping = aes(x = number_project, y = last_evaluation), color = "darkblue") +
  theme_light() +
  ggtitle("Projektu skaicius - Darbuotojo ivertinimas") +
  geom_smooth(mapping = aes(x = number_project, y = last_evaluation), method = "loess")

proj_2 <- HR1 %>% 
  filter(left == 1) %>%
  ggplot() +
  geom_point(mapping = aes(x = number_project, y = average_monthly_hours), color = "darkblue") +
  theme_light() +
  ggtitle("Projektu skaicius - Vidutine darbo trukme") +
  geom_smooth(mapping = aes(x = number_project, y = average_monthly_hours), method = "loess")

grid.arrange(proj_1, proj_2, nrow = 1)

#Corplots

library(corrplot)
HR_cor <- HR1 %>% 
  select(-sales) %>%
  mutate(salary = factor(salary, labels = c(3, 1, 2)))
  

corrplot(cor(sapply(HR_cor, as.integer)), 
         method = "square", 
         cl.ratio = 0.2, cl.pos = "b", 
         tl.col = "black", tl.cex = 0.8, 
         addCoef.col = T, number.cex = 0.5)

HR_cor_0 <- HR_cor %>%
  filter(left == 0) %>%
  select(-left)

corrplot(cor(sapply(HR_cor_0, as.integer)), 
         method = "square",
         cl.ratio = 0.2, cl.pos = "b", 
         tl.col = "black", tl.cex = 0.8, 
         addCoef.col = T, number.cex = 0.5)

HR_cor_1 <- HR_cor %>%
  filter(left == 1) %>%
  select(-left)

corrplot(cor(sapply(HR_cor_1, as.integer)), 
          method = "square",
          cl.ratio = 0.2, cl.pos = "b", 
          tl.col = "black", tl.cex = 0.8,
          addCoef.col = T, number.cex = 0.5)

#Models

#Splitting to train and test data

HR_train <- HR1 %>%
  sample_n(nrow(HR1)*0.7)

HR_test <- HR1 %>%
  anti_join(HR_train)

#Model with everything in it.
summary(fit_best <- glm(left ~ ., data = HR_train, family = binomial()))
 
summary(glm(left ~ . -sales, data = HR_train, family = binomial())) 
summary(glm(left ~  satisfaction_level + time_spend_company + number_project + last_evaluation, data = HR_train, family = binomial())) 
#Etc.
#Negalime pasalinti sales is tyrimo nes mazeja AIC

#LOG NUMBER PROJECT INCREASES ACCURACY LOL LETS FUCKING GO

#fit_best <- 
summary()#)

fit_best$aic
#This is the best combination of all the INTEGERS in the dataset

#Visi atrodo reiksmingi, todel modelio tobulint not sure ar reikia

#Testavimas

predictions <- predict(fit_best, newdata = HR_test, type = "response")
conf_matrix <- table(HR_test$left, predictions > 0.5)
#HEY THAT'S PREDDY GOOD WTF

#Model accuracy

(`Model Accuracy` <- (conf_matrix[2,2] + conf_matrix[1,1]) / (sum(conf_matrix)))

#HEY I THINK THAT IS GOOD WTFFFFFFFFFF

##RSQ
library(pscl)
pR2(fit_best)

