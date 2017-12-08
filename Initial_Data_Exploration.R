### 1. DUOMENU PARUOSIMAS

library(tidyverse)
HR <- read_csv("D:/Projects/Studies/Kursinis/HR_comma_sep.csv")

(sapply(HR, class))

HR <- as_tibble(cbind(HR[1:5], sapply(HR[6:10], as.factor)))
(sapply(HR1, class))


HR1 <- HR %>%
  mutate(satisfaction_level = satisfaction_level*100,
         last_evaluation = last_evaluation*100)


#Isimsime kintamuosius kurie nedaro itakos o labiau yra kitas "satisfaction" matavimo vienetas ir
#savaime aisku daro itaka. (Pats "sau" daro itaka)

HR1 <- HR1 %>%
  select(-last_evaluation, -left)

### 2. APRASOMOJI STATISTIKA

#-- Graph on SALES v MEAN_SATISFACTION --#

HR1 %>%
  group_by(sales) %>%
  summarize(mean_satisfaction = mean(satisfaction_level)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = sales, y = mean_satisfaction), fill = "lightblue", stat = "identity") +
  geom_hline(mapping = aes(yintercept = mean(mean_satisfaction)), color = "red") +
  coord_cartesian(ylim=c(1,100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#----#

HR1 %>%
  group_by(sales) %>%
  summarize(mean(number_project), mean(average_montly_hours), mean(time_spend_company))

#DOESN'T LOOK LIKE ANYTHING TO ME.
#----#

HR1 %>%
  group_by(sales) %>%
  ggplot() +
  geom_bar(mapping = aes(x = sales, fill = salary), position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
 
#Hello there frend managements
#----#

ggplot(data = HR1) +
  geom_point(mapping = aes(x = satisfaction_level, y = average_montly_hours, color = number_project), position = "jitter")

#BREAKING NEWS - MORE PROJECTS CORELATE WITH MORE MONTHLY HOURS!!!
#Also less satisfaction
#----#


#----#
#We'll now try to explain the change since satisfaction itself CANT BE EXPLAINED BY ALL THIS FUCKING SHIT FUCK 

#-- Duomenu tvarkymas --#

HR2 <- HR %>%
  select(-left)

HR2 <- HR2 %>%
  mutate(satisfaction_change = satisfaction_level - last_evaluation) %>%
  select(-satisfaction_level, -last_evaluation) %>%
  select(satisfaction_change, average_montly_hours:salary)

#----#

#-- Aprasomoji statistika --#

cor(HR2[1:3])
#Hey, that's preddygud

#----#

HR2 %>%
  group_by(sales) %>%
  summarize(mean_satisfaction = mean(satisfaction_change)*-1) %>%
  ggplot() +
  geom_bar(mapping = aes(x = sales, y = mean_satisfaction), fill = "lightblue", stat = "identity") +
  geom_hline(mapping = aes(yintercept = mean(mean_satisfaction)), color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
#HEY, THAT's PREDDY GUD

#----#

ggplot(data = HR2) +
  geom_point(mapping = aes(x = sales, y = satisfaction_change, color = sales, size = salary))

summary(lm(satisfaction_change~., data = HR2))
#----#

#### 3. MODELIAI

#-- Testas 1 --#
management_data <- HR1 %>%
  filter(sales == "management") %>%
  select(-sales)

summary(lm(satisfaction_level ~ ., data = management_data))
#nice
summary(lm(satisfaction_level ~ . -promotion_last_5years -time_spend_company -average_montly_hours, data = management_data))
#Rsq = 0.05 B==D ~
#Visu kitu taip pat mazas :)
#----#

#-- Testas 2 --#

salary_data <- HR1 %>%
  filter(salary == "medium") %>%
  select(-salary)

summary(lm(satisfaction_level ~ ., data = salary_data))
#----#

fit_auto <- regsubsets(satisfaction_level ~ ., data = HR1, nvmax = 8)

fit1 <- lm(satisfaction_level ~ ., data = HR1)
(summary(fit1))

fit_no_sales <- lm(satisfaction_level ~ . - sales, data = HR1)
(summary(fit_no_sales))


fit_sales <- lm(satisfaction_level ~ sales, data = HR1)
(summary(fit_sales))
