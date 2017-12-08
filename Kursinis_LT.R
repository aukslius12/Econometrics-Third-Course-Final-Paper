#Duomenys
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

pavadinimai <- c("paliko",
                 "pasitenkinimo_lygis",
                 "darbdavio_ivertinimas",
                 "projektu_skaicius",
                 "valandos_per_menesi",
                 "laikas_praleistas_kompanijoje",
                 "nelaime_darbovieteje",
                 "paaukstinimas_per_5metus",
                 "sritis",
                 "atlyginimas"
)

#Koreliacijos grafikas 1

library(corrplot)
HR_cor <- HR1 %>% 
  select(-sales) %>%
  mutate(salary = factor(salary, labels = c(3, 1, 2)))

names(HR_cor) <- pavadinimai[-9]


corrplot(cor(sapply(HR_cor, as.integer)), 
         method = "square", 
         cl.ratio = 0.2, cl.pos = "b", 
         tl.col = "black", tl.cex = 0.6, 
         addCoef.col = T, number.cex = 0.5)

#Grafikas SATISFACTION-EVALUATION BY GROUPS

library(gridExtra)

left_1 <- 
  HR1 %>% 
  filter(left == 1) %>%
  ggplot() +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation), color = "darkblue") +
  theme_light() +
  labs(x = "Pasitenkinimo Lygis", y = "Darbdavio Ivertinimas")+
  ggtitle("Isejo is darbo")

left_0 <- 
  HR1 %>% 
  filter(left == 0) %>%
  ggplot() +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation), color = "darkblue") +
  theme_light() +
  labs(x = "Pasitenkinimo Lygis", y = "Darbdavio Ivertinimas")+
  ggtitle("Liko kompanijoje")

grid.arrange(left_1, left_0, nrow = 1)


HR_left_2 <- HR1 %>% 
  filter(left == 1) %>%
  mutate(class = ifelse(
    last_evaluation > 75 & satisfaction_level > 70, "Gerai dirba - Gerai jauciasi", ifelse(
      last_evaluation > 75 & satisfaction_level < 15, "Gerai dirba - Blogai jauciasi", ifelse(
        last_evaluation < 60 & satisfaction_level %in% c(35:47), "Blogai dirba - Blogai jauciasi", "Kiti"
      )
    )
  ))

ggplot(data = HR_left_2) +
  geom_point(mapping = aes(x = satisfaction_level, y = last_evaluation, color = class)) +
  labs(x = "Pasitenkinimo Lygis", y = "Darbdavio Ivertinimas", color = "Klases")+
  theme_light()

#DENSITY GRAPHS BY VARIABLE

##1st GRAPH LEFT - SATISFACTION_LEVEL, LAST EVALUATION
grid.arrange(
  ggplot(data = HR1) +
    geom_density(mapping = aes(x = satisfaction_level, group = left, color = left), cex = 1) +
    labs(x = "Pasitenkinimo Lygis", y = "Tankis", color = "Paliko") +
    theme_light(),
  
  ggplot(data = HR1) +
    geom_density(mapping = aes(x = last_evaluation, group = left, color = left), cex = 1)+
    labs(x = "Darbdavio Ivertinimas", y = "Tankis", color = "Paliko") +
    theme_light(),
  ncol = 2)

##2nd NUMBER PROJECT AND TIME SPEND COMPANY
grid.arrange(
  ggplot(data = HR1) +
    geom_density(mapping = aes(x = time_spend_company, group = left, color = left), cex = 1) +
  labs(x = "Laikas praleistas kompanijoje", y = "Tankis", color = "Paliko") +
  theme_light(),
  
ggplot(data = HR1) +
  geom_density(mapping = aes(x = number_project, group = left, color = left), cex = 1) +
  labs(x = "Projektu skaicius", y = "Tankis", color = "Paliko") +
  theme_light(),
ncol = 2)
#3rd AVG HOURS, SALARY
library(forcats)
HR1 <- HR1 %>%
  mutate(salary = fct_recode(salary, 
                             "Mazas" = "low",
                             "Vidutinis" = "medium",
                             "Didelis" = "high")) %>%
  mutate(salary = fct_relevel(salary, c("Mazas", "Vidutinis", "Didelis")))


grid.arrange(
ggplot(data = HR1) +
  geom_density(mapping = aes(x = average_monthly_hours, group = left, color = left), cex = 1) +
  labs(x = "Vid. Valandos Darbe", y = "Tankis", color = "Paliko") +
  theme_light(),

ggplot(data = HR1) +
  geom_density(mapping = aes(x = salary, group = left, color = left), cex = 1) +
  labs(x = "Atlyginimas", y = "Tankis", color = "Paliko") +
  theme_light(),
ncol = 2)

#4th PROMOTION (USELESS), SALES (NO INFORMATION), WORK ACCIDENT (USELESS)

ggplot(data = HR1) +
  geom_bar(mapping = aes(x = sales, fill = left), position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Darbo Sritis", y = "", fill = "Isejo") +
  scale_x_discrete(labels = c("Apskaita",
                              "Zmogiskieji Istekliai",
                              "IT",
                              "Vadyba",
                              "Marketingas",
                              "Produktu Vadybininkai",
                              "Moksliniai Tyrimai ir Pletra",
                              "Pardavimai",
                              "Pagalbos Skyrius",
                              "Technika"
  ))

grid.arrange(
ggplot(data = HR1) +
  geom_bar(mapping = aes(x = work_accident, fill = left), position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Nelaime Darbovieteje", y = "", fill = "Isejo"),

ggplot(data = HR1) +
  geom_bar(mapping = aes(x = promotion_last_5years, fill = left), position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Paaukstinimas Pareigose", y = "", fill = "Isejo"),
ncol = 2)

##DEPARTMENT VS SATISFACTION LEVEL
HR1 %>% 
  group_by(sales) %>%
  summarize(mean = mean(satisfaction_level)) %>%
  arrange(desc(mean))
