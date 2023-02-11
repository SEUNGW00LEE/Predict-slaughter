library(readxl)
library(tidyr)
library(tidyverse)
library(reshape2)
getwd()
setwd("Desktop/Predict-slaughter/")
rm(list=ls())

breeding <- read_xlsx("Dataset/14_22사육.xlsx", sheet = 2) %>%
  separate(구분...1, sep = "년", into = c("year", "month")) %>% 
  filter(구분...3 != '소계') %>%
  pivot_longer(cols= 5:43, values_to = "breeding_count", names_to = "age")  %>%
  rename("kind" = "구분...2", "gender" = "구분...3") %>% 
  mutate(
    month = parse_number(month),
    type = "breeding",
    age = parse_number(age)
    )

slaughter <- read_xlsx("Dataset/14_22도축.xlsx", sheet = 2) %>% 
  separate(구분...1, sep = "년", into = c("year", "month")) %>% 
  filter(구분...3 != '소계') %>% 
  pivot_longer(cols= 5:26, values_to = "slaughter_count", names_to = "age") %>%
  mutate(
    month = parse_number(month),
    age = parse_number(age),
    type = "slaughter",
    age = ifelse(age == 20, 200, age)
  ) %>% 
  rename("kind" = "구분...2", "gender" = "구분...3")
  


breeding_under20 <- breeding %>% 
  mutate(
    age = ifelse(age <=20, 200, age)
    ) %>%
  group_by(year, month, kind, gender, age, type) %>% 
  summarise(breeding_count = sum(breeding_count, na.rm=TRUE)) %>% 
  filter(age == 200)


breeding <- merge(breeding, breeding_under20, all=TRUE)

total <- merge(breeding, slaughter, by=c('year'='year', 'month'='month', 'kind' = 'kind', 'gender'='gender', 'age'='age'), all=TRUE) %>% 
  mutate(
    rate = (slaughter_count/(breeding_count+slaughter_count))
  ) %>% 
  select(-c(type.x,type.y))

test <- total %>% 
  filter(year == 2022) 

train <- total %>% 
  filter(year != 2022) 


View(sum_train)


rate <- train %>% 
  group_by(kind, gender, age) %>% 
  summarise(mean_rate = mean(rate, na.rm=TRUE))



train %>% 
  filter(kind=="한우" & gender=="암" & year == 2021 & month == 12) %>% 
  merge(rate) %>% 
  mutate(
    predict_breeding = ifelse(age<=36, lag(breeding_count,n=1, order_by = age), breeding_count),
    predict_slaughter = predict_breeding * mean_rate
  ) %>% 
  select(-c(breeding_count, slaughter_count, rate, mean_rate, year, month)) -> predictJan


test %>% 
  filter(kind == "한우" & gender == "암" & year == 2022 & month == 1) %>%
  select(-c(rate, year, month))  %>% 
  merge(predictJan) %>% 
  mutate(
    score = (slaughter_count / predict_slaughter) * 100
  ) -> result_predictJan
  
scoreJan <-
  (sum(result_predictJan$predict_slaughter, na.rm=TRUE) / sum(result_predictJan$slaughter_count, na.rm = TRUE)) * 100
scoreJan

predictJan %>% 
  mutate(
         predictFeb = ifelse(age<=36, lag(predict_breeding,n=1, order_by = age), predict_breeding),
         predict_slaughter = ifelse(age<=36, lag(predict_slaughter,n=1, order_by = age), predict_slaughter),
         predictBr_Sl = predictFeb - predict_slaughter
  ) -> predictFeb


predictFeb %>% 
  mutate(
    predictBr_Sl = ifelse(is.na(predictBr_Sl)==TRUE, predictFeb, predictBr_Sl)
    ) %>% 
  merge(rate) %>% 
  select(-c(predict_breeding, predict_slaughter)) %>% 
  mutate(
    predict_slaughter = predictBr_Sl * mean_rate
  ) -> predictFeb
  
test %>% 
  filter(kind == "한우" & gender == "암" & year == 2022 & month == 2) %>%
  select(-c(rate, year, month))  %>% 
  merge(predictFeb) %>% 
  mutate(
    score = (predict_slaughter/slaughter_count) * 100
  ) -> result_predictFeb

scoreFeb <-
  (sum(result_predictFeb$predict_slaughter, na.rm=TRUE) / sum(result_predictFeb$slaughter_count, na.rm = TRUE)) * 100

scoreFeb

sum_train <- train %>% 
  group_by(year, month, kind, gender) %>% 
  summarise(breeding_count =sum(breeding_count, na.rm = TRUE),
            slaughter_count = sum(slaughter_count, na.rm = TRUE))%>% 
  mutate(
    rate = slaughter_count / breeding_count
  )
View(sum_train)

library(ggplot2)

sum2021 <- sum_train %>% 
  filter(year==2021) 
View(sum2021)
sum2021$kind_gender <- paste(sum2021$kind, sum2021$gender, sep="-")

sum2021$year_month <- paste(sum2021$year, sum2021$month, sep="_")

sum2021 <- sum2021 %>% 
  filter(rate != Inf)
View(sum2021)

ggplot(diamonds, aes(x = carat)) +  geom_histogram()


ggplot(sum2021, aes(x=month)) +
  geom_line(alpha = 0.5, aes(y=rate, color = kind_gender))+ 
  theme_minimal(base_family = "AppleSDGothicNeo-SemiBold") 
  
sum2021 %>% 
  group_by(year,month) %>% 
  summarise(slaughter_count = sum(slaughter_count, na.rm = TRUE)) -> total_sum2021
View(total_sum2021)  
ggplot(total_sum2021, aes(x=month)) +
  geom_histogram(alpha = 0.5, aes(y=slaughter_count), stat='identity', fill='skyblue')
  



p <- ggplot() + 
  geom_bar(data=total_sum2021, alpha = 0.3, aes(x=month, y=slaughter_count), stat='identity', fill='skyblue', width = 0.5) +
  geom_line(data=sum2021,aes(x=month,y=rate*600000,color = kind_gender),alpha = 1) +
  scale_y_continuous(name = "도축마리",
                     sec.axis = sec_axis(~./600000 , name="도축율")) +
  scale_x_continuous(name = "기간", breaks = seq(1, 12, 1), labels = paste0(seq(1, 12, 1), '월')) + 
  theme_minimal(base_family = "AppleSDGothicNeo-SemiBold") +
  theme(
    plot.title = element_text(hjust = 0.5,size=18, color = "royalblue4", face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "grey"),
    panel.grid.minor.y = element_line(size = 0.1, color = "grey")
    )+
  labs(
    title = "도축마리 및 도축율",
    color = "종류"
  ) 
  
p
