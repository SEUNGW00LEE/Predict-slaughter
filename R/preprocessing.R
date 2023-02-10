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
View(breeding)
View(breeding_under20)
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

rate <- train %>% 
  group_by(kind, gender, age) %>% 
  summarise(mean_rate = mean(rate, na.rm=TRUE))
View(rate)


train %>% 
  filter(kind=="한우" & gender=="암" & year == 2021 & month == 12) %>% 
  merge(rate) %>% 
  mutate(
    predict_breeding = ifelse(age<=37, lag(breeding_count,n=1, order_by = age), breeding_count)
  ) %>% 
  View()
View(predictJan)  
predictJan$predict_breeding_count <- lag(predictJan$breeding_count)
predictJan







