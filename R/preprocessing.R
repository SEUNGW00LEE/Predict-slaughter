library(readxl)
library(tidyr)
library(tidyverse)
library(reshape2)
setwd("Desktop/Predict-slaughter/")

breeding <- read_xlsx("Dataset/14_22사육.xlsx", sheet = 2) %>% 
  separate(구분...1, sep = "년", into = c("year", "month")) %>% 
  mutate(
    month = parse_number(month)
  ) %>% 
  filter(구분...3 != '소계') %>%
  pivot_longer(cols= 5:43, values_to = "breeding_count", names_to = "age")  %>% 
  rename("kind" = "구분...2", "gender" = "구분...3") %>% 
  mutate(type = "breeding")

slaughter <- read_xlsx("Dataset/14_22도축.xlsx", sheet = 2) %>% 
  separate(구분...1, sep = "년", into = c("year", "month")) %>% 
  mutate(
    month = parse_number(month)
  ) %>% 
  filter(구분...3 != '소계') %>%
  pivot_longer(cols= 5:26, values_to = "slaughter_count", names_to = "age") %>% 
  rename("kind" = "구분...2", "gender" = "구분...3")  %>% 
  mutate(type = "slaughter")

df <- left_join(breeding, slaughter, by=c('year'='year', 'month'='month', 'kind' = 'kind', 'gender'='gender', 'age'='age')) %>% 
  mutate(
    rate = (slaughter_count/(breeding_count+slaughter_count))*100
  ) %>% 
  select(-c(type.x,type.y))
View(df)
total <- rbind(breeding, slaughter) 
View(total)

hanwoo_cow <- total %>% 
  filter(kind == "한우") %>% 
  filter(gender == "암")


