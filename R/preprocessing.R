library(readxl)
library(tidyr)
library(tidyverse)
library(reshape2)
library(ggplot2)

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


scoreJan <- (sum(result_predictJan$predict_slaughter, na.rm=TRUE) / sum(result_predictJan$slaughter_count, na.rm = TRUE)) * 100


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

predictFeb %>% 
  mutate(
    predictMar = ifelse(age<=36, lag(predict_breeding,n=1,order_by=age),predict_breeding),
    predict_slaughter = ifelse(age<=36, lag(predict_slaughter,n=1, order_by = age), predict_slaughter),
    predictBr_Sl = predictMar - predict_slaughter
  ) -> predictMar

predictMar %>% 
  mutate(
    predictBr_Sl = ifelse(is.na(predictBr_Sl)==TRUE, predictMar, predictBr_Sl)
  ) %>% 
  merge(rate) %>% 
  mutate(
    Marpredict_slaughter = predictBr_Sl * mean_rate
  ) -> predictMar

test %>% 
  filter(kind == "한우" & gender == "암" & year == 2022 & month == 2) %>%
  select(-c(rate, year, month))  %>% 
  merge(predictMar) %>% 
  mutate(
    score = (predict_slaughter/slaughter_count) * 100
  ) -> result_predictMar

scoreFeb <-
  (sum(result_predictFeb$predict_slaughter, na.rm=TRUE) / sum(result_predictFeb$slaughter_count, na.rm = TRUE)) * 100

scoreMar <-
  (sum(result_predictMar$predict_slaughter, na.rm=TRUE) / sum(result_predictMar$slaughter_count, na.rm = TRUE)) * 100

score <- data.frame(Month = c("Jan","Feb","Mar"),
                    Score = c(scoreJan,scoreFeb,scoreMar))
score$Score <- abs(100-score$Score)

JanFebMarPlot<- ggplot(score, aes(x=Month, y=Score, fill=Month), alpha=0.1) +
  geom_col(width=0.7)+
  geom_text(aes(x=Month, y=Score, label= round(Score)),
            vjust=3, colour="white", size=5) +
  scale_x_discrete(limits=c("Jan", "Feb", "Mar")) + 
  theme_minimal(base_family = "AppleSDGothicNeo-SemiBold") +
  ggtitle("예측 도축량과 실제 도축량의 오차율") +
  theme(
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(hjust = 0.5,size=18, color = 'grey3', face="bold")
    )




sum_train <- train %>% 
  group_by(year, month, kind, gender) %>% 
  summarise(breeding_count =sum(breeding_count, na.rm = TRUE),
            slaughter_count = sum(slaughter_count, na.rm = TRUE))%>% 
  mutate(
    rate = slaughter_count / breeding_count
  )

sum2021 <- sum_train %>% 
  filter(year==2021) 

sum2021$kind_gender <- paste(sum2021$kind, sum2021$gender, sep="-")

sum2021$year_month <- paste(sum2021$year, sum2021$month, sep="_")

sum2021 <- sum2021 %>% 
  filter(rate != Inf)


sum2021 %>% 
  group_by(year,month) %>% 
  summarise(slaughter_count = sum(slaughter_count, na.rm = TRUE)) -> total_sum2021
   
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
    color = "종별 도축울"
  )
p
ggsave(p, file="Visualization/도축마리및도축율.png")


#각 연도,월 - 종별 도축두수

train %>% 
  group_by(year, month, kind) %>% 
  summarise(total_slaughter = sum(slaughter_count, na.rm = TRUE)) -> year_month_sum

train %>% 
  group_by(year, month, kind) %>% 
  summarise(total_slaughter = sum(slaughter_count, na.rm = TRUE)) -> year_month_sum

year_month_sum %>% 
  group_by(year, month) %>% 
  summarise(month_slaughter = sum(total_slaughter)) %>% 
  mutate(
    slaughter_rate = round((month_slaughter/sum(month_slaughter))*100,1),
    str_slaughter_rate = paste0(" ",slaughter_rate,"%")
  ) -> total_rate

# 월별 도축두수 animation + 월별 rate

View(total_rate)
library(gganimate)

ani <- ggplot(data = year_month_sum, aes(x=month)) +
  geom_bar(alpha=0.5, aes(y=total_slaughter,fill=kind),position='stack',stat='identity') +
  theme_minimal(base_family = "AppleSDGothicNeo-SemiBold") +
  scale_x_continuous(name = "기간", breaks = seq(1, 12, 1), labels = paste0(seq(1, 12, 1), '월')) +
  scale_y_continuous(breaks=seq(0,120000,15000), labels = scales::comma)+
  theme(
    plot.title = element_text(hjust = 0.5,size=18, color = "royalblue4", face="bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "grey"),
    panel.grid.minor.y = element_line(size = 0.1, color = "grey")
  ) +
  transition_states(year,
                    transition_length=40, #총 시간
                    state_length=10)+
  geom_text(data = total_rate,
            aes(x=month,y=month_slaughter,label=str_slaughter_rate),
            vjust=-1, hjust=0.5)+
  labs(
    title = '{closest_state}년 월별 도축두수',
    subtitle = '2014년-2021년',
    caption = '축산물이력제 데이터랩',
    y= "도축두수",
    fill = "종"
  )+
ease_aes('quartic-in-out') +
  enter_fade()
ani

ani <-animate(plot=ani, nframes=400, end_pause = 20, width=1080, height=720)  


