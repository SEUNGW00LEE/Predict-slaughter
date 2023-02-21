Predict slaughter
=============

## Predicts the number of Cow slaughtered.


#### [축산물이력제 데이터랩](https://datalab.mtrace.go.kr/)에서 '소의 종류별/성별/개월령별 사육두수'와 '소의 종류별/성별/개월령별 도축두수'를 통해 주어지지않은 도축량을 예측합니다.

---

  - 사육두수는 3개월령부터 집계가 되지만 도축두수는 20개월령 이하로 집계가 되는 불일치가 있습니다. 사육두수의 dataframe에  `age = 200`에 20개월령 이하 사육두수를 모두 합친 row를 생성합니다.


  - 2022년 data를 예측하는 모델을 만들기위해  `year == 2022 `인 dataframe을  `test`로  `year != 2022 `인 dataframe을 `train`로 생성합니다.
```R
test <- total %>% 
  filter(year == 2022) 

train <- total %>% 
  filter(year != 2022) 
```


  - train의 각 종별, 월령별 평균 도축율을 계산합니다.
```R
rate <- train %>% 
  group_by(kind, gender, age) %>% 
  summarise(mean_rate = mean(rate, na.rm=TRUE))
```


  - 2021년 12월 사육두수를 토대로 `lag`를 통해 2022년 1월 사육두수를 산출했고, 이 사육두수에 위에서 구한 평균 도축율을 곱하여 도축수를 예측합니다.

|kind |gender | age| predict_breeding| predict_slaughter|
|:----|:------|---:|----------------:|-----------------:|
|한우 |암     | 200|           762343|         153.65943|
|한우 |암     |  21|            56497|          93.01075|
|한우 |암     |  22|            63676|         160.14666|
|한우 |암     |  23|            57578|         204.03919|
|한우 |암     |  24|            35214|         185.06054|
|한우 |암     |  25|            26808|         193.18978|
|한우 |암     |  26|            23291|         225.25030|
|한우 |암     |  27|            20973|         270.36351|
|한우 |암     |  28|            23426|         374.61788|
|한우 |암     |  29|            25098|         483.22390|
|한우 |암     |  30|            30817|         682.22363|
|한우 |암     |  31|            33257|         790.31395|
|한우 |암     |  32|            38438|         923.91667|
|한우 |암     |  33|            48120|        1130.16815|
|한우 |암     |  34|            52469|        1180.87373|
|한우 |암     |  35|            46607|        1035.86852|
|한우 |암     |  36|            25643|         584.40336|
|한우 |암     |  37|           285301|        7690.82286|   


  - 예측한 2022년 1월 데이터의 예측 사육두수 - 예측 도축두수를 통해 2022년 2월 사육두수를 예측합니다. 이 예측 사육두수에서 다시 한번 평균 도축율을 곱하여 2022년 2월 예측 도축두수를 산출합니다.   
```R
predictJan %>% 
  mutate(
         predictFeb = ifelse(age<=36, lag(predict_breeding,n=1, order_by = age), predict_breeding),
         predict_slaughter = ifelse(age<=36, lag(predict_slaughter,n=1, order_by = age), predict_slaughter),
         predictBr_Sl = predictFeb - predict_slaughter
  ) -> predictFeb
``` 
- 동일한 방식으로 2022년 3월도 예측합니다.

  
- 예측한 2022년 1월 도축두수와 2022년 2월,3월 도축두수의 실제 데이터와 비교해 `score = |1 - 예측 도축두수 / 실제 도축두수| * 100`을 구합니다. 이를 그래프로 표현합니다.   

![image](https://user-images.githubusercontent.com/86904141/218403481-1f1e1cf1-d748-4075-8d3d-61ed284807c1.png)

> 1월은 오차가 20%이내로 비교적 안정적이였지만, 2월,3월은 80프로로 매우 커져 예측이 진행되지않았다.   

**예측에 사용한 도축율 자체가 오차범위가 굉장히 큰 문제를 가지고 있고, 1월에 적용된 오차가 2월,3에 다시 한번 적용되며 오차율이 매우 커진 것으로 보인다.**   

**예측에 평균 도축율 이외에도 아니라 월별 도축량, 전기 대비 도축량 등 다양한 feature을 도입해 예측해야 더 정확한 결과가 나올 것이다.**

- 2021년 월별 도축율과 종별 도축량   

![도축마리및도축율](https://user-images.githubusercontent.com/86904141/218397183-3684f3db-592a-4e0f-b6c8-fcbfccda4814.png)

-----
### develop 사항

- **도축두수가 월별로 도축이 몇 % 차지하는지 확인**


- **종별, 개월별로 증감을 확인하여 예측**


- **이 후, 추가적인 feature을 통해 machine learning**

------

### gganimate를 사용한 월별, 종별 도축두수 animation

![월별도축그래프](https://user-images.githubusercontent.com/86904141/218952979-dcf53ecb-d679-4511-8f0b-ddd325f08787.gif)



-----

### Feedback

- **자기상관**

**시계열데이터 생성**

```R
ts(data=predict_slaughter, start=c(2014,1),frequency = 12)  -> predict_slaughter
```

![image](https://user-images.githubusercontent.com/86904141/219692078-10db519d-33b2-4446-bd20-1f5e7f451c88.png)

```R
predict_slaughter.decompose <- decompose(predict_slaughter)
plot(predict_slaughter.decompose)
````

**계절요인(seasonal)을 삭제한 그래프**
```R
plot(predict_slaughter - predict_slaughter.decompose$seasonal)
```
![image](https://user-images.githubusercontent.com/86904141/219694098-6d22f30b-23e9-4611-98b7-00956352ec9d.png)


**forecast() 를 사용하여 예측 그래프 생성**

```R
auto.arima(predict_slaughter)

predict_slaughter.arima <- arima(predict_slaughter, order=c(2,1,2))
predict_slaughter.arima

predict_slaughter.forecast <- forecast(predict_slaughter.arima, h=10)
plot(predict_slaughter.forecast)
```
![image](https://user-images.githubusercontent.com/86904141/219696129-6b31c30d-e2ae-46f3-8e9d-e6ebb832bbbf.png)
![image](https://user-images.githubusercontent.com/86904141/219696737-0a528d04-6edf-4f90-8ae7-e3002f8808b6.png)


#### diff = 1, 2, 3, log 에 따른 차이 

![image](https://user-images.githubusercontent.com/86904141/220284987-f1a5b9f5-8b45-4ede-87cc-dd1bafe07d87.png)

**diff = 2로 결정**

**diff=2의 decompose**

![image](https://user-images.githubusercontent.com/86904141/220288571-c4ef496f-3ed9-4f42-bd20-e935d36e04c4.png)

**diff=2의 seasonal 제거 그래프**
![image](https://user-images.githubusercontent.com/86904141/220291856-97b42f0c-3ae5-4da5-b74f-11a2e7d2dc4d.png)

**diff=2,seasonal 제거의 acf **
![image](https://user-images.githubusercontent.com/86904141/220296876-4eef6dc3-3618-42bb-9250-ec42fbef2bcb.png)

**diff=2, seasonal 제거의 arima**

```R
ARIMA(1,0,2)(1,0,0)[12] with zero mean 

Coefficients:
          ar1      ma1     ma2     sar1
      -0.2666  -1.8027  0.8208  -0.4195
s.e.   0.1160   0.0728  0.0709   0.1064

sigma^2 = 68219107:  log likelihood = -983.95
AIC=1977.89   AICc=1978.57   BIC=1990.61
```
**forecast를 통해 6개월 예측**

```R
fore <- forecast(model)
fore2 <- forecast(model, h = 6)
plot(fore)
plot(fore2)
```
![image](https://user-images.githubusercontent.com/86904141/220309421-f0d6d254-84d8-4023-9902-db5b6455a1b2.png)


- **명절있는 달을 색깔을 바꿔 차이점을 명확히 파악할 수 있도록 변경**
