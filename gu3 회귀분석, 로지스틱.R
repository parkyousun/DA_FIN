gu3 <- read.csv("gu_data_3.csv")

mean_price1 <- mean(gu3$평당가격, trim = 0.3)
gu3$price <- ifelse(gu3$평당가격 >= mean_price1 ,"이상","미만")
gu3$price <- revalue(gu3$price, replace=c("이상"="0", "미만"="1"))

str(gu3)
# 더미변수로 만들기
gu3$subway.dummy <-ifelse(gu3$`해당.지역.지하철.지점수`>mean(gu3$해당.지역.지하철.지점수), 1, 0)
gu3$park.dummy <-  ifelse(gu3$`국립공원수`>mean(gu3$국립공원수), 1, 0)
gu3$crime.dummy <-ifelse(gu3$범죄발생수>mean(gu3$범죄발생수), 1, 0)
gu3$nightlife.dummy <-ifelse(gu3$유흥업소수>mean(gu3$유흥업소수), 1, 0)
gu3$education.dummy <-ifelse(gu3$교육기관수>mean(gu3$교육기관수), 1, 0)
gu3$health.dummy <-ifelse(gu3$보건기관수>mean(gu3$보건기관수), 1, 0)
gu3$book.dummy <- ifelse(gu3$보유도서합계>mean(gu3$보유도서합계), 1, 0)
gu3$library.dummy <- ifelse(gu3$도서관.방문자수>mean(gu3$도서관.방문자수), 1, 0)
gu3$bank.dummy <- ifelse(gu3$은행합계>mean(gu3$은행합계), 1, 0)
gu3$passport.dummy <- ifelse(gu3$여권발급합계>mean(gu3$여권발급합계), 1, 0)
gu3$fine.dust.dummy <- ifelse(gu3$미세먼지평균>mean(gu3$미세먼지평균),1,0)
gu3$car.dummy <- ifelse(gu3$합계_자가용>mean(gu3$합계_자가용),1,0)

str(gu3)
library(plyr)
gu3$price <- as.factor(gu3$price)
gu3$subway.dummy <- as.factor(gu3$subway.dummy)
gu3$park.dummy <- as.factor(gu3$park.dummy)
gu3$crime.dummy <- as.factor(gu3$crime.dummy)
gu3$nightlife.dummy <- as.factor(gu3$nightlife.dummy)
gu3$education.dummy <- as.factor(gu3$education.dummy)
gu3$health.dummy <- as.factor(gu3$health.dummy)
gu3$book.dummy <- as.factor(gu3$book.dummy)
gu3$bank.dummy <- as.factor(gu3$bank.dummy)
gu3$library.dummy <- as.factor(gu3$library.dummy)
gu3$passport.dummy <- as.factor(gu3$passport.dummy)
gu3$fine.dust.dummy <- as.factor(gu3$fine.dst.dummy)
gu3$car.dummy <- as.factor(gu3$car.dummy)

# 다중회귀분석
regression1 <- lm(`평당가격` ~`해당.지역.지하철.지점수`+국립공원수+범죄발생수+유흥업소수+교육기관수+보건기관수+합계_자가용+ 보유도서합계+도서관.방문자수+여권발급합계+은행합계+미세먼지평균, data= gu3)
summary(regression1)

str(gu3)
logreg0 <- glm(price ~`해당.지역.지하철.지점수`+국립공원수+범죄발생수+유흥업소수+교육기관수+보건기관수+합계_자가용+ 보유도서합계+도서관.방문자수+여권발급합계+은행합계+미세먼지평균,family =  binomial, data= gu3)
summary(logreg0)

install.packages("Epi")
library(Epi)
graph1 <- ROC(form =price ~ `해당.지역.지하철.지점수`+`국립공원수`+`교육기관수`+`범죄발생수`+`보건기관수`+합계_자가용+ 보유도서합계+도서관.방문자수+여권발급합계+은행합계+미세먼지평균,data= gu3,plot = "ROC")
graph1$AUC # 0.5 ~ 1.0: The bigger, the better,


