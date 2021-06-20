rn_apt <- read.csv("rn_apt_factor.csv")
rn_apt <- rn_apt[,-1]

colnames(rn_apt)[c(3,4,5,6,17)] <- c("건축 연도"," 최근 매매 연도","아파트 이름","최근 매매 월","해당 지역 지하철 지점수")

regression4 <- lm(`평당가격` ~ `건축 연도`+`해당 지역 지하철 지점수`+`국립공원수`+`유흥업소수`+`교육기관수`+`보건기관수`+`범죄발생수`,data = rn_apt)
summary(regression4)

logreg1 <- glm(`평당가격` ~ `해당 지역 지하철 지점수`+`국립공원수`+`유흥업소수`+`교육기관수`+`범죄발생수`+`보건기관수`, family=binomial, data= rn_apt)
summary(logreg1)


rn_apt$범죄발생수 <- as.factor(as.character(rn_apt$범죄발생수))
rn_apt$`해당 지역 지하철 지점수` <- as.factor(as.character(rn_apt$`해당 지역 지하철 지점수`))
rn_apt$`국립공원수` <- as.factor(as.character(rn_apt$`국립공원수`))
rn_apt$유흥업소수 <- as.factor(as.character(rn_apt$유흥업소수))
rn_apt$교육기관수 <- as.factor(as.character(rn_apt$교육기관수))
rn_apt$보건기관수 <- as.factor(as.character(rn_apt$보건기관수))
str(rn_apt)


rn_apt$subway.dummy1 <-ifelse(rn_apt$`해당 지역 지하철 지점수`==1, 1, 0)
rn_apt$park.dummy1 <-  ifelse(rn_apt$`국립공원수`==1, 1, 0)

regression5 <- lm(`평당가격` ~ `해당 지역 지하철 지점수`+`국립공원수`,data = rn_apt)
summary(regression5)



+`유흥업소수`+`교육기관수`+`보건기관수`+`범죄발생수`