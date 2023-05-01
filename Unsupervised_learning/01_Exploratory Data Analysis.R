
## --------------  패턴탐색 2주차 ---------------- ##
##                "탐색적자료분석"                 ##
## ----------------------------------------------- ##

## 1. 내장데이터 불러오기
## 변수 설명 : mpg(연비), wt(중량), am(변속기)
data("mtcars")
head(mtcars, n=3)
str(mtcars)

cars <- mtcars
rownames(cars) <- NULL
cars <- cars[, c("mpg","wt","am")]
cars$am <-factor(cars$am, levels = 0:1, 
                   labels = c("자동", "수동")) 


## 2. 결측값 확인하기
colSums(is.na(cars))

library(VIM) 
VIM::aggr(cars)

mtcars$hp <- ifelse(mtcars$hp %in% sample(mtcars$hp, 10, replace = F), NA, mtcars$hp)
mtcars$drat <- ifelse(mtcars$drat %in% sample(mtcars$drat, 15, replace = F), NA, mtcars$drat)
aggr(mtcars)

## 3. 종속변수 살펴보기 
## 1) 수치를 통한 자료 요약 
##자료의 중심
#평균
mean(cars$mpg)
mean(cars$mpg, trim = 0.05)

#중앙값 
median(cars$mpg)

#최빈값
table(cars$mpg)
which.max(table(cars$mpg))
table(cars$mpg)[1]

library(prettyR)
Mode(cars$wt)
mode(cars$mpg)

##자료의 상대적 위치
#사분위수 
quantile(cars$mpg)

#백분위수 
quantile(cars$mpg, 0.8)
quantile(cars$mpg, c(0.2, 0.6, 0.9))

##자료의 산포 
#중앙값 절대 편차
mad(cars$mpg)

#사분위수 범위
IQR(cars$mpg)

#분산
var(cars$mpg)

#표준편차
sd(cars$mpg)

##변동계수와 상관계수
cov(cars$mpg, cars$wt)
cor(cars$mpg, cars$wt)

##한번에 보기 
apply(cars[, -3], 2, mean)
sapply(cars[-3], mean)

summary(cars)

library(psych)
psych::describe(cars[, -3])


## 2) 표를 통한 자료 요약
mpg_N <- cut(cars$mpg, breaks = seq(10, 35, 5), right = T,
             labels = c("10 ~ 15", "15 ~ 20", "20 ~ 25", "25 ~ 30", "30 ~ 35"))

table(mpg_N)
prop.table(table(mpg_N))

data.frame(table(mpg_N))
df <- data.frame(table(mpg_N))
df
names(df) <- c("계급구간", "도수")


## 3) 그림을 통한 자료 요약
#히스토그램 
summary(cars$mpg)

hist(cars$mpg)
hist(cars$mpg, breaks = 5) -> hist
hist(cars$mpg, breaks = seq(10, 35, 5))
hist(cars$mpg, breaks = c(10, 15, 20, 25, 30, 35))

hist(cars$mpg, breaks = 10) 
hist(cars$mpg, breaks = 10, ylim = c(0, 10)) 
hist(cars$mpg, breaks = seq(10, 34, 2), ylim = c(0, 10))

hist(cars$mpg, breaks = 10 , ylim = c(0, 10), xlim  = c(10, 35),
     axes = FALSE, 
     main = "자동차 연비의 분포", xlab = "자동차 연비", ylab = "빈도")

axis(side=1, at = seq(10, 34, 2))
axis(side=2, at = seq(0, 10, 2))


hist2 <- data.frame(계급구간 = paste(c(10, 15, 20, 25, 30), "~", c(15, 20, 25, 30, 35)),
                    도수 = hist$counts,
                    상대도수 = hist$counts/sum(hist$counts))

#상자그림
boxplot(cars$mpg)
boxplot(cars$mpg, horizontal = T)

abline(v = quantile(cars$mpg), lty = 3, col = "darkblue")

points(x = mean(cars$mpg), y = 1, pch = 2, col = "red")
abline(v = mean(cars$mpg), lty = 2, col = "red")


#이상치 판별
cars2 <- c(cars$mpg, 70, 45, 2)
outwith = boxplot(cars2)
outwith$out

library(outliers)
outlier(cars2)
outlier(cars2, opposite = TRUE)


## 4. 그룹별 연속형 자료의 요약
#shapiro.test(cars$mpg)   # 대립가설 :정규성 불충족 

t <- table(cars$am)
t_bar <- barplot(t, ylim=c(0,25))
text(x = t_bar, y = t_bar + 20, labels = t)

auto <- cars[cars$am == "자동", "mpg"]
manual <- cars[cars$am == "수동", "mpg"]

mean(auto); mean(manual)
var(auto); var(manual)

aggregate(mpg ~ am, cars, median)
aggregate(mpg ~ am, cars, var)

library(doBy)
summaryBy(mpg ~ am, cars, FUN = c(mean,median,var))



hist(auto, freq = F)
hist(manual, freq = F)
shapiro.test(auto)
shapiro.test(manual)

hist_auto <- hist(auto, plot = FALSE)
hist_manual <- hist(manual,plot = FALSE)
plot(hist_auto, col = adjustcolor("blue", alpha = 0.5), freq = F, 
     main = "변속기에 따른 자동자 연비의 분포", xlab = "자동자 연비", xlim = c(10, 35))
plot(hist_manual, col = adjustcolor("red", alpha = 0.5), freq = F, add = TRUE)


#boxplot(auto)
#boxplot(manual)
boxplot(mpg ~ am, cars)
points(c(mean(auto), mean(manual)), pch = 19, col = "red")




