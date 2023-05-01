

###################### 10주차 수업 ##########################
##                  순차연관규칙분석                       ##
#############################################################


## 1. 데이터 불러오기 
setwd("C:/Users/hallym/Desktop/[2022-2] 패턴탐색/10주차/")
dat <- read.csv("M20_merge.csv", header = T, fileEncoding = "CP949", 
                stringsAsFactors = F)  
head(dat)



## 2. 순차연관규칙의 데이터 형태에 맞게 전처리하기 
dat2 <- dat
dat2$size <- 1
dat2 <- dat2[, c(1,2,4,3)]

dat2 <- dat2[order(dat2$sequenceID, dat2$eventID), ]   # sequenceID > eventID 순으로 정렬

dup <- duplicated(dat2[, c(1,2)], fromLast = T)    # (default = F) 만약에 3개가 겹치면 첫번째가 F, 두세번째가 T
which(dup)
dat2[17718:17720, ]
dat2 <- dat2[-which(dup), ]
sum(duplicated(dat2[, c(1,2)]))


#write.table(dat2, "./seq_dat.txt", row.names = F, sep = "\t", col.names = F)



## 3. 저장한 데이터 불러오기
## 연관규칙과 다르게, 순차연관규칙은 데이터를 불러옴과 동시에 트랜잭션 데이터로 변환해줌
library(arulesSequences)
dat3 <- read_baskets(con = "./seq_dat.txt", sep = "\t",
                     info = c("sequenceID", "eventID", "SIZE"))  # item열을 제외한 열의 이름 
str(dat3)
head(as(dat3, "data.frame"))

## 어떤 sequence의 규칙이 있는지를 살펴봄
seque <- cspade(dat3,
                parameter = list(support = 0.4),   # cspade()는 최소지지도만 설정
                control = list(verbose = F))

seque
inspect(sort(seque, by = "support"))    # 상세불명의 급성 기관지염이 가장 많이 관측되었기 때문에 많이 보임
# 기관지염이 1,2,3,5일 중에 진단 받으면 1,2,3일을 제거해서 보던지 전처리 방식을 달리하면 됨
 

## 4. 규칙 생성 
## 위의 seque를 넣어주게 되면 규칙을 생성할 수 있음
## apriori와 같은 함수
rule <- ruleInduction(seque, confidence = 0.5)
summary(rule)
inspect(rule)   # 연관규칙과 시차를 고려한 차이점 : lhs 먼저 일어났고 rhs가 먼저 일어남



##5. 규칙 시각화 : 네트워크 플랏(matrix로 바꿔서 바꿔야함 )

