

###################### 10주차 수업 ##########################
##                    연관규칙분석                         ##
#############################################################


## 1. 데이터 불러오기 
## 환자 데이터 
setwd("D:/[2022-2] 패턴탐색/")
dat <- read.csv("M20.csv", header = T, fileEncoding = "CP949", 
                stringsAsFactors = F)   
str(dat)

table(dat$주상병코드)
dat$주상병코드 <- ifelse(nchar(dat$주상병코드) > 3, 
                         paste0(substr(dat$주상병코드, 1, 3), 
                               ".", substr(dat$주상병코드, 4, 6)),
                         dat$주상병코드)
dat$주상병코드 <- substr(dat$주상병코드, 1, 5)


## 주상병 코드 데이터
code <- read.csv("KCD.csv", stringsAsFactors = F, fileEncoding = 'CP949')
code <- code[code$분류.기준=="세", c(3,6)]

code <- rbind(code, 
              data.frame(질병분류.코드=paste0(LETTERS, "_"),
                         한글명칭=paste0(LETTERS, "민감상병")))  

dat2 <- merge(x = dat, y = code, 
              by.x = "주상병코드", by.y = "질병분류.코드")
sum(is.na(dat2$한글명칭))


## 순차연관규칙을 위한 열 이름 설정 
dat2 <- dat2[, -1]
names(dat2) <- c("sequenceID", "eventID", "item")



## 2. 트랜잭션 데이터로 변환하기
library(arules)  # 규칙 생성 
library(arulesViz)  # 규칙 시각화 
library(RColorBrewer)


## 모든 사건이 동일하게 동시에 일어났다는 것(순서, 흐름을 보이기 힘듦)
dat2_spl <- split(dat2$item, dat2$sequenceID)  
dat2_trans <- as(dat2_spl, "transactions")  
str(dat2_trans)
inspect(dat2_trans)
summary(dat2_trans)

length(unique(dat2$sequenceID)) ; length(unique(dat2$item))
sort(itemFrequency(dat2_trans, type = "absolute"),  
     decreasing = T)[1:5]
itemFrequencyPlot(dat2_trans, topN=5, type="absolute", 
                  xlab = "주상병", ylab = "빈도", col = c("red", rep("grey", 4)))
summary(sort(itemFrequency(dat2_trans)))  


df <- data.frame(tapply(dat2$item, 
                        dat2$sequenceID, 
                        function(x) length(unique(x))))
table(df)



## 3. 규칙 생성 
#apriori(dat2_trans, control = list(verbose=F))
rules1 <- apriori(dat2_trans, 
                  parameter = list(supp=0.2, conf=0.5, minlen=2),
                  control = list(verbose=F))
summary(rules1)


inspect(rules1)
inspect(sort(rules1, by="lift")[1:5])



## 4. 규칙 시각화 
plot(rules1)   
#display.brewer.all()
plot(sort(rules1, by="lift")[1:5],
     control = list(col= rev(brewer.pal(8,"Set2")))) 
        

plot(rules1, method = "grouped") 
plot(sort(rules1, by="lift")[1:5], method = "grouped")  
plot(sort(rules1, by="lift")[1:5], method = "grouped", measure="confidence")   

plot(rules1, method = "paracoord")
plot(sort(rules1, by="lift")[1:5], method = "paracoord")      
plot(sort(rules1, by="lift")[1:5], method = "paracoord", measure="confidence")  

plot(rules1, method = "graph")
plot(sort(rules1, by="lift")[1:5], method = "graph")   
plot(sort(rules1, by="lift")[1:5], method = "graph", measure="confidence")

plot(rules1, method = "graph", engine = "htmlwidget")
plot(sort(rules1, by="lift")[1:5], method = "graph", engine = "htmlwidget")



rules1.1 <- subset(rules1, lhs %in% c("상세불명의 알레르기비염", "상세불명의 급성 기관지염"))
inspect(rules1.1)

rules1.2 <- subset(rules1, rhs %in% c("상세불명의 알레르기비염", "상세불명의 급성 기관지염"))
inspect(rules1.2)






