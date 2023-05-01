

######################## 5주차 #########################
###                   주성분분석                     ###
########################################################

## 1. 데이터 불러오기
setwd("C:/Users/hallym/Desktop/[2022-2] 패턴탐색/5주차/")
pima <- read.csv("pima.csv", header = T)

pima$diabetes <- factor(pima$diabetes, levels = c(0, 1),
                        labels = c("당뇨에 걸리지 않음", "당뇨"))


## 2. 데이터 표준화 
str(pima)
pima[, -9] <- scale(pima[, -9])

apply(pima[, -9], 2, mean) ; apply(pima[, -9], 2, sd)

cor_mat <- cor(pima[, -9])
#install.packages("corrplot")
library(corrplot)
corrplot(cor_mat, method = "color", diag = FALSE, type ="upper",
         addCoef.col = "black", number.digits = 2)



## 3. 주성분 분석 실시 
## 1) Bartlett's test of sphericity(구형성 검정) : 주성분분석에 적합한 데이터인가?
library(psych)
cortest.bartlett(R = cor_mat, n = nrow(pima))   # 자유도 : p(p-1)/2

## 2) KMO 테스트 : 다른 변수의 영향을 제거하고 계산하는 "편상관계수" 이용
KMO(cor_mat)


pca_res <- princomp(pima[, -9], cor = F)  
summary(pca_res)  


## 이론을 이해하기 위해 2차원으로 생각해보자 !
## 1 - 공분산행렬 구하기
pima_ex <- pima[c("pregnant", "age")]
cov(pima_ex)   

pca_res2 <- princomp(pima_ex, cor = F)
summary(pca_res2)


## 2 - 고유값과 그 의미 생각하기 
pca_res2$sdev^2    
1.54/2 
0.46/2


## 3 - 고유벡터 구하기
pca_res2$loadings    

## 4 - 새로운 축 PCA1과 PCA2를 기준으로 새로운 좌표 구하기
## PC1의 좌표: 고유벡터 X_1 * 원래변수1 + 고유벡터 X_2 * 원래변수2
head(pca_res2$scores, n = 1)
(pca_res2$loadings[1] * pima_ex$pregnant[1]) + (pca_res2$loadings[2] * pima_ex$age[1])
(pca_res2$loadings[3] * pima_ex$pregnant[1]) + (pca_res2$loadings[4] * pima_ex$age[1])
plot(pca_res2$scores)


## 5 - 시각화 결과 : 기존의 x축과 y축 기준 데이터 vs 새로운 축 PCA1과 PCA2 기준 데이터 
plot(pima_ex$pregnant, pima_ex$age, xlim = c(-4,4), ylim = c(-4,4))  # 분산과 공분산
abline(h = 0, col = "darkgray", lty = 2)
abline(v = 0, col = "darkgray", lty = 2)
abline(a = pca_res2$loadings[1], b = pca_res2$loadings[2], col = "red", lwd = 1.5) 
abline(a = pca_res2$loadings[3], b = pca_res2$loadings[4], col = "darkgreen", lwd = 1.5)


## 6 - 주성분끼리는 직교하므로 상관계수 0!
cor(pca_res2$scores[, 1], pca_res2$scores[, 2])


##7 - PC1과 두 변수의 상관관계 구해보기, 그리고 제곱하면?
## PC1이 pregnant의 분산 중 얼마나 설명하는가? 가져갔는가?
cor(pca_res2$scores[, 1], pima_ex$pregnant)
cor(pca_res2$scores[, 1], pima_ex$age)
cor(pca_res2$scores[, 1], pima_ex$pregnant)^2 + cor(pca_res2$scores[, 1], pima_ex$age)^2

cor(pca_res2$scores[, 2], pima_ex$pregnant)
cor(pca_res2$scores[, 2], pima_ex$age)


## 4. 주성분 분석 결과 
#pca_res <- prcomp(pima[, -9], scale = F)
summary(pca_res)
screeplot(pca_res, col = "red", type = "lines", 
          npcs = length(pca_res$sdev))

library(ggplot2)
library(ggfortify)
autoplot(pca_res, data = pima, col = 'diabetes')


#plot(pca_res$scores)
#text(pca_res$scores, cex = 0.5)
biplot(pca_res, choices = c(1,2))   


pca_res$loadings # 변수들의 가중치(주성분 계수)
pca_1_g <- fviz_cos2(pca_res, choice = "var", axes = 1, top = 5)
pca_2_g <- fviz_cos2(pca_res, choice = "var", axes = 2, top = 5)

cowplot::plot_grid(pca_1_g, pca_2_g, ncol = 2)

#library(factoextra)
#fviz_pca_var(pca_res, col.var = "contrib", 
#             gradient.cols = c("blue", "red"), repel = TRUE)




##################### 5주차 - 2 ########################
###               요인분석 맛보기                    ###
########################################################


## 1. 주성분분석의 공분산행렬의 분산을 회귀분석의 설명계수로 대체하여 요인 추출
pima_ex <- pima[c("pregnant", "age")]
apply(pima_ex, 2, sd)
cov(pima_ex)

fit1 <- lm(pima_ex$pregnant ~ pima_ex$age)
fit2 <- lm(pima_ex$age ~ pima_ex$pregnant)

fit1_res <- summary(fit1)
summary(fit2)


## 2. 업데이트된 공분산행렬의 고유값과 고유벡터 구하기
step1_mat <- matrix(c(fit1_res$r.squared, cov(pima_ex)[2], cov(pima_ex)[2], fit1_res$r.squared), 
                    ncol = 2, byrow = FALSE)

eigen(step1_mat)   



