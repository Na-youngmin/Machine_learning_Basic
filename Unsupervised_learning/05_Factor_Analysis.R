

######################## 6주차 #########################
###                    요인분석                      ###
########################################################


## 1. 데이터 불러오기
getwd()
pima <- read.csv("pima.csv", header = T)
pima$diabetes <- factor(pima$diabetes, levels = c(0, 1), labels = (c("정상", "당뇨")))


## 2. 표준화
str(pima)
pima[, -9] <- scale(pima[, -9])

apply(pima[, -9], 2, mean) ; apply(pima[, -9], 2, sd)


## 3. 주성분분석의 공분산행렬의 분산은 회귀분석의 결정계수로 대체
cor_mat <- cor(pima[, -9])
library(corrplot)

corrplot(cor_mat, method = "color", diag = F, type = "upper", 
         addCoef.col = "black", number.digits = 2)


pima_ex <- pima[c("pregnant", "age")]
apply(pima_ex, 2, sd)
cov(pima_ex)

fit1 <- lm(pima_ex$age ~ pima_ex$pregnant)
fit2 <- lm(pima_ex$pregnant ~ pima_ex$age)

fit1_res <- summary(fit1)
summary(fit2)
  
  
## 4. 업데이트된 공분산행렬의 고유값과 고유벡터 구하기
old_mat <- matrix(c(fit1_res$r.squared, cov(pima_ex)[2], cov(pima_ex)[2], fit1_res$r.squared),
                  ncol = 2, byrow = FALSE)
old_mat
eigen(old_mat)



## 5. 반복 알고리즘 : 공통성의 초기 추정값을 이용해서 기존의 공통성과 재추정한 공통성의 차이
cal_cnew <- function(new_value) {
  sxy <- cov(pima_ex)[2]
  new_mat <- matrix(c(new_value, sxy, sxy, new_value), ncol = 2, byrow = F)
  eigen_val <- eigen(new_mat)$values[1]
  eigen_vec <- eigen(new_mat)$vectors[1]
  print(eigen(new_mat)$values) ; print(eigen(new_mat)$vectors)
  
  c_new <- (eigen_vec * sqrt(eigen_val))^2
  
  return(c_new)
}

c_old <- fit1_res$r.squared
c_new <- c()

iter <- 1
while (iter <= 25) {
  print(iter)
  #print(c_old)
  
  c_old <- cal_cnew(new_value = c_old)
  c_new[iter] <- c_old
  
  iter <- iter + 1 
  c_old <- c_old 
}


fa_res <- data.frame("num_iter" = 1:25,
                     "c_old" = c(fit1_res$r.squared, c_new[-25]),
                     "c_new" = c_new)
fa_res$D <- (fa_res$c_old - fa_res$c_new)^2 * 2  
fa_res$D < 0.00001
plot(x = fa_res$num_iter, y = fa_res$D, type = "b")
abline(h = 0.00001, col = "red")

pca_res <- princomp(pima[, -9], cor = F)
screeplot(pca_res, col = "red", type = "lines", npcs = length(pca_res$sdev))



## 요인분석 실시  (알고리즘) 주축요인추출법, (요인 수 결정) - 주축요인, 주성분분석(제일 많이씀), 최대우도법
## 반복 알고리즘과 공통성(community) 비교
fa(pima_ex, nfactors = 1, fm ="pa", rotate = "varimax")  # pa : 주축요인, ml : 최대우도법


## 1) 요인 수 결정  
library(GPArotation)
library(psych)

pfa_res <- princomp(pima[, -9], cor = F)
summary(pfa_res)
pca_res$sdev^2
screeplot(pfa_res, type = "lines", pch = 19)
abline(h = 1, col = "red")



## 2) 요인 갯수 지정 후 요인회전 적용하여 요인분석 실시
pfa_final <- principal(pima[-9], rotate = "varimax", nfactors = 3)
plot(pfa_final)

plot(pfa_final$loading[, c(1:2)])
text(pfa_final$loadings[, c(1:2)], colnames(pima[-9]))

biplot(pfa_final)
fa.diagram(pfa_final)
pfa_final$loadings


## 다른 방법 : 최대우도법
pfa_final2 <- factanal(pima[, -9], factor = 3, n.obs = nrow(pima),
                       rotation = "varimax", score = "regression")
pfa_final2

print(pfa_final2, cutoff = 0)


