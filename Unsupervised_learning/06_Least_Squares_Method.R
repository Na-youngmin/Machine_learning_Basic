########################### 7주차 동영상 강의 ##################################
##                   최소제곱법을 이용한 회귀계수 추정                        ##
################################################################################


## 1. 데이터 불러오기 
getwd()
setwd("D:/7주차 동영상 강의/")
dat <- read.csv("매출광고데이터.csv", header = T)


## 2. 회귀분석 실시
fit <- lm(Sales ~ TV, data = dat)
fit$coefficients


## 3. 최소제곱법을 통한 회귀계수 추정
## sum((Y_i - hat(Y_i))^2) := RSS(잔차제곱합)
## hat(Y_i) = hat(B0) + hat(B1) * X_i


## hat(B0)과 hat(B1)를 grid 나누어 뽑기 
## hat(B0)
b0_hat <- c()

b0 <- seq(from=-3, to=13, length=879)
I_0 <- round(seq(from=1, to=800, length=10), 0)

for (i in I_0){
  b0_sample <- sample(b0[i:(i+89)], 1)
  b0_hat <- c(b0_hat,b0_sample)
}

b0_hat


## hat(B1)
b1_hat <- c()

b1 <- seq(from=-0.02, to=0.08, length=879)
I_1 <- round(seq(from=1, to=800, length=10), 0)

for(i in I_1){
  b1_sample <- sample(b1[i:(i+89)],1)
  b1_hat <- c(b1_hat,b1_sample)
}

b1_hat


## hat(B0)와 hat(B1)
coef_hat <- expand.grid(b0_hat, b1_hat)
names(coef_hat) <- c("b0_hat", "b1_hat")


## 잔차제곱합 구하기
res_RSS <- c()

for (j in 1:nrow(coef_hat)) {
  res_RS <- c()
  
  for (i in 1:nrow(dat)) {
    hat_Y = coef_hat$b0_hat[j] + coef_hat$b1_hat[j] * dat$TV[i]
    cal_RS <- (dat$Sales[i] - hat_Y)^2
    
    res_RS <- c(res_RS, cal_RS)
    #print(res_RS)
  }
  
    cal_RSS <- sum(res_RS)  
    res_RSS <- c(res_RSS, cal_RSS)
}            

res_RSS 
coef_hat$RSS <- res_RSS


## 잔차제곱합이 가장 작은 회귀계수
coef_hat[coef_hat$RSS == min(coef_hat$RSS), ]
min_idx <- which.min(coef_hat$RSS)



## 시각화 
#install.packages("scatterplot3d")
library(scatterplot3d)
g <- scatterplot3d(coef_hat, pch = 16, angle = 40, color = "skyblue",
                   xlab = expression(beta[0]), ylab = expression(beta[1]), main = "RSS plot")
g$points3d(x = coef_hat$b0_hat[min_idx], y = coef_hat$b1_hat[min_idx], 
           z = coef_hat$RSS[min_idx], col = "red", pch = 11)    

