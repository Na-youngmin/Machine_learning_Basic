

###################### 11주차 수업 ##########################
##                   로지스틱회귀분석                      ##
#############################################################

## 0. 변수 설명
## 종속변수 : LC(폐암 여부)
## 독립변수 : BK(N: 흑인이 아님, Y: 흑인), SS(사회 경제적 사회수준), YR(교육년수)
## 1. 데이터 불러오기
setwd("D:/[2022-2] 패턴탐색/11주차/")

lung <- read.table("lung-cancer.txt", header = TRUE, stringsAsFactors = FALSE)
colSums(is.na(lung))


## 2. 데이터 전처리
str(lung)
lung$LC <- factor(lung$LC, levels = c(0,1), labels = c("정상", "폐암"))

for (var in colnames(lung)[2:4]){
  lung[, var] <- as.factor(lung[, var])
  print(str(lung))
}


## 3. 회귀모형식을 통해 x에 대한 cutoff value 구하기
fit <- glm(LC ~ YR, data = lung, family = "binomial")
summary(fit)   # 회귀모형식 : log_e(p/1-p) = -2.2755 + 0.0533 * YR

p_hat <- fitted(fit)   # lr.eta는 p의 추정치
p_hat

x_cutoff <- c()

for (i in 1:nrow(lung)) { 
  x_cutoff[i] <- (log(p_hat[i]/(1 - p_hat[i])) + 2.2755) / 0.0533
}

x_cutoff



## 4. 로지스틱 회귀분석
set.seed(1111)

idx <- sample(nrow(lung), 0.7 * nrow(lung))
train <- lung[idx, ] ; nrow(train)
test <- lung[-idx, ] ; nrow(test)

table(train$LC) ; table(test$LC)

fit2.full <- glm(LC ~ ., data = train, family = "binomial")
summary(fit2.full)

fit2.null <- glm(LC ~ 1, data = lung, family = "binomial")
fit2.fin <- step(fit2.full, direction = "both",
                 scope = list(lower = fit2.null, upper = fit2.full))
summary(fit2.fin)


## 5. ROC Curve
# cutoff를 변화해가며 직접 그린 결과 
library(caret)

y_pred <- predict(fit2.fin, newdata = test[ , -1], type="response")
summary(y_pred)

roc_df <- data.frame(cut_value = seq(0.01, 0.80, by = 0.005))

sens <- c() ; spec <- c()
for (j in 1:nrow(roc_df)) {
  y_res <- ifelse(y_pred > roc_df$cut_value[j], "폐암", "정상")
  y_res <- factor(y_res, levels = c("정상", "폐암"), labels = c("정상", "폐암")) 
  pred_res <- confusionMatrix(y_res, test$LC, positive = "폐암")
  
  sens[j] <- unname(pred_res$byClass["Sensitivity"])
  spec[j] <- unname(pred_res$byClass["Specificity"])
  
  df <- data.frame("sens" = sens,
                   "spec" = spec)
  print(df)
}

roc_df <- cbind(roc_df, df)
max_ix <- which.max(roc_df$sens + roc_df$spec)
roc_df[max_ix, ]

library(ggplot2)

ggplot(roc_df, aes(x = 1-spec, y = sens)) + 
  geom_point(aes(x=1-spec, y=sens), size = 1.3) + 
  geom_line() +
  theme_bw() + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  labs(x = "1-Specificity", y = "Sensitivity")  


# R 결과
library(Epi)
test$prob <- predict(fit2.fin, newdata = test[, -1], type = "response")

Epi::ROC(form = LC ~ prob, data = test, plot ="ROC", AUC = T)


## 6. 최적의 cutoff 비교
max_idx <- which.max(roc_df$sens + roc_df$spec)
roc_df[max_idx, ]



