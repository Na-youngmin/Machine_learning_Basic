#################### 12주차 수업 ####################
##                   의사결정 나무                 ##
#####################################################

## 0.데이터 설명
## 종속변수 : LC(폐암 여부)
## 독립변수 : BK(N: 흑인이 아님, Y: 흑인), SS(사회 경제적 사회수준), YR(교육년수)
## 1. 데이터 불러오기

lung <- read.table('lung-cancer.txt', header = TRUE, stringsAsFactors = FALSE)
colSums(is.na(lung))

## 2. 데이터 전처리
str(lung)
lung$LC <- factor(lung$LC, levels = c(0,1), labels = c('정상','폐암'))

for (var in colnames(lung)[2:4]){
  lung[,var] <- as.factor(lung[,var])
  print(str(lung))
}

## 3. 전체 데이터를 대상으로 가장 분류를 잘 할 수 있는 변수 탐색
library(RWeka)
lung_1R <- OneR(LC ~., data = lung)
summary(lung_1R)

lung_JRip <- JRip(LC ~ ., data = lung)
summary(lung_JRip)

## 4.데이터셋 분리
set.seed(1111)

idx <- sample(nrow(lung), 0.7*nrow(lung))
train <- lung[idx,] ; nrow(train)
test <- lung[-idx,] ; nrow(test)

prop.table(table(train$LC)) ; prop.table(table(test$LC))


## 5. 의사결정나무 생성
library(rpart)
library(rpart.plot)

tree <- rpart(LC ~., data = train, method = 'class',         # 회귀나무 : method = 'anova'
              parms = list(split = 'gini'),                  # 엔트로피 : information
              control = rpart.control()) 

tree
rpart.plot(tree, extra = 101, digits = 3)

tree$control

## 최적의 minbucket의 수 결정
train_score <- c() ; test_score <-c()

for (i in 1:10) {
  m_case <- rpart(LC ~ .,
                  data = train,
                  control = rpart.control(minbucket = i))
  # 훈련 데이터 점수
  tr_pred <- predict(m_case, newdata = train[,-1], type = 'class')
  tr_score <- sum(train$LC == tr_pred)/nrow(train) * 100
  
  # 테스트 데이터 점수
  te_pred <- predict(m_case, newdata = test[,-1], type = 'class')
  te_score <- sum(test$LC == te_pred)/nrow(test) * 100
  
  train_score <- c(train_scoe, tr_score)
  test_score <- c(test_score, te_score)
}

train_score
test_score

## 두 점수의 차이를 그래프로 표현
plot(1:10, train_score - test_score, type = 'o',
     col = 'darked', xlab = 'minbucket의 갯수',ylab = '점수 차')



y_pred <- predict(tree, newdata = test[-1], type = 'class')
library(caret)
confusionMatrix(y_pred, test$LC, positive = '폐암')


## 7.가지치기 진행
tree$cptable
plotcp(tree)

min_ix <- which, min(tree$cptable[,'xerror'])
opt_cp <- tree$cptable[min_ix,'cp']
tree_prune <- prune(tree, cp = opt_cp)

rpart.plot(tree_prune, cex = 0.7, extra = 101, digits = 3)

y_pred2 <- predict(tree_prune, newdata = test[-1], type = 'class')
confusionMatrix(y_pred2, test$LC, positive = '폐암')

## 8. 오분류 최소화: 비용행렬(cost matrix) => 패널티 반영
## 실제 폐암인데 정상인 것을 놓치는 것이 5배 만큼의 비용이 든다면?
error_cost <- matrix(c(0,1,4,0), ncol = 2,
                     dimnames = list('Predicted'=c('정상','폐암'),
                                     'Reference'=c('정상','폐암')))

tree_cost <- rpart(LC ~ ., data = train, method  = 'class',
                   parms = list(split = 'gini', loss = error_cost))
min_ix2 <- which.min(tree_cost$cptable[,'xerror'])
opt_cp2 <- tree_cost$cptable[min_ix2, 'cp']
tree_prune2 <- prune(tree_cost, cp = opt_cp2)

y_pred3 <- predict(tree_prune2, newdata = test[-1], type = 'class')
confusionMatrix(y_pred3, test$LC, positive = '폐암')


## 9.결과 시각화
library(pROC)
y_prop_prune <- predict(tree_prune, newdata = test[-1], type = 'prob') #가지치기 진행
y_prop_cost <- predict(tree_prune2, newdata = test[-1], type = 'prob') #비용행렬 + 가지치기

roc_prune <- roc(test$LC, y_prob_prune[,2])
roc_cost <- roc(test$LC, y_prob_cost[,2])

roc_df <- data.frame('Sens' = c(roc_prune$sensitivities, roc_cost$sensitivities),
                     'spec' = c(roc_prune$specificities, roc_cost$specificities),
                     'Model' = c(rep('Model1', length(roc_prune$sensitivities)),
                                 rep('Model2', length(roc_cost$sensitivities))))


library(ggplot2)

ggplot(roc_df) +
  geom_line(aes(x = 1 - Spec, y =Sens, col = Model), size = 1.2) +
  labs(x = '1 - 특이도', y = '민감도') +
  theme_bw()

