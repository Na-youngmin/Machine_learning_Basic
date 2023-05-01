



##################### 빅데이터마이닝:패턴탐색 #######################
##                   사용자 정의 함수 만들기                       ##
#####################################################################


## 1. 데이터 불러오기 및 전처리

lung <- read.table("lung-cancer.txt", header = T, stringsAsFactors = F)
colSums(is.na(lung))

str(lung)
lung$LC <- factor(lung$LC, levels = c(0,1), labels = c("정상", "폐암"))

for (var in colnames(lung)[2:4]){
  lung[, var] <- as.factor(lung[, var])
  print(str(lung))
}



## 2. 독립변수 개별 로지스틱 회귀분석 실시 함수 생성
res_logistic_reg <- data.frame()

logistic_reg <- function(data, y_index, x_index, times, thresh, positive_class, nonpositive_class) {
  
  # 필요 패키지 로드
  require(dplyr)
  require(pscl)
  require(Epi)
  require(caret)
  
  # seed 고정
  set.seed(1124)
  train_idx <- createDataPartition(data[, y_index], p = 0.7, times = 10, list = F)
  
  
  for (i in 1:times) {
    train = data[train_idx[, i], ]
    test = data[-train_idx[, i], ]
    
    train_cols = colnames(train)
    #y_var = noquote(train_cols[y_index])
    
    for (j in x_index) {
      model <- glm(LC ~ . , data = train[c(y_index, j)], family = "binomial")
      #summary(model)
      
      test_sub <- test
      test_sub$prob <- predict(model, newdata = test[-1], type = "response")    

      yhat <- ifelse(test_sub$prob > thresh, positive_class, nonpositive_class)
      yhat <- factor(yhat, levels = c(nonpositive_class, positive_class))
      
      pred_res <- confusionMatrix(yhat, test_sub[, y_index], positive = positive_class)
      
      acc <- unname(pred_res$overall["Accuracy"])
      sens <- unname(pred_res$byClass["Sensitivity"])
      spec <- unname(pred_res$byClass["Specificity"])
      
      df_logistic_reg <- data.frame('R' = i,
                                    'variable' = train_cols[j], 
                                    'Accuracy' = acc,
                                    'Sensitivity' = sens,
                                    'Specificity' = spec) 
      
      res_logistic_reg = rbind(res_logistic_reg, df_logistic_reg)
      
      final_res_logistic <- res_logistic_reg %>%
        group_by(variable) %>%
        summarize(mean_acc = round(mean(Accuracy), 3),
                  sd_acc = round(sd(Accuracy), 3),
                  mean_sens = round(mean(Sensitivity), 3),
                  sd_sens = round(sd(Sensitivity), 3),
                  mean_spec = round(mean(Specificity), 3),
                  sd_spec = round(sd(Specificity), 3))
      
      
    }
  }
  
  return(res_logistic_reg)
}



res_logistic_reg <- logistic_reg(data = lung, y_index = 1, x_index = 2:6, times = 10, thresh = 0.35, 
                                 positive_class = "폐암", nonpositive_class = "정상")




## 3. 모든 변수 넣어서 시행 
res_logistic_reg_full <- data.frame()

logistic_reg_full <- function(data, y_index, times, thresh, positive_class, nonpositive_class) {
  
  # 필요 패키지 로드
  require(dplyr)
  require(pscl)
  require(Epi)
  require(caret)
  
  # seed 고정
  set.seed(1124)
  train_idx <- createDataPartition(data[, y_index], p = 0.7, times = 10, list = F)
  
  
  for (i in 1:times) {
    train = data[train_idx[, i], ]
    test = data[-train_idx[, i], ]
    
    train_cols = colnames(train)
    #y_var = noquote(train_cols[y_index])
    
  
    model <- glm(LC ~ . , data = train, family = "binomial")
    model <- step(model, direction = "both", scope = list(upper = model, lower = ~1))
      
    test_sub <- test
    test_sub$prob <- predict(model, newdata = test[-1], type = "response")    
      
    yhat <- ifelse(test_sub$prob > thresh, positive_class, nonpositive_class)
    yhat <- factor(yhat, levels = c(nonpositive_class, positive_class))
      
    pred_res <- confusionMatrix(yhat, test_sub[, y_index], positive = positive_class)
      
    acc <- unname(pred_res$overall["Accuracy"])
    sens <- unname(pred_res$byClass["Sensitivity"])
    spec <- unname(pred_res$byClass["Specificity"])
      
    df_logistic_reg <- data.frame('R' = i,
                                  'Accuracy' = acc,
                                  'Sensitivity' = sens,
                                  'Specificity' = spec) 
      
    res_logistic_reg_full = rbind(res_logistic_reg_full, df_logistic_reg)
      
    final_res_logistic_full <- res_logistic_reg %>%
        summarize(mean_acc = round(mean(acc), 3),
                  sd_acc = round(sd(acc), 3),
                  mean_sens = round(mean(sens), 3),
                  sd_sens = round(sd(sens), 3),
                  mean_spec = round(mean(spec), 3),
                  sd_spec = round(sd(spec), 3))
      
  }
  
  return(res_logistic_reg_full)
}



logistic_reg_full(data = lung, y_index = 1, times = 10, thresh = 0.35, 
                  positive_class = "폐암", nonpositive_class = "정상")







