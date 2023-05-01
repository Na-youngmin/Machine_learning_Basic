####################### 패턴탐색 기말 프로젝트 #######################
#############       데이터사이언스 20183217 나영민     ###############
######################################################################

#### 필요 패키지 및 데이터 로드
library(PerformanceAnalytics)
library(dplyr)
library(fmsb)
library(reshape2)
library(ggplot2)
library(ROCR)

df <- read.csv('wisc_bc_data.csv')

df %>% View()
df$diagnosis <- ifelse(df$diagnosis == 'M',1,0)
df$diagnosis <- factor(df$diagnosis ,levels = c(0,1),labels = c("Benign", "Malignant"))
str(df$diagnosis)


#### 데이터 전처리
## 1. 종속변수, 독립변수 나누기
## 2. 결측치 확인
## 3. 중복값 확인
## 4. 종속변수 비율 확인
## 5. 다중공선성 검정
## 6. 표준화, 척도 변환 

## 1. 종속변수, 독립변수 나누기
Y <- df$diagnosis
X <- df[,c(3:32)]

## 2. 결측치 확인
colSums(is.na(df))
is.na(df) %>% table()

## 3. 중복값 확인
sum(duplicated(df))

## 4. 종속변수 비율 확인
prop.table(table(df$diagnosis))

## 5. 다중공선성 검정
chart.Correlation(X[,c(1:10)], histogram = TRUE)
chart.Correlation(X[,c(11:20)], histogram = TRUE)
chart.Correlation(X[,c(21:30)], histogram = TRUE)
# 두 변수간 상관계수가 0.9가 넘는 변수들이 여럿 있었음
# 독립변수간 상관성이 강하면 추정된 회귀계수의 분산이 매우 커지게 되어 신뢰하기 힘들어짐
# 따라서 VIF 검정을 통해 변수를 삭제

# 변수가 30개 이므로 사용자 정의함수 생성
vif_func <- function(in_frame,thresh=10, trace=F,...){
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  vif_init <- vector('list', length = ncol(in_frame))
  names(vif_init) <- names(in_frame)
  var_names <- names(in_frame)
  
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val,' ~ .'))
    vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
  }
  
  vif_max<-max(unlist(vif_init))
  
  if(vif_max < thresh){
    if(trace==T){
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('', times = nrow(vif_init) ),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  
  else{
    in_dat<-in_frame
    while(vif_max >= thresh){
      vif_vals <- vector('list', length = ncol(in_dat))
      names(vif_vals) <- names(in_dat)
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val,' ~ .'))
        vif_add <- VIF(lm(form_in,data=in_dat,...))
        vif_vals[[val]] <- vif_add
      }
      max_row <- which.max(vif_vals)
      vif_max<-vif_vals[max_row]
      if(vif_max<thresh) break
    
      if(trace==T){
        vif_vals <- do.call('rbind', vif_vals)
        vif_vals
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        cat('\n')
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
    }
    return(names(in_dat))
  }
  
}

X_independent <- vif_func(X, thresh=10, trace=T) 

# vif 검정을 끝낸 변수들만 담긴 데이터프레임 생성
X_2 <- X[,X_independent]
str(X_2)
summary(X_2)

chart.Correlation(X_2, histogram = TRUE)
# 0.9의 상관관계를 가지는 변수들은 사라졌지만 아직도 높은 상관계수를 가지는 변수들이 존재

## 6. 표준화, 척도 변환 
# 데이터의 단위들이 다르기 때문에 표준화를 진행
X_3 <- scale(X_2)
summary(X_3)

#### EDA
## 1. t-test 검정
## 2. 데이터 시각화

## 1. t-test 검정
# 진단결과 (M,B)의 집단별로 설명변수 간의 차이 존재여부를 알기 위해 t-test 진행
# 이때 p값이 0.05를 초과하는 변수들은 제거

X_names <- names(data.frame(X_3))
X_names

t.test_p.value_df <- data.frame()

for (i in 1:length(X_names)){
  t.test_p.value <- t.test(df[,X_names[i]]~df$diagnosis, var.equal = TRUE)$p.value
  t.test_p.value_df[i,1] <- X_names[i]
  t.test_p.value_df[i,2] <- t.test_p.value
}

colnames(t.test_p.value_df) <- c('x_var_name','p.value')
arrange(t.test_p.value_df,p.value)

# p_value가 0.05보다 큰 값을 가지는 변수제거
# 삭제 되는 변수: dimension_se, smoothness_se, dimension_mean,texture_se ,symmetry_se

t.test_filtered <- t.test_p.value_df$p.value < 0.05
X_names_filtered <- X_names[t.test_filtered]

X_4 <- data.frame(X_3[,X_names_filtered])
str(X_4)

## 2. 데이터 시각화
# 2 - 1. Box plot
# 12개 설명변수에 대해 M그룹과 B그룹을 분리해 상자그림을 그림

# 그래프로 보기 편하게 p.value 기준으로 내림차순 정렬
t.test_p.value_df.sorted <- arrange(t.test_p.value_df[t.test_filtered,],desc(p.value))
t.test_p.value_df.sorted

X_names_sorted <- t.test_p.value_df.sorted$x_var_name
X_names_sorted

X_5 <- X_4[X_names_sorted]
head(X_5,2)

df_2 <- data.frame(Y, X_5)
str(df_2)

# boxplot 그리기
df_2_melt <- melt(df_2, id.var = 'Y')

ggplot(data = df_2_melt[df_2_melt$value < 3,],aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(Y))) +
  theme_bw() +
  coord_flip()


#t-test 결과 p-value 가 작았던 설명변수 순서대로 위에서 부터 아래로 시각화
# p-value가 작을 수록 상자그림에서 보면
#Malignant(악성)인 그룹과 Benign(양성) 간의 차이가 큼

# scatter plot 그리기

#p-value가 가장 작게 나왔던 상위 변수
# 'points_mean'를 기준으로 
#다음으로 작은 변수'area_worst', 'perimeter_se' 그리고 중간값인 'texture_mean', 가장 큰 값인 'concavity_se'
# 조합해서 산점도로 시각화 
#이때 진단결과(악성: 'M', 1, vs. 양성: 'B', 0) 별로 색깔을 다르게 함


g1 <- ggplot(data=df_2, aes(x=points_mean, y=area_worst, colour=as.factor(Y), alpha=0.7)) +
   geom_point(shape=20, size=2) +
   ggtitle("Scatter Plot of points_mean & area_worst")

g2 <- ggplot(data=df_2, aes(x=points_mean, y=perimeter_se, colour=as.factor(Y), alpha=0.7)) +
  geom_point(shape=20, size=2) +
  ggtitle("Scatter Plot of points_mean & perimeter_se")

g3 <- ggplot(data=df_2, aes(x=points_mean, y=texture_mean, colour=as.factor(Y), alpha=0.7)) +
  geom_point(shape=20, size=2) +
  ggtitle("Scatter Plot of points_mean & texture_mean")

g4 <- ggplot(data=df_2, aes(x=points_mean, y= concavity_se, colour=as.factor(Y), alpha=0.7)) +
  geom_point(shape=20, size=2) +
  ggtitle("Scatter Plot of points_mean & concavity_se")

grid.arrange(g1, g2,g3, g4, ncol=2)

#### 예측 모델 생성
## 1. 독립변수 개별 로지스틱 회귀분석 실시 함수 생성
## 2. 모든 변수 넣어서 시행 
## 3. 성능확인

## 1. 독립변수 개별 로지스틱 회귀분석 실시 함수 생성
res_logistic_reg <- data.frame()

logistic_reg <- function(data, y_index, x_index, times, thresh, positive_class, nonpositive_class) {
  
  # 필요 패키지 로드
  require(dplyr)
  require(pscl)
  require(Epi)
  require(caret)
  
  # seed 고정
  set.seed(1124)
  train_idx <- createDataPartition(data[, y_index], p = 0.7, times = 20, list = F)
  
  
  for (i in 1:times) {
    train = data[train_idx[, i], ]
    test = data[-train_idx[, i], ]
    
    train_cols = colnames(train)

    
    for (j in x_index) {
      model <- glm(Y ~ . , data = train[c(y_index, j)], family = "binomial"(link = 'logit'))
      
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



res_logistic_reg <- logistic_reg(data = df_2, y_index = 1, x_index = 2:12, times = 20, thresh = 0.5, 
                                 positive_class = "Malignant", nonpositive_class = "Benign")
res_logistic_reg


## 2. 모든 변수 넣어서 시행 
res_logistic_reg_full <- data.frame()

logistic_reg_full <- function(data, y_index, times, thresh, positive_class, nonpositive_class) {
  
  # 필요 패키지 로드
  require(dplyr)
  require(pscl)
  require(Epi)
  require(caret)
  
  # seed 고정
  set.seed(1124)
  train_idx <- createDataPartition(data[, y_index], p = 0.7, times = 20, list = F)
  
  
  for (i in 1:times) {
    train = data[train_idx[, i], ]
    test = data[-train_idx[, i], ]
    
    train_cols = colnames(train)
    #y_var = noquote(train_cols[y_index])
    
    
    model <- glm(Y ~ concavity_se + compactness_se + dimension_worst +
                   symmetry_mean + points_se + texture_mean + symmetry_worst + 
                   smoothness_worst +
                   perimeter_se + area_worst + points_mean + smoothness_mean ,
                 data = train, 
                 family = "binomial"(link = 'logit'))
    model <- step(model, direction = "both", scope = list(upper = model, lower = ~1))
    
    test_sub <- test
    test_sub$prob <- predict(model, newdata = test[-1], type = "response")    
    
    yhat <- ifelse(test_sub$prob > thresh, positive_class, nonpositive_class)
    yhat <- factor(yhat, levels = c(nonpositive_class, positive_class))
    
    pred_res <- confusionMatrix(yhat, test_sub[, y_index], positive = positive_class)
    
    acc <- unname(pred_res$overall["Accuracy"])
    sens <- unname(pred_res$byClass["Sensitivity"])
    spec <- unname(pred_res$byClass["Specificity"])
    
    df_logistic_reg <- data.frame('Accuracy' = acc,
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

# AIC 값이 가장 낮은 식
#diagnosis ~ concavity_se + compactness_se + texture_mean + symmetry_worst + 
#smoothness_worst + area_worst + points_mean
result <- logistic_reg_full(data = df_2, y_index = 1, times = 20, thresh = 0.5, 
                            positive_class = "Malignant", nonpositive_class = "Benign")

## 3. 성능확인

result <- as.data.frame(result)
result
result_df <- data.frame(score = c('정확도','민감도','특이도'),
                        average = c(mean(result$Accuracy),mean(result$Sensitivity),mean(result$Specificity)),
                        sd = c(sd(result$Accuracy),sd(result$Sensitivity), sd(result$Specificity)))

result_df


# 정확도, 민감도, 특이도 시각화
plot(result$Accuracy,type = 'b')
abline(h = mean(result$Accuracy),col = 'red')
plot(result$Sensitivity, type = 'b')
abline(h = mean(result$Sensitivity),col = 'red')
plot(result$Specificity,type = 'b')
abline(h = mean(result$Specificity),col = 'red')


