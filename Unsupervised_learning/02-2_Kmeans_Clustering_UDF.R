



##################### 빅데이터마이닝:패턴탐색 #######################
##                   사용자 정의 함수 만들기                       ##
#####################################################################


## 1. 내장데이터 iris 불러오기
data(iris)
str(iris)



## 2. 군집분석 결과 
minmax <- function(x) {
  y = (x - min(x)) / (max(x) - min(x))
  
  return(y)
}


resulting_cluster <- function(data, scaling_method, y_index) {
  
  ## 필요 패키지 로드
  require(ggpubr)
  require(GGally)
  require(factoextra)
  
  
  ## 연속형 변수 선별
  class_var <- sapply(data, is.numeric)
  data_numeric <- data[names(class_var[unname(class_var) == T])]
  colnms <- colnames(data_numeric)
  
  
  ## scaling 
  if (scaling_method == "minmax") {
    data_sc <- minmax(data_numeric)
  }
  
  else {
    data_sc <- as.data.frame(scale(data_numeric)) 
  }
  
  #print(data_sc)
  
  
  ## 군집분석 실시
  set.seed(1130)
  
  res_k_means <- kmeans(data_sc, centers = length(levels(data[, y_index])), 
                        nstart = 30)  
  
  data_sc$cluster <- res_k_means$cluster
  data_sc$cluster <- as.factor(data_sc$cluster)
  t <- table(data_sc$cluster, data[, y_index])
  
  
  ## 군집분석 결과 시각화
  ## 평행좌표그림
  numeric_index <- which(colnames(data) %in% names(class_var[unname(class_var) == T]))
  parallel <- ggparcoord(data = data_sc,
                         columns = numeric_index, groupColumn = "cluster") +
                         labs(x = "변수", y = "", title = "평행좌표그림") +
                         theme_bw() + 
                         theme(plot.title = element_text(hjust = 0.5),
                               axis.text.x=element_text(angle=45, hjust=1)) 
  
  
  ## 산점도
  scatterplot <- plot(data_sc[, numeric_index], col = data_sc$cluster)
  
  
  ## 군집결과 시각화
  res_pca <- prcomp(data_sc[ , -y_index], scale = F)  
  coord <- as.data.frame(get_pca_ind(res_pca)$coord)
  coord$cluster <- factor(res_k_means$cluster)
  coord$label <- data[, y_index]
  
  eigenvalue <- round(get_eigenvalue(res_pca), 1)
  variance.percent <- eigenvalue$variance.percent
  head(eigenvalue)
  
  
  g <- ggscatter(coord, x = "Dim.1", y = "Dim.2", 
                 color = "cluster", palette = "npg", ellipse = T, ellipse.type = "convex",
                  shape = "label", size = 2,  legend = "right", ggtheme = theme_bw(),
                  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
                  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )) +
        stat_mean(aes(color = cluster), size = 6)
  
  
  
  
  return(list(t, parallel, scatterplot, g))
}
  


resulting_cluster(data = iris, scaling_method = "minmax", y_index = 5)




## 3. 개별 변수 별 군집분석 실시 시 결과 확인 함수 생성
res_ind_cluster <- list()

ind_cluster <- function(data, scaling_method, y_index) {
  
  ## 연속형 변수 선별
  class_var <- sapply(data, is.numeric)
  data_numeric <- data[names(class_var[unname(class_var) == T])]
  colnms <- colnames(data_numeric)
  
  
  ## scaling 
  if (scaling_method == "minmax") {
    data_sc <- minmax(data_numeric)
  }
  
  else {
    data_sc <- as.data.frame(scale(data_numeric)) 
  }
  
  
  for (i in colnms) {
    kmean_res <- kmeans(data_sc[, i], centers = length(levels(data[, y_index])), nstart = 30)
    
    data_sc$cluster <- kmean_res$cluster
    data_sc$cluster <- as.factor(data_sc$cluster)
    
    t <- table(data_sc$cluster, data[, y_index])
    res_ind_cluster[[i]] <- t
  }
  

  return(res_ind_cluster)
}


ind_cluster(data = iris, scaling_method = "minmax", y_index = 5)












