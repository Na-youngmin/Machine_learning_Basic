

## --------------  패턴탐색 3주차 ---------------- ##
##               "K-means Clustering"              ##
## ----------------------------------------------- ##


## 1. 내장데이터 iris 불러오기
data(iris)
str(iris)


## 2. 데이터 탐색: 결측치 및 이상치 확인, 변수(차원) 선택 
## 최적의 K를 찾아도 고차원 상의 데이터 상에서는 정확성이 떨어지고
## 차원이 높아질수록 거리의 개념이 실질적으로 가까운지 알기 힘듦
colSums(is.na(iris))

num_var <- names(iris)[-5]
boxplot(iris[ , num_var])


## 3. 연속형 변수의 단위통일: 관측치 간의 거리를 이용하므로 변수의 단위에 영향을 미침 
## min-max scaling 
minmax <- function(x) {
  y = (x - min(x)) / (max(x) - min(x))
  
  return(y)
}


#options(scipen = 100)
minmax(iris$Petal.Length)
summary(minmax(iris$Petal.Length))

iris_mn <- iris
for(i in num_var) {
  iris_mn[, i] <- minmax(iris_mn[, i])
}



## 4. 군집의 개수 K 지정
## 4-1. 계층적 군집분석의 덴드로그램을 이용
## 군집과 군집 간의 거리를 측정하는 연결법: 단일연결법, 완전연결법, 평균연결법,
##                                          중심연결법, Ward연결법 


iris <- iris[c("Sepal.Length", "Petal.Length", "Species")]

## 유클리디안 거리
dist(iris[1:2, -3], method = "euclidean")

ecu_dist <- function(x1, x2) {
  y = sqrt(sum((x1 - x2)^2))
  
  return(y)
}


ecu_dist(iris[1, -3], iris[2, -3])
dist_ecu <- dist(iris[, -3], method = "euclidean")


ward_res <- hclust(dist_ecu, method = "ward.D")
plot(ward_res)


## 4-2. 팔꿈치 방법 이용(The Elbow method)
wcss <- c()

for (i in 1:10) {
  
  set.seed(1160)
  kmean_res <- kmeans(iris[, -3], centers = i, iter.max = 1000)
  wcss[i] <- kmean_res$tot.withinss
}

plot(c(1:10), wcss, type = "b", 
     main = "", xlab = "number of clusters", ylab = "WCSS")


## 4-3. 실루엣 방법 이용(The Silhouette method)
#install.packages(c("factoextra", "cluster"))
library(factoextra)
library(cluster)

kmean_res2 <- kmeans(iris[, -3], center = 2, iter.max = 10000)
sil <- silhouette(kmean_res2$cluster, dist_ecu)
fviz_silhouette(sil)


## 5. 군집분석 결과 확인
table(kmean_res2$cluster, iris$Species)

plot(iris[, -5], col = kmean_res2$cluster, main = "Result of K-means clustering")


### 차원(변수)을 축소해서 실루엣 계수를 비교해보세요 !


