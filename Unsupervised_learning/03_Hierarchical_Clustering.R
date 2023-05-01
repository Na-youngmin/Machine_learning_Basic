

######################## 4주차 #########################
###                계층적 군집분석                   ###
########################################################


## 데이터 생성
ex <- data.frame("id" = LETTERS[1:6], 
           x = c(1, 2, 4, 5, 5, 5.5), y = c(4, 4.5, 2, 4.5, 1.5, 2))
ex

plot(ex$x, ex$y, xlim = c(0, 6), ylim = c(0, 5))
text(ex$x+0.5, ex$y, label = ex$id, cex = 0.8)


## 유클리디안 거리를 통해 Distance matrix 생성
dist <- round(dist(ex[-1], method = "euclidean"), 2)
min(dist)


c_res <- hclust(dist, method = "single")
rev(c_res)
plot(c_res, hang = -1)
rect.hclust(c_res, k = 2, border = "red")


c_res2 <- cutree(c_res, k = 2)
c_res3 <- cutree(c_res, k = 3)

par(mfrow = c(1,2))
plot(ex[-1], pch = 18, col = c_res2, main = "K = 2")
text(ex$x+0.5, ex$y, label = ex$id, cex = 0.8)

plot(ex[-1], pch = 18, col = c_res3, main = "K = 3")
text(ex$x+0.5, ex$y, label = ex$id, cex = 0.8)


## 비계층적 군집분석을 통한 K 결정
#install.packages(c("factoextra", "cluster"))
library(factoextra)
library(cluster)

kmean_res2 <- kmeans(ex[, -1], center = 2, iter.max = 10000)
kmean_res3 <- kmeans(ex[, -1], center = 3, iter.max = 10000)

sil2 <- silhouette(kmean_res2$cluster, dist)
fviz_silhouette(sil2)

sil3 <- silhouette(kmean_res3$cluster, dist)
fviz_silhouette(sil3)



