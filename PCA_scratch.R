library(dplyr)
library(ggplot2)
library(gridExtra)

### iris data
data <- iris[,1:4] %>% as.matrix()

### データの中心化
center <- apply(data, 2, mean) # average of each column  各列の平均値
data <- sweep(data, 2, center) # sweep average from each column  各列から平均値を引く


### 固有値・固有ベクトル
sigma <- t(data) %*% data # Variance-covariance matrix  分散共分散行列
lambda <- eigen(sigma)$value # eigen value  固有値
phi <- eigen(sigma)$vectors # eigen vector  固有ベクトル

PC_data <- data %*% phi 
PVE <- lambda/sum(lambda) # PVE : the proportion of variance explained 累積寄与率

(prcomp(data) %>% summary() %>% .$importance)[2,] # Check if same as "prcomp"


### Visualization PVE
PVEplot <- qplot(c(1:4), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Cumulative PVE plot
cumPVE <- qplot(c(1:4), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)

### check correlation 相関係数の確認
PC_data[,1:2] %>% cor()
data[,1:2] %>% cor()
