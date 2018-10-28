### Dritte Iteration ###
### Gleiche Vorprozess wie letzte Iteration ###

# Zufallsfunktionen benutzen, um zufällige Ergebnisse konstant zu erzeugen

set.seed(42)

# Variablenreduktion

data <- read.csv("PUBG_Player_Statistics.csv")

solo_data <- data[c("solo_KillDeathRatio", "solo_HealsPg", 
                    "solo_MoveDistancePg", 
                    "solo_AvgSurvivalTime", 
                    "solo_AvgWalkDistance")]

# notwendige Packages laden

library(rgl)

# PCA Analyse mit Skalierung

pca_result <- prcomp(solo_data, scale=TRUE)

fit1 <- pca_result

#Transformation von PCA

pca_matrix <- cbind(matrix(fit1$x[,1]), matrix(fit1$x[,2]), 
           matrix(fit1$x[,3]))
l <- data.frame(pca_matrix)


# Menge der Clusterzentren und dazugehörige BCSS/CSS 

result <- c()

for(i in 2:6){
  fit_cluster <- kmeans(l, i, nstart = 5)
  result[i-1] <- fit_cluster$betweenss/fit_cluster$totss*100
}

x <- c(2:6)
y <- c(result)

# Visualisierung : Menge der Clusterzentren und dazugehörige BCSS/CSS

plot(x, y, xlab="Number of Clusters", ylab="between_SS / total_SS  (%)", 
     col="blue", pch=21, bg="blue", ylim=c(20, 80))  
lines(x, y, col="blue", type="b", lty=2)

text(x, y, labels=round(y), cex= 0.7, pos=3)

# 3D Visualisierung der K-means Clustering in 4 Gruppen

# Clustering mittels K-means, dieses mal nur 4 Zentren

fit_cluster <- kmeans(l, 4, nstart = 5)

l$groups <- fit_cluster$cluster
attach(l)

library(rgl)
plot3d(X1, X2, X3,
       xlab = "PC1", ylab = "PC2", zlab = "PC3",
       col=rainbow(4)[groups], 
       size = 0.5, type='s')


legend3d("topright", legend = paste('Group', c('1', '2', '3', '4')),
         pch = 16, cex=2, inset=c(0.02), col = rainbow(4))

