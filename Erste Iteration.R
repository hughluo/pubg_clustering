### Erste Iteration ###
# Zufallsfunktionen benutzen, um zufällige Ergebnisse konstant zu erzeugen

set.seed(42)

# Daten laden

# Ordner mit dem Datensatz nennen

setwd("C:/Users/Hades/Desktop/Marketing-R/R-Programm-Code")

data <- read.csv("PUBG_Player_Statistics.csv")
data1 <- data 

#pick the neccessary variables
solo_data <- data1[c("solo_KillDeathRatio", "solo_WinRatio", 
                     "solo_Top10Ratio", "solo_DamagePg", 
                     "solo_HealsPg", 
                     "solo_KillsPg", "solo_MoveDistancePg", 
                     "solo_TimeSurvivedPg", 
                     "solo_AvgSurvivalTime", "solo_AvgWalkDistance")]

# notwendige Packages laden

library(ggplot2)

# PCA Analyse mit Skalierung

pca_result <- prcomp(solo_data, scale=TRUE)

fit1 <- pca_result

#Transformation von PCA

pca_matrix <- cbind(matrix(fit1$x[,1]), matrix(fit1$x[,2]), 
                    matrix(fit1$x[,3]))
l <- data.frame(pca_matrix)

# PCA Ergebnis
# x1 as PC1-Koordinate
# x1 as PC2-Koordinate
# x1 as PC3-Koordinate

ggplot(data= l, aes(x= X1, y=X2, color=X3)) + 
   geom_point() + scale_colour_gradient2()

# Ergebnis des Clusterings

result <- c()

for(i in 2:5){
  fit_cluster <- kmeans(l, i, nstart = 5)
  result[i-1] <- fit_cluster$betweenss/fit_cluster$totss*100
  print(fit_cluster)
}
