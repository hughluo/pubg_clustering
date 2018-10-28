### Zweite Iteration ###
# Zufallsfunktionen benutzen, um zufällige Ergebnisse konstant zu erzeugen

set.seed(42)

# Daten laden

# notwendige Packages laden

library(psych)

library(rgl)

# Ordner mit dem Datensatz nennen
setwd("C:/Users/Hades/Desktop/Marketing-R/R-Programm-Code")

data <- read.csv("PUBG_Player_Statistics.csv")
data1 <- data 

# 10 Variablen bei der ersten Iteration
solo_data <- data1[c("solo_KillDeathRatio", "solo_WinRatio", 
                     "solo_Top10Ratio", "solo_DamagePg", 
                     "solo_HealsPg", 
                     "solo_KillsPg", "solo_MoveDistancePg", 
                     "solo_TimeSurvivedPg", 
                     "solo_AvgSurvivalTime", "solo_AvgWalkDistance")]

# Streudiagramm-Matrix von 10 Variablen bei der ersten Iteration

pairs.panels(solo_data,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# Variablenreduktion

solo_data <- data[c("solo_KillDeathRatio", "solo_HealsPg", 
                    "solo_MoveDistancePg", 
                    "solo_AvgSurvivalTime", 
                    "solo_AvgWalkDistance")]

# Streudiagramm-Matrix von 5 Variablen

pairs.panels(solo_data,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# PCA Analyse mit Skalierung

pca_result <- prcomp(solo_data, scale=TRUE)

fit1 <- pca_result

#Transformation von PCA

pca_matrix <- cbind(matrix(fit1$x[,1]), matrix(fit1$x[,2]), 
                    matrix(fit1$x[,3]))
l <- data.frame(pca_matrix)

# Clustering mittels K-means

fit_cluster <- kmeans(l, 5, nstart = 5)
fit_cluster

# 3D Visualisierung der K-means Clustering in 5 Gruppen

library(rgl)
l$groups <- fit_cluster$cluster
attach(l)
plot3d(X1, X2, X3, 
       xlab = "PC1", ylab = "PC2", zlab = "PC3",
       size = 10, col=l$groups)