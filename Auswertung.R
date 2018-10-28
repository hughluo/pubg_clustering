### Auswertung ###
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

library(dplyr)

library(ggplot2)

# PCA Analyse mit Skalierung

pca_result <- prcomp(solo_data, scale=TRUE)

fit1 <- pca_result

#Transformation von PCA

pca_matrix <- cbind(matrix(fit1$x[,1]), matrix(fit1$x[,2]), 
                    matrix(fit1$x[,3]))
l <- data.frame(pca_matrix)

fit_cluster <- kmeans(l, 4, nstart = 5)

# Überblick

data_with_groups <- solo_data
data_with_groups$groups <- fit_cluster$cluster


grouped <- data_with_groups %>% group_by(groups)

d <- count(grouped)


ggplot(aes(x=groups, y=n), data=d) + geom_bar(stat = "identity") + 
  labs(y = "Number of Objects", x = "Groups")

names(d) <- c("Groups", "Number of Objects")
#Ergebnis des Clusterings

average_result <- grouped %>% 
  summarise(solo_KillDeathRatio = mean(solo_KillDeathRatio),
            solo_HealsPg = mean(solo_HealsPg),
            solo_MoveDistancePg = mean(solo_MoveDistancePg),
            solo_AvgSurvivalTime = mean(solo_AvgSurvivalTime),
            solo_AvgWalkDistance = mean(solo_AvgWalkDistance)
  )

a <- average_result 

a_transposed <- as.data.frame(t(a))

# Zeile 1 brauchen nicht

a_transposed <- a_transposed[-1, ] 

names(a_transposed) <- c('Group 1', 'Group 2', 'Group 3', 'Group 4')
