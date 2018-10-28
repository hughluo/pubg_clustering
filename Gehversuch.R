### Gehversuch ###
# Zufallsfunktionen benutzen, um zufällige Ergebnisse konstant zu erzeugen

set.seed(42)

# Daten laden

iris2 <- iris
iris2$Species <- NULL

# notwendige Packages laden

library(ggplot2)

library(dplyr)

# Menge der Clusterzentren und dazugehörige BCSS/CSS

result <- c()

for(i in 2:6){
  fit_cluster <- kmeans(iris2, i, nstart = 5)
  result[i-1] <- fit_cluster$betweenss/fit_cluster$totss*100
  print(fit_cluster)
}

# Visualisierung von BCSS/CSS-Veränderung

x <- c(2:6)
y <- result


plot(x, y, xlab="Number of Clusters", 
     ylab="between_SS / total_SS  (%)", 
     col="blue", pch=21, bg="blue", ylim=c(70, 100))  
lines(x, y, col="blue", type="b", lty=2)
text(x, y, labels=round(y), cex= 0.7, pos=3)

# Überblick

iris_with_groups <- iris2
kmeans.result <- kmeans(iris2, 3)
iris_with_groups$groups <- kmeans.result$cluster

iris_grouped <- iris_with_groups %>% group_by(groups)

count_iris <- count(iris_grouped)

ggplot(aes(x=groups, y=n), data=count_iris) + geom_bar(stat = "identity") + 
  labs(y = "Number of Objects", x = "Groups")

names(count_iris) <- c("Groups", "Number_of_Objects")

# Matrix von Verteilungen

pairs(iris2[1:4], main="Iris Scatterplot Matrix", 
      col=c("red","green3","blue")[kmeans.result$cluster],
      pch=16)

# Ergebnis des Clusterings

iris_grouped_sum <- iris_grouped %>% 
  summarise(Sepal.Length = mean(Sepal.Length),
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width),
  )

iris_transposed <- as.data.frame(t(iris_grouped_sum))

# Zeile 1 brauchen nicht
iris_transposed <- iris_transposed[-1, ] 

names(iris_transposed) <- c('Group 1', 'Group 2', 'Group 3')

# Vergleich mit Spieces
table(kmeans.result$cluster, iris$Species)
