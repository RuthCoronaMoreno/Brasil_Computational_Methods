# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Topic: An introduction to multivariate analysis
# --------------------------------------------------#

library(vegan)
library(cluster)


data(dune)
data(dune.env)
table(dune.env$Management)

#-----------Cluster analysis of the dune vegetation---------
#************************************************************

#We calculate two dissimlairty indices between sites: Bray-Curtis distance and Chord distance
bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

#We perform the cluster analysis. Which is the default clustering method?
b_cluster <- hclust(bray_distance, method = "mcquitty")  #required(cluster)
c_cluster <- hclust(chord_distance, method = "average")

#Let’s plot them side to side
par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1, 1))


#-----------------------------PCA----------------------------
#************************************************************

norm <- decostand(dune, "norm")
pca <- rda(norm)
plot(pca)
summary(pca)
eigen(pca)
dim(dune)

#Let's do a PCA of the environment matrix
nameS(dune.env)
apply(dune.env, 2, class)

library(dplyr)
dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)

pca_env <- rda(dune.env[, c("A1", "Moisture", "Manure")])
plot(pca_env)
pairs(dune.env)
cor(dune.env[,c("A1", "Moisture", "Manure")])
