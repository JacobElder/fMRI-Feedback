library(igraph)

setwd("~/Google Drive/Volumes/")
posDf <- read.csv("./Research Project/Trait Network_Behaviral/generating network/output/adjacencyMatrix_p.csv")
posMat <- as.matrix(posDf)
posGraph <- graph.adjacency(posMat)
simMat <- similarity.dice(posGraph)

k <- apply(simMat, 1, max, na.rm=TRUE)
order <- sort(k, decreasing=TRUE, index.return=TRUE)$ix
simMat[order, order]

heatmap(simMat)
