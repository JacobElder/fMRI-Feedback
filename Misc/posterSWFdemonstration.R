setwd("~/Google Drive/Volumes/")
posDf <- read.csv("./Research Project/Trait Network_Behaviral/generating network/output/adjacencyMatrix_p.csv")
traitLevelAttribute_p <- read.csv("/Volumes/GoogleDrive/My Drive/Volumes/Research Project/Trait Network_Behaviral/generating network/output/traitLevelAttribute_p.csv")

colnames(posDf) <-traitLevelAttribute_p$trait
rownames(posDf) <-traitLevelAttribute_p$trait
posMat <- as.matrix(posDf)
posGraph <- graph.adjacency(posMat)

traitLevelAttribute_p$trait[c(68,21,11,4)]
traitLevelAttribute_p$trait[c(47,46,54,39)]

simSmart <- similarity.dice(posGraph)[86,c(68,21,11,4)] # Outgoing, to knowledgeable, contemplative, clever, capable
simsSocial <- similarity.dice(posGraph)[86,c(47,46,54,39)] # Outgoing, to Fun, Friendly, Good-Humored, Extraverted

feedback <- c(6,7,7,6)
sum(simSmart*feedback)/sum(simSmart)
sum(simsSocial*feedback)/sum(simsSocial)
