library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)
library(ggplot2)
source('LOTRFn.R', echo=TRUE)
# http://www.imsdb.com/Movie%20Scripts/Lord%20of%20the%20Rings:%20Fellowship%20of%20the%20Ring,%20The%20Script.html
# http://www.fempiror.com/otherscripts/LordoftheRings2-TTT.pdf
# http://www.imsdb.com/Movie%20Scripts/Lord%20of%20the%20Rings:%20Return%20of%20the%20King%20Script.html

# ---------------LOTR 1
raw <- readLines("LOTR.txt")
lines <- readToLinesFn(raw)
characterScenes <- parseToCharactersFn(lines)
lines$raw <- trimws(lines$raw)
characterList <- unique(characterScenes$raw)
# manual removal of unwanted/wrong elements
unwanted <- c(characterList[1], characterList[10], characterList[11], characterList[12],
              characterList[13], characterList[14], characterList[15], characterList[17],
              characterList[18], characterList[20], characterList[31], characterList[32],
              characterList[33])
characterList <- characterList[! characterList %in% unwanted]
characterScenes <- characterScenes %>% filter(raw %in% characterList)
rm(unwanted)

# Change STRIDER to ARAGORN (same person)
sameNameFn <- function(x){
  gsub("STRIDER", "ARAGORN", x)
}
characterScenes$raw <- sapply(characterScenes$raw, sameNameFn)

sceneCharacterCount <- characterScenes %>% count(scene,raw)
sceneCharacterMatrix <- sceneCharacterCount %>% acast(raw~scene, fun.aggregate = length)
normMatrix <- sceneCharacterMatrix / rowSums(sceneCharacterMatrix)
characterCluster <- hclust(dist(normMatrix, method="manhattan"))
plot(characterCluster)

# ---------------LOTR 2
raw2 <- readLines("LOTR2.txt")
lines2 <- readToLinesFn(raw2)
characterScenes2 <- parseToCharactersFn(lines2)
lines2$raw <- trimws(lines2$raw)
characterList2 <- unique(characterScenes2$raw)
# manual removal of unwanted/wrong elements
unwanted2 <- c(characterList2[7], characterList2[8], characterList2[10], characterList2[14],
               characterList2[16], characterList2[17], characterList2[18], characterList2[19],
               characterList2[26], characterList2[27], characterList2[29], characterList2[30],
               characterList2[31], characterList2[32], characterList2[36], characterList2[37],
               characterList2[38], characterList2[39], characterList2[41], characterList2[44],
               characterList2[46])
characterList2 <- characterList2[! characterList2 %in% unwanted2]
characterScenes2 <- characterScenes2 %>% filter(raw %in% characterList2)
rm(unwanted2)

sceneCharacterCount2 <- characterScenes2 %>% count(scene,raw)
sceneCharacterMatrix2 <- sceneCharacterCount2 %>% acast(raw~scene, fun.aggregate = length)
normMatrix2 <- sceneCharacterMatrix2 / rowSums(sceneCharacterMatrix2)
characterCluster2 <- hclust(dist(normMatrix2, method="manhattan"))
plot(characterCluster2)

# ---------------LOTR 3
raw3 <- readLines("LOTR3.txt")
lines3 <- readToLinesFn(raw3)
characterScenes3 <- parseToCharactersFn(lines3)
lines3$raw <- trimws(lines3$raw)
characterList3 <- unique(characterScenes3$raw)
# manual removal of unwanted/wrong elements
unwanted3 <- c(characterList3[1], characterList3[11], characterList3[17], characterList3[21],
               characterList3[23], characterList3[24], characterList3[26], characterList3[27],
               characterList3[28], characterList3[30], characterList3[31], characterList3[32],
               characterList3[33], characterList3[34], characterList3[35], characterList3[36],
               characterList3[37], characterList3[38], characterList3[39], characterList3[40],
               characterList3[41], characterList3[43], characterList3[14])
characterList3 <- characterList3[! characterList3 %in% unwanted3]
characterScenes3 <- characterScenes3 %>% filter(raw %in% characterList3)
rm(unwanted3)

# Change WITCH-KING to WITCH KING (same person)
sameNameFn3 <- function(x){
  gsub("WITCH-KING", "WITCH KING", x)
}
characterScenes3$raw <- sapply(characterScenes3$raw, sameNameFn3)

sceneCharacterCount3 <- characterScenes3 %>% count(scene,raw)
sceneCharacterMatrix3 <- sceneCharacterCount3 %>% acast(raw~scene, fun.aggregate = length)
normMatrix3 <- sceneCharacterMatrix3 / rowSums(sceneCharacterMatrix3)
characterCluster3 <- hclust(dist(normMatrix3, method="manhattan"))
plot(characterCluster3)

# ---------------LOTR all 3
totalSCC <- rbind(sceneCharacterCount, sceneCharacterCount2, sceneCharacterCount3)
end1 <- nrow(sceneCharacterCount)
end2 <- nrow(sceneCharacterCount) + nrow(sceneCharacterCount2)
totalSCC[(end1+1):end2,1] <- sapply(totalSCC[(end1+1):end2,1], function(x) {x+1000})
totalSCC[(end2+1):nrow(totalSCC),1] <- sapply(totalSCC[(end2+1):nrow(totalSCC),1], function(x) {x+2000})


totalSCCMatrix <- totalSCC %>% acast(raw~scene, fun.aggregate = length)
normMatrixTotal <- totalSCCMatrix / rowSums(totalSCCMatrix)
characterClusterTotal <- hclust(dist(normMatrixTotal, method="manhattan"))
plot(characterClusterTotal)
ordering <- characterClusterTotal$labels[characterClusterTotal$order]

# Visualize timeline of all scenes
scenes <- totalSCC %>% filter(n() > 1) %>% ungroup() %>%
  mutate(scene = as.numeric(factor(scene)), character=factor(raw, levels=ordering))

ggplot(scenes, aes(scene, character)) + geom_point() + geom_path(aes(group=scene))

corr <- totalSCCMatrix %*% t(totalSCCMatrix)
heatmap(corr)

library(igraph)
g <- graph.adjacency(corr,weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight)