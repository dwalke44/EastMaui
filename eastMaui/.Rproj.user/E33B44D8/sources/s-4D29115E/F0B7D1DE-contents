# Code outside of eastMaui Package

setwd("~/Programming/Trutta/HSHEP/EMaui")


library(tidyverse)
library(readxl)
library(writexl)
library(gridExtra)
library(formattable)
library(webshot)
library(htmltools)

watersheds = read_xlsx("Catch2WshedCopy4.xlsx", sheet = 3)
watersheds = watersheds[ ,-16]

watershedsWaterInput = watersheds[ ,c(1,2,7)]
waterInput.mat= as.matrix(watershedsWaterInput)

nodes = read_xlsx("Catch2WshedCopy4.xlsx", sheet = 4)
nodes = nodes[ ,c(1, 8:18)]
nodes0 = nodes[1, ]
nodes = nodes[-1, ]
nodes = rbind(nodes, nodes0)

# ----------------------------------------------------------------------------------
# Create nodes.mat for all values = 1

nodes1 = nodes
nodes1[ , c(3:12)] <- 1
# View(nodes1)

nodes1.mat = as.matrix(nodes1[ ,-2])
row.names(nodes1.mat)<- unlist(nodes1[ ,1])
nodes1.mat[ , c(3:11)] = as.numeric(nodes1.mat[ , c(3:11)] )
# View(nodes1.mat)
# ----------------------------------------------------------------------------------
# Create nodes.mat for all values = 0

nodes0 = nodes
nodes0[which(nodes0$NodeType == "MinorDiversion"), c(3:12)] <- 0
nodes0[which(nodes0$NodeType == "MajorDiversion"), c(3:12)] <- 0
nodes0.mat = as.matrix(nodes0[ ,-2])
row.names(nodes0.mat)<-unlist(nodes0[ ,1])
nodes0.mat[ , c(3:11)] = as.numeric(nodes0.mat[ , c(3:11)] )
# View(nodes0.mat)
#-----------------------------------------------------------------------------------
#
