# summary tables for every watersheds - SCRATCH

library(readxl)
library(tidyverse)
library(eastMaui)
path = "~/Programming/Trutta/HSHEP/EMaui/EMpackage/eastMaui/rawData"
inputs = read_excel(paste(path, "/EMauiAllData.xlsx", sep = ""), sheet = 3)
nodes = read_excel(paste(path, "/EMauiAllData.xlsx", sep = ""), sheet = 4)

addDitches = read_excel('~/Programming/Trutta/HSHEP/Emaui/fixNodesDitches.xlsx', sheet = 1)
# --------------------------------------------------------------------
# add grouping data to new basin input for EM package
View(addDitches)
colnames(addDitches)[3]<- 'nodeID'
addDitches$nodeID = as.integer(addDitches$nodeID)
colnames(nodes)[1] <- 'nodeID'

dsNodes = left_join(addDitches, nodes, by = "nodeID")
colnames(dsNodes)[2]<- 'BASINID'

newInput = left_join(basins, dsNodes, by = "BASINID")
View(newInput)
colnames(newInput)[26] <- 'dsNodeID'
# writexl::write_xlsx(newInput, "newInput.xlsx")
groupedBasins = read_excel(paste(path, 'EMpackage/eastMaui/rawData/BasinsAllGroups.xlsx', sep = ""))
# devtools::use_data(groupedBasins)
# ---------------------------------------------------------------------------------

# fix springs on nodes, replace input rain with BFQ
# using new inputs from DrJP - 17 April 2018

devtools::use_data(inputs, overwrite = TRUE)

devtools::use_data(nodes, overwrite = TRUE)
