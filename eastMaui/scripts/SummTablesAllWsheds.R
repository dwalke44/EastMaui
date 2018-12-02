# summary tables for every watersheds - SCRATCH

library(readxl)
library(tidyverse)
library(eastMaui)
path = "~/Programming/Trutta/HSHEP/EMaui/EMpackage/eastMaui/rawData"
inputs1 = read_excel(paste(path, "/EMauiAllData.xlsx", sep = ""), sheet = 3)
nodes1 = read_excel(paste(path, "/EMauiAllData.xlsx", sep = ""), sheet = 4)
inputs2 = read_excel(paste(path,"/WatershedAndBasins_wGroups.xlsx", sep = ""))
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

devtools::use_data(inputs1, overwrite = TRUE)
devtools::use_data(inputs2)
devtools::use_data(nodes1, overwrite = TRUE)
