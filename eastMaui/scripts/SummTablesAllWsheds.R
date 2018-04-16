# summary tables for every watersheds - SCRATCH

library(readxl)
library(tidyverse)
path = "~/Programming/Trutta/HSHEP/EMaui/"
path0 = paste(path, 'Catch2WshedWorkCopy.xlsx', sep = "")
basins = read_excel(path0, sheet = 3)

# nodes = nodes

addDitches = read_excel(paste(path, 'fixNodesDitches.xlsx', sep = ""), sheet = 1)

View(addDitches)


colnames(addDitches)[3]<- 'nodeID'
addDitches$nodeID = as.integer(addDitches$nodeID)
colnames(nodes)[1] <- 'nodeID'

dsNodes = left_join(addDitches, nodes, by = "nodeID")
colnames(dsNodes)[2]<- 'BASINID'

newInput = left_join(basins, dsNodes, by = "BASINID")
View(newInput)
colnames(newInput)[26] <- 'dsNodeID'
writexl::write_xlsx(newInput, "newInput.xlsx")

groupedBasins = read_excel(paste(path, 'EMpackage/eastMaui/rawData/BasinsAllGroups.xlsx', sep = ""))
devtools::use_data(groupedBasins)
# dat %>% group_by(WshedID) %>%

