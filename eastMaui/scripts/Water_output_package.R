# create water inputs
setwd("~/Programming/Trutta/HSHEP/EMaui")

library(eastMaui)
library(tidyverse)
library(readxl)
library(writexl)
library(gridExtra)
library(formattable)
library(webshot)
library(htmltools)

watersheds = inputs
watersheds = watersheds[ ,-16]

watershedsWaterInput = watersheds[ ,c(1,2,7)]
waterInput.mat= as.matrix(watershedsWaterInput)

nodes = nodes
nodes = nodes[ ,c(1, 8:18)]
nodes0 = nodes[1, ]
nodes = nodes[-1, ]
nodes = rbind(nodes, nodes0)
nodes.mat = as.matrix(nodes[ ,-2])
# --------------------------------------------------------------------------------------------------
# Create nodes.mat for all values = 1

nodes1 = nodes
nodes1[ , c(3:12)] <- 1
# View(nodes1)

nodes1.mat = as.matrix(nodes1[ ,-2])
row.names(nodes1.mat)<- unlist(nodes1[ ,1])
nodes1.mat[ , c(3:11)] = as.numeric(nodes1.mat[ , c(3:11)] )
# View(nodes1.mat)
# --------------------------------------------------------------------------------------------------
# Create nodes.mat for all values = 0

nodes0 = nodes
nodes0[which(nodes0$NodeType == "MinorDiversion"), c(3:12)] <- 0
nodes0[which(nodes0$NodeType == "MajorDiversion"), c(3:12)] <- 0
nodes0.mat = as.matrix(nodes0[ ,-2])
row.names(nodes0.mat)<-unlist(nodes0[ ,1])
nodes0.mat[ , c(3:11)] = as.numeric(nodes0.mat[ , c(3:11)] )
# View(nodes0.mat)
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# Run scenarios
# waterInput = waterInput.mat

scenario.current = diversion.fun(nodes.mat, waterInput.mat)

scenario.1 = diversion.fun(nodes1.mat, waterInput.mat)

scenario.0 = water.fun(nodes0.mat, waterInput.mat)

# --------------------------------------------------------------------------------------------------
# append outputs
scenario.current = scenario.current[1, ]
scenario.1 = scenario.1[1,]
scenario.0 = scenario.0[1,]

scenario.c = as.data.frame(t(scenario.current))
scenario.1t = as.data.frame(t(scenario.1))
scenario.0t = as.data.frame(t(scenario.0))

scenario.c = tibble::rownames_to_column(scenario.c, var = "rowname")
scenario.0t = rownames_to_column(scenario.0t)

colnames(scenario.1t) = "All_1"
colnames(scenario.0t) = "All_0"

scenario.test = merge(scenario.c, scenario.0t, by = "rowname" ) %>%
  merge(scenario.1t) %>%
  separate(rowname, into = c("E", "Wshed"), sep = "^([E])")

# match basinID to watershed

colnames(scenario.test)[1] = 'Wshed'
colnames(scenario.test)[2] = "BASINID"
colnames(scenario.test)[3] = "current"
scenario.test$BASINID = as.numeric(scenario.test$BASINID)
scenario.test$Wshed = as.numeric(scenario.test$Wshed)
wsheds = watersheds[ ,c(1,2)]

scenario.test = left_join(scenario.test, wsheds, by = "BASINID")
scenario.test = scenario.test[ ,-1]
scenario.test = scenario.test[ ,c(5,1:4)]

# generate new ID variables
scenario.test$IDx1 = sample(1:3, nrow(scenario.test), replace = TRUE)
scenario.test$IDx2 = sample(1:5, nrow(scenario.test), replace = TRUE)
scenario.test = scenario.test[ ,c(1:2,6:7, 3:5)]
# scenario.test = scenario.test[ , c(1,2,30,31, 3:29)]
# write_xlsx(scenario.test, "scenario.test1.xlsx")
# --------------------------------------------------------------------------------------------------
# tables and plots
# output = summary of habitat by watershed and by species

output = scenario.test %>%
  group_by(BASINID) %>%      #LINE to change GROUP ID var
  summarise_at(vars(c(4:6)), sum)
# write_xlsx(output, "WshedSummary.xlsx")

# *******************************************
# calculate percent change from output
label = output[ ,1]
FullDiv = output[ ,3]
colnames(FullDiv)[1] = "FullDiversion"
percentChangeFrozen = ((output[ ,c(2)]- output[ ,c(3)])/output[ ,c(3)])*100
percentChangeNoDiv = ((output[ ,c(4)]- output[ ,c(3)])/output[ ,c(3)])*100

scenario.changes = bind_cols(label,FullDiv,percentChangeFrozen, percentChangeNoDiv)
scenario.changes[is.na(scenario.changes)]<- 0
colnames(scenario.changes)[3]<- c("pChangeFrozen")
colnames(scenario.changes)[4]<- c("pChangeNoDiv")

# *******************************************
# summaries by IDvar
VarID = c(1:n_distinct(scenario.changes))
totalHab.FullDiv = rowSums(FullDiv)
totalHab.FullDiv[is.na(totalHab.FullDiv)]<- 0
totalHab.frozen = rowSums(output[ ,c(2)])
totalHab.frozen[is.na(totalHab.frozen)]<- 0
totalHab.NoDiv = rowSums(output[ ,c(4)])
totalHab.NoDiv[is.na(totalHab.NoDiv)]<- 0
IDvar = data.frame(VarID, totalHab.FullDiv, totalHab.frozen, totalHab.NoDiv)

# totalHab.0 = full diversion/sugar production scenario = baseline for comparison
IDvar$percentDelta.frozen = ((IDvar[, 3]-IDvar[ ,2])/IDvar[,2])*100
IDvar$percentDelta.frozen[is.na(IDvar$percentDelta.frozen)]<- 0
IDvar$percentDelta1 = ((IDvar[, 4]-IDvar[ ,2])/IDvar[,2])*100
IDvar$percentDelta1[is.na(IDvar$percentDelta1)]<- 0
IDvar$percentDelta.frozen[is.infinite(IDvar$percentDelta.frozen)]<- 0
IDvar$percentDelta1[is.infinite(IDvar$percentDelta1)]<- 0
# reshape data to plot percent delta

IDvar.long = gather(IDvar, `percentDelta.frozen`, `percentDelta1`,  key = "NodeValue", value = "PercentChange")
IDvar.long$PercentChange[is.infinite(IDvar.long$PercentChange)]<- 0
IDvar.long$lPercentChange = log(IDvar.long$PercentChange)
IDvar.long$lPercentChange[is.infinite(IDvar.long$lPercentChange)]<- 0
IDvar.long$lPercentChange[is.infinite(IDvar.long$lPercentChange)]<- 0


# barplot
colnames(IDvar)<-c("IDvar", "Sugar/Full Div", "Frozen", "No Div", "pChange Frozen", "pChange No Div")

g1 = ggplot(IDvar, aes(x = VarID, y = log(`Sugar/Full Div`))) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "log(Sugar/Full Div)") +
  theme_classic()+
  ggtitle("ID variable summary - Baseline")



g2 = ggplot(IDvar.long, aes(x = VarID, y = lPercentChange, fill = NodeValue)) +
  geom_bar( stat = 'identity', position = 'dodge') +
  ylim(0, 1.2*max(IDvar.long$lPercentChange)) +
  labs(y = "log(PercentChange)") +
  theme_classic() +
  ggtitle("IDvariable summary - New Scenarios")
# Export summary bar plots for all varID's
summPlotPath1 = paste0(getwd(), "/testOutput/", "Diversions_BaselineSummary.jpeg")
summPlotPath2 = paste0(getwd(), "/testOutput/", "Diversions_ScenarioTestSummary.jpeg")

jpeg(file = summPlotPath1)
plot(g1)
dev.off()
jpeg(file = summPlotPath2)
plot(g2)
dev.off()


# generate summary table as separate jpeg

colnames(IDvar)[2:6]<-c("Sugar/Full Div", "Frozen", "No Div", "PercentChangeFrozen", "PercentChangeNoDiv")

IDvar.tab = formattable(IDvar, list(
  PercentChangeFrozen= color_tile("white", "lightblue"),
  PercentChangeNoDiv= color_tile("white", "lightblue")
))
IDvar.tab$PercentChangeFrozen= percent(IDvar.tab$PercentChangeFrozen)
IDvar.tab$PercentChangeNoDiv= percent(IDvar.tab$PercentChangeNoDiv)
# print to jpeg in wd()
# webshot::install_phantomjs()
table.path= paste0(getwd(), '/testOutput/IDvartab.jpeg')
export_formattable(IDvar.tab, table.path)


# *******************************************************************
# *******************************************************************
