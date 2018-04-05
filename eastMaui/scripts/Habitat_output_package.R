# Data input/manipulation
# Code outside of eastMaui Package

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
# Run convertMatrix.R
matrices = convertMatrix(nodes, watersheds)

# output generation

# --------------------------------------------------------------------------------------------------
# Run scenarios

scenario.current = basin.fun(matrices$nodes, matrices$inputs)

scenario.1 = basin.fun(nodes1.mat, matrices$inputs)

scenario.0 = basin.fun(nodes0.mat, matrices$inputs)

# --------------------------------------------------------------------------------------------------
# append outputs

scenario.c = as.data.frame(t(scenario.current))
scenario.1t = as.data.frame(t(scenario.1))
scenario.0t = as.data.frame(t(scenario.0))

scenario.c = tibble::rownames_to_column(scenario.c, var = "rowname")


colnames(scenario.1t) = paste("1", colnames(scenario.1t), sep = "_")
colnames(scenario.0t) = paste("0", colnames(scenario.0t), sep = "_")

scenario.test = bind_cols(scenario.c, scenario.0t) %>%
  bind_cols(scenario.1t) %>%
  separate(rowname, into = c("E", "Wshed"), sep = "^([E])")

# match basinID to watershed

colnames(scenario.test)[1] = 'Wshed'
colnames(scenario.test)[2] = "BASINID"

scenario.test$BASINID = as.numeric(scenario.test$BASINID)
scenario.test$Wshed = as.numeric(scenario.test$Wshed)
wsheds = watersheds[ ,c(1,2)]

scenario.test = left_join(scenario.test, wsheds, by = "BASINID")
wsheds = scenario.test[ , 27]
scenario.test = scenario.test[ , -27]
scenario.test$Wshed = wsheds
# write_xlsx(scenario.test, "scenario_test.xlsx")

# Watershed totals for each scenario
for(i in 1:nrow(scenario.test)){
  scenario.test$Wshed.total[i] = sum(scenario.test[i, c(3:10)])
}
for(i in 1:nrow(scenario.test)){
  scenario.test$Wshed.total1[i] = sum(scenario.test[i ,c(19:26)])
}
for(i in 1:nrow(scenario.test)){
  scenario.test$Wshed.total0[i] = sum(scenario.test[i ,c(11:18)])
}

# generate new ID variables
scenario.test$IDx1 = sample(1:3, nrow(scenario.test), replace = TRUE)
scenario.test$IDx2 = sample(1:5, nrow(scenario.test), replace = TRUE)

scenario.test = scenario.test[ , c(1,2,30,31, 3:29)]
# write_xlsx(scenario.test, "scenario.test1.xlsx")
# --------------------------------------------------------------------------------------------------
# tables and plots
# output = summary of habitat by watershed and by species
WshedID = unique(scenario.test$Wshed)
WshedCount = c(1:n_distinct(WshedID))
summaries = tibble(WshedID, WshedCount)
species.abbvs = c("AB", "AS", "ES", "LC", "MG", "NG", "SH", "SS")

output = scenario.test %>%
  group_by(Wshed) %>%      #LINE to change GROUP ID var
  summarise_at(vars(c(4:27)), sum) #Have to change columns depending on ID variable to summarise

# write_xlsx(output, "WshedSummary.xlsx")
# *******************************************
# calculate percent change from output

label = output[, 1]
FullDiv = round(output[ ,c(10:17)], 2)
# original = round(original, 2)
percentChangeFrozen = ((output[ ,c(2:9)]- output[ ,c(10:17)])/output[ ,c(10:17)])*100
percentChangeNoDiv = ((output[ ,c(18:25)]- output[ ,c(10:17)])/output[ ,c(10:17)])*100

scenario.changes = bind_cols(label,FullDiv,percentChangeFrozen, percentChangeNoDiv)
scenario.changes[is.na(scenario.changes)]<- 0

# *******************************************
# summaries by IDvar
VarID = c(1:n_distinct(scenario.changes))
totalHab.FullDiv = rowSums(FullDiv)
totalHab.FullDiv[is.na(totalHab.FullDiv)]<- 0
totalHab.frozen = rowSums(output[ ,c(2:9)])
totalHab.frozen[is.na(totalHab.frozen)]<- 0
totalHab.NoDiv = rowSums(output[ ,c(18:25)])
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
summPlotPath1 = paste0(getwd(), "/testOutput/", "BaselineSummary.jpeg")
summPlotPath2 = paste0(getwd(), "/testOutput/", "ScenarioTestSummary.jpeg")

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
# *******************************************

# summaries for individual species
species.abbvs = c("AB", "AS", "ES", "LC", "MG", "NG", "SH", "SS")

AB = scenario.changes[ ,c(1,2,10,18)]
# colnames(AB)<-c("VarID", "Frozen", "Sugar/FullDiv", "NoDiv")
AS = scenario.changes[ ,c(1,3,11,19)]
ES = scenario.changes[ ,c(1,4,12,20)]
LC = scenario.changes[ ,c(1,5,13,21)]
MG = scenario.changes[ ,c(1,6,14,22)]
NG = scenario.changes[ ,c(1,7,15,23)]
SH = scenario.changes[ ,c(1,8,16,24)]
SS = scenario.changes[ ,c(1,9,17,25)]
species.list = list(AB,AS,ES,LC,MG,NG,SH,SS)
species.plots = list()

for(i in 1:length(species.list)){
  colnames(species.list[[i]])<- c("VarID", "Frozen", "Sugar/FullDiv", "NoDiv")

  species.list[[i]][,5] = ((species.list[[i]][ ,2]-species.list[[i]][,3])/species.list[[i]][,3])*100
  species.list[[i]][,6] = ((species.list[[i]][ ,4]-species.list[[i]][,3])/species.list[[i]][,3])*100
  colnames(species.list[[i]][ ,5:6])<-c("PercentChangeFrozen", "PercentChangeNoDiv")
  species.list[[i]][is.na(species.list[[i]])]<- 0
  dat.long = gather(species.list[[i]], "Frozen.1", "NoDiv.1", key = "NodeValue", value = "PercentChange")
  dat.long$lPercentChange = log(dat.long$PercentChange)
  dat.long$lPercentChange[is.na(dat.long$lPercentChange)]<- 0
  dat.long$lPercentChange[is.infinite(dat.long$lPercentChange)]<- 0

  g.sp = ggplot(dat.long, aes(x = VarID, y = lPercentChange, fill = NodeValue )) +
    geom_bar( stat = 'identity', position = 'dodge') +
    theme_classic() +
    ggtitle(paste0("Species: ", species.abbvs[i], "; Changes in: ", colnames(scenario.changes)[1], sep = " "))
  species.plots[[i]] <- g.sp

  plot.path = paste0(getwd(), "/testOutput/", species.abbvs[i], "plots.jpeg")
  jpeg(file = plot.path)
  plot(g.sp)
  dev.off()

}

# **********************************************************
# summary tables for individual species


species.abbvs = c("AB", "AS", "ES", "LC", "MG", "NG", "SH", "SS")

AB = scenario.changes[ ,c(1,2,10,18)]
AS = scenario.changes[ ,c(1,3,11,19)]
ES = scenario.changes[ ,c(1,4,12,20)]
LC = scenario.changes[ ,c(1,5,13,21)]
MG = scenario.changes[ ,c(1,6,14,22)]
NG = scenario.changes[ ,c(1,7,15,23)]
SH = scenario.changes[ ,c(1,8,16,24)]
SS = scenario.changes[ ,c(1,9,17,25)]
species.list = list(AB,AS,ES,LC,MG,NG,SH,SS)
species.tabs = list()

for(i in 1:length(species.list)){
  colnames(species.list[[i]])<- c("VarID", "Frozen", "Sugar/FullDiv", "NoDiv")

  species.list[[i]][,5] = ((species.list[[i]][ ,2]-species.list[[i]][,3])/species.list[[i]][,3])*100
  species.list[[i]][,6] = ((species.list[[i]][ ,4]-species.list[[i]][,3])/species.list[[i]][,3])*100
  colnames(species.list[[i]][ ,5:6])<-c("PercentChangeFrozen", "PercentChangeNoDiv")
  species.list[[i]][is.na(species.list[[i]])]<- 0
  dat.long = gather(species.list[[i]], "Frozen.1", "NoDiv.1", key = "NodeValue", value = "PercentChange")
  dat.long$lPercentChange = log(dat.long$PercentChange)
  dat.long$lPercentChange[is.na(dat.long$lPercentChange)]<- 0
  dat.long$lPercentChange[is.infinite(dat.long$lPercentChange)]<- 0

  # ****************************************************
  # summary tables for each species
  IDvar.tab = formattable(species.list[[i]], list(
    PercentChangeFrozen= color_tile("white", "lightblue"),
    PercentChangeNoDiv= color_tile("white", "lightblue")
  ))
  species.tabs[[i]]<- IDvar.tab
  colnames(IDvar.tab)[5:6]<-c("PercentChangeFrozen", "PercentChangeNoDiv")
  IDvar.tab$PercentChangeFrozen= percent(IDvar.tab$PercentChangeFrozen)
  IDvar.tab$PercentChangeNoDiv= percent(IDvar.tab$PercentChangeNoDiv)
  # print to jpeg in wd()
  # webshot::install_phantomjs()
  table.path= paste0(getwd(), "/testOutput/",species.abbvs[i], "tab.jpeg")
  export_formattable(IDvar.tab, table.path)

}
