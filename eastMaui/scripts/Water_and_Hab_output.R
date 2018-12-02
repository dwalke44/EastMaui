# Step 1. Run first two blocks of input generation code in .rmd file

# Step 2. Run water function.

# ---------------------------------------------------
# Inputs
waterInput = waterInput.mat

# prep inputs for export to final workbook
waterOutput = watersheds[ ,-c(17:24)]
waterOutput = waterOutput[ ,c(1:2, 17:21, 3:16)]

nodespath = "~/Programming/Trutta/HSHEP/EMaui/EMpackage/eastMaui/rawData"
nodesOutput = read_excel(paste(nodespath, "/nodes_for_output.xlsx", sep = ""))

dsNodes = read_excel(paste(nodespath, "/BasinsAllGroups.xlsx", sep = ""))
dsNodes = dsNodes[ ,c(1,2,25)]
sets = c("inStream", "inWshed", "inDitch", "springs", "sinks", "WshedCheck")

path = paste(getwd(), "/outputs/", sep = "")
# ---------------------------------------------------
# Run function w/ different scenarios

test1 = basinWater.fun(nodes.mat, waterInput.mat)
test2 = basinWater.fun(nodes0.mat, waterInput.mat)
test3 = basinWater.fun(nodes1.mat, waterInput.mat)

# ---------------------------------------------------
instream1 = as.data.frame(test1$inStream)
instream2 = as.data.frame(test2$inStream)
instream3 = as.data.frame(test3$inStream)

instream = bind_rows(instream1, instream2, instream3)
instream = t(instream)
colnames(instream)<-c("mixed", "all0", "all1")
instream = as.data.frame(instream) %>%
  rownames_to_column(var = "basinID") %>%
  separate(basinID, into = c("E", "Basin"), sep = "^([E])")
instream$Basin = as.numeric(instream$Basin)
instream = instream[-53,]
instream[75, 2] = 77

add206 = c(0,206,0,0,0)
names(add206) = c("E", "Basin", "mixed", "all0", "all1")
instream = rbind(instream, add206)

# add271 = c(0,271,0,0,0)
# names(add271) = c("E", "Basin", "mixed", "all0", "all1")
# instream = rbind(instream, add271)
# add281 = c(0,281,0,0,0)
# names(add281) = c("E", "Basin", "mixed", "all0", "all1")
# instream = rbind(instream, add281)

# ---- habitat calculation inputs----------------------
instreamh = instream[order(instream$Basin), -1]
instreamh = remove_rownames(instreamh) %>%
  column_to_rownames(var = "Basin")
instreamh.mat = data.matrix(instreamh)

instreamh.nat = instreamh.mat[ ,3]

instreamh.0 = instreamh.mat[ ,2]
instreamh.mix = instreamh.mat[ ,1]

waterImpactedHab.nat = (instreamh.nat/instreamh.nat)*100
waterImpactedHab.0 = (instreamh.0/instreamh.nat)*100
waterImpactedHab.mix = (instreamh.mix/instreamh.nat)*100

waterImpactedHab.0[is.na(waterImpactedHab.0)]<- 0
waterImpactedHab.mix[is.na(waterImpactedHab.mix)]<- 0
waterImpactedHab.nat[is.na(waterImpactedHab.nat)]<-0
waterImpactedHab = waterImpactedHab.mix

# -----------------------------------------------------
inwshed1 = as.data.frame(test1$inWshed)
inwshed2 = as.data.frame(test2$inWshed)
inwshed3 = as.data.frame(test3$inWshed)

inwshed = bind_rows(inwshed1, inwshed2, inwshed3)
inwshed = t(inwshed)
colnames(inwshed)<-c("mixed", "all0", "all1")
inwshed = as.data.frame(inwshed) %>%
  rownames_to_column(var = "wshedID") %>%
  separate(wshedID, into= c("w", "Wshed"), sep = "^([w])")
inwshed$Wshed = as.numeric(inwshed$Wshed)
inwshed = inwshed[, -1]
# -----------------------------------------------------
inditch1 = as.data.frame(test1$inDitch)
inditch2 = as.data.frame(test2$inDitch)
inditch3 = as.data.frame(test3$inDitch)

inditch = bind_rows(inditch1, inditch2, inditch3)
inditch = t(inditch)
colnames(inditch)<-c("mixed", "all0", "all1")

inditch = as.data.frame(inditch) %>%
  rownames_to_column(var = "ditchID") %>%
  separate(ditchID, into = c("n", "NodeID"), sep = "^([n])")
inditch$NodeID = as.numeric(inditch$NodeID)
inditch = inditch[ , -1]

# convert to mgd - millions of gallons per day
inditch$mixed.mgd = inditch$mixed*0.646317
inditch$all0.mgd = inditch$all0*0.646317
inditch$all1.mgd = inditch$all1*0.646317
# ---------------------------------------------------
wshedcheck1 = as.data.frame(test1$WshedCheck)
wshedcheck2 = as.data.frame(test2$WshedCheck)
wshedcheck3 = as.data.frame(test3$WshedCheck)

check = bind_rows(wshedcheck1, wshedcheck2, wshedcheck3)
check = t(check)
colnames(check)<- c("mixed", "all0", "all1")

check = as.data.frame(check) %>%
  rownames_to_column(var = "wshed") %>%
  separate(wshed, into = c("w", "Wshed"), sep = "^([w])")
check$Wshed = as.numeric(check$Wshed)
check = check[, -1]

# ----------------------------------------------

springs1 = as.data.frame(test1$springs)
springs2 = as.data.frame(test2$springs)
springs3 = as.data.frame(test3$springs)

springs = bind_rows(springs1, springs2, springs3)
springs = t(springs)
colnames(springs)<- c("mixed", "all0", "all1")

springs = as.data.frame(springs) %>%
  rownames_to_column(var = "springID") %>%
  separate(springID, into = c("E", "spring_ID"), sep = "^([E])")
springs = springs[ , -1]

# ----------------------------------------------

sinks1 = as.data.frame(test1$sinks)
sinks2 = as.data.frame(test2$sinks)
sinks3 = as.data.frame(test3$sinks)

sinks = bind_rows(sinks1, sinks2, sinks3)
sinks = t(sinks)

colnames(sinks)<- c("mixed", "all0", "all1")

sinks = as.data.frame(sinks) %>%
  rownames_to_column(var = "sinkID") %>%
  separate(sinkID, into = c("E", "sink_ID"), sep = "^([E])")
sinks = sinks[ , -1]

# ---------------------------------------------
# water modeling results
output_list = list("watershedsInput" = waterOutput,
                   "diversionsInput" = nodesOutput,
                   "WshedCheck" = check,
                   "inStream" = instream,
                   "inWshed"= inwshed,
                   "inDitch" = inditch,
                   "springs" = springs,
                   "sinks" = sinks)
file = paste(path, "water_outputs.1.0.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(output_list, file = file)

# --------------------------------------------
# water modeling key
waterOutput %>% distinct( WshedID, Wshed_Name ) -> wshedNames

colnames(nodesOutput)[1]<- "NodeID"
nodesInfo = nodesOutput[, c(1,4,6,7)]
inditch %>% left_join(nodesInfo, by = "NodeID") -> nodesOutput0
nodesOutput0 = nodesOutput0[which(nodesOutput0$DiversionID %in% c(1:10)), ]
writexl::write_xlsx(nodesOutput0, path = paste(path, "/nodes_output_summ.1.0.xlsx", sep = ""))

nodesOutput0 %>%
  group_by(DiversionID) %>%
  summarise_at(vars(c(2:7)), sum) -> summByDiversion

leasesByID = waterOutput[ ,c(2,4,7)]
colnames(dsNodes)[3]<- "NodeID"
leases = left_join(dsNodes, leasesByID, by = "BASINID") %>%
  left_join(inditch, by = "NodeID") %>%
  filter(NodeID<45 | NodeID>54) %>%
  group_by(Lease_ID)
leases = leases[complete.cases(leases), ]
leases = leases %>% summarise_at(vars(c(5:7)), sum)

taro = left_join(dsNodes, leasesByID, by = "BASINID") %>%
  left_join(inditch, by = "NodeID") %>%
  filter(NodeID<45 | NodeID>54) %>%
  group_by(Taro_ID)
taro = taro[complete.cases(taro),]
taro = taro %>% summarise_at(vars(c(5:7)), sum)

key_list = list("Watersheds" = wshedNames,
                "Diversion" = summByDiversion,
                "leases" = leases,
                "taro" = taro)
file = paste(path, "water_outputs_key.1.0.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(key_list, file = file)

# --------------------------------------------
# Habitat Modeling
htest = basin.fun(nodesh.mat, habitatInput.mat, waterImpactedHab.mix)
htest1 = basin.fun(nodes1h.mat, habitatInput.mat, waterImpactedHab.nat)
htest0 = basin.fun(nodes0h.mat, habitatInput.mat, waterImpactedHab.0)


AllSpHab = as.data.frame(htest$WsedHabAllSp)
AllSpHab1 = as.data.frame(htest1$WsedHabAllSp)
AllSpHab0 = as.data.frame(htest0$WsedHabAllSp)
AllSpHabm = as.tibble(t(AllSpHab))
AllSpHab1 = as.tibble(t(AllSpHab1))
AllSpHab0 = as.tibble(t(AllSpHab0))

ABspHab = as.tibble(htest$WshedHabAB)
ABspHab1 = as.tibble(htest1$WshedHabAB)
ABspHab0 = as.tibble(htest0$WshedHabAB)
ABspHabm = as.tibble(t(ABspHab))
ABspHab1 = as.tibble(t(ABspHab1))
ABspHab0 = as.tibble(t(ABspHab0))
colnames(ABspHabm) = "AB_sum"
colnames(ABspHab1) = "AB_sum"
colnames(ABspHab0) = "AB_sum"

AllSpHab.mix = bind_cols(AllSpHabm, ABspHabm)
AllSpHab.mix$total = rowSums(AllSpHab.mix)
AllSpHab.nat = bind_cols(AllSpHab1, ABspHab1) %>%
  rownames_to_column(var = WshedID)
AllSpHab.nat$total = rowSums(AllSpHab.nat)
AllSpHab.0 = bind_cols(AllSpHab0, ABspHab0)
AllSpHab.0$total = rowSums(AllSpHab.0)


summWshed = c(1:45)
natural.total = tibble(summWshed,AllSpHab.nat$total)
colnames(natural.total)<-c("WshedID", "total.habitat")
mix.total = tibble(summWshed,AllSpHab.mix$total)
colnames(mix.total)<-c("WshedID", "total.habitat")
fullDiv.total = tibble(summWshed, AllSpHab.0$total)
colnames(fullDiv.total)<-c("WshedID", "total.habitat")

addDitches = read_excel('~/Programming/Trutta/HSHEP/Emaui/fixNodesDitches.xlsx', sheet = 1)
colnames(addDitches)<- c("WshedID", "BASINID", "NodeID")
basinDivs = left_join(addDitches, inditch, by = "NodeID" )
basinDivs[is.na(basinDivs)]<- 0
basinDivs[6, c(4:9)]= 0
natural.diversion = basinDivs %>%
  group_by(WshedID) %>%
  summarise_at(vars("all1.mgd"), sum)
mixed.diversion = basinDivs %>%
  group_by(WshedID) %>%
  summarise_at(vars("mixed.mgd"), sum)
full.diversion = basinDivs %>%
  group_by(WshedID) %>%
  summarise_at(vars("all0.mgd"), sum)

natural = left_join(natural.total,natural.diversion, by = "WshedID")
colnames(natural)[c(2,3)]<- c("Natural.Habitat.Units", "no.div.mgd")
mixed = left_join(mix.total, mixed.diversion, by = "WshedID")
colnames(mixed)[c(2,3)]<- c("Mixed.Habitat.Units", "mixed.div.mgd")
full = left_join(fullDiv.total, full.diversion, by = "WshedID")
colnames(full)[c(2,3)]<- c("Full.Diversion.Habitat.Units", "full.div.mgd")

summ = left_join(natural, mixed, by = "WshedID") %>% left_join(full, by = "WshedID")

full = bind_cols(mix)
hab_list = list("Summary" = summ,
                "AllSpeciesHabitat.nat" = AllSpHab.nat,
                "AllSpeciesHabitat.mix" = AllSpHab.mix,
                "AllSpeciesHabitat.0" = AllSpHab.0)
file = paste(path, "habitat_outputs.1.0.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(hab_list, file = file)
