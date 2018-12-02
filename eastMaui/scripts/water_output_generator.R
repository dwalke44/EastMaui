library(openxlsx)

# ---------------------------------------------------
# prep inputs for export to final workbook
waterOutput = watersheds[ ,-c(17:24)]
waterOutput = waterOutput[ ,c(1:2, 17:21, 3:16)]

nodespath = "~/Programming/Trutta/HSHEP/EMaui/EMpackage/eastMaui/rawData"
nodesOutput = read_excel(paste(nodespath, "/nodes_for_output.xlsx", sep = ""))

dsNodes = read_excel(paste(nodespath, "/BasinsAllGroups.xlsx", sep = ""))
dsNodes = dsNodes[ ,c(1,2,25)]

# --------------------------------------------------
# define terms
waterInput = waterInput.mat

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

add271 = c(0,271,0,0,0)
names(add271) = c("E", "Basin", "mixed", "all0", "all1")
instream = rbind(instream, add271)
add281 = c(0,281,0,0,0)
names(add281) = c("E", "Basin", "mixed", "all0", "all1")
instream = rbind(instream, add281)

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
# results
output_list = list("watershedsInput" = waterOutput,
                   "diversionsInput" = nodesOutput,
                   "WshedCheck" = check,
                   "inStream" = instream,
                   "inWshed"= inwshed,
                   "inDitch" = inditch,
                   "springs" = springs,
                   "sinks" = sinks)
file = paste(path, "water_outputs.0.4.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(output_list, file = file)

# ---------------------------------------------------------
# key
waterOutput %>% distinct( WshedID, Wshed_Name ) -> wshedNames

colnames(nodesOutput)[1]<- "NodeID"
nodesInfo = nodesOutput[, c(1,4,6,7)]
inditch %>% left_join(nodesInfo, by = "NodeID") -> nodesOutput0
nodesOutput0 = nodesOutput0[which(nodesOutput0$DiversionID %in% c(1:10)), ]
writexl::write_xlsx(nodesOutput0, path = paste(path, "/nodes_output_summ.0.1.xlsx", sep = ""))

nodesOutput0 %>%
  group_by(DiversionID) %>%
  summarise_at(vars(c(2:4)), sum) -> nodesOutput1

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
                "Nodes" = nodesOutput1,
                "leases" = leases,
                "taro" = taro)
file = paste(path, "water_outputs_key.0.2.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(key_list, file = file)

# -------------------------------------------------------------

# divs = c(1:10)
# divOnly = nodesOutput[which(nodesOutput$DiversionID %in% divs),]
# nonDiv = nodesOutput[which(nodesOutput$DiversionID %in% c(11:14)), ]


# -------------------------------------------------------------
# Habitat output - test of 10 wshed
poutput.df = as.data.frame(poutput)
poutput.df = as.data.frame(t(poutput.df))

wphabAll.df = as.data.frame(wphabAll)
wphabAll.df = as.data.frame(t(wphabAll.df))

wphabAB.df = as.data.frame(wphabAB)
wphabAB.df = as.data.frame(t(wphabAB.df))

WshedHabAllsp.df = as.data.frame(WshedHabAllsp)
WshedHabAllsp.df = as.data.frame(t(WshedHabAllsp.df))

WshedHabABsp.df = as.data.frame(WshedHabABsp)
WshedHabABsp.df = as.data.frame(t(WshedHabABsp.df))

WshedHabAll.df = bind_cols(WshedHabAllsp.df, WshedHabABsp.df)

houtput_list = list("pOutput" = poutput.df,
                    "WshedHabAllsp" = WshedHabAll.df,
                    "wphabAll" = wphabAll.df,
                    "wphabAB" = wphabAB.df)
file = paste(path, "firstTenWsheds.0.2.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(houtput_list, file = file)
