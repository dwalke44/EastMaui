# summByDiversion = total water in each diversion
# wshedNames = wshedID + wshedName
# leases = lease name + mixed all0 all1
# taro = same as leases
colnames(inwshed)[1] = "WshedID"
wshedsOutput = left_join(wshedNames, inwshed, by = "WshedID")
wshedsOutput = wshedsOutput[ , c(1,2,5,4,3)]
colnames(wshedsOutput) = c("WatershedID", "Watershed Name", "natural.CFS",
                           "fullDiv.CFS", "mixed.CFS")
wshedsOutput$natural.mgd = wshedsOutput$natural.CFS*0.646317
wshedsOutput$fullDiv.mgd = wshedsOutput$fullDiv.CFS*0.646317
wshedsOutput$mixed.mgd = wshedsOutput$mixed.CFS*0.646317

leasesOutput= leases[ ,c(1,4,3,2)]
colnames(leasesOutput) = c("Lease Name", "natural.CFS", "fullDiv.CFS", "mixed.CFS")
leasesOutput$natural.mgd =leasesOutput$natural.CFS*0.646317
leasesOutput$fullDiv.mgd = leasesOutput$fullDiv.CFS*0.646317
leasesOutput$mixed.mgd = leasesOutput$mixed.CFS*0.646317

taroOutput= taro[ ,c(1,4,3,2)]
colnames(taroOutput) = c("Taro", "natural.CFS", "fullDiv.CFS", "mixed.CFS")
taroOutput$natural.mgd =taroOutput$natural.CFS*0.646317
taroOutput$fullDiv.mgd = taroOutput$fullDiv.CFS*0.646317
taroOutput$mixed.mgd = taroOutput$mixed.CFS*0.646317

colnames(nodesOutput0)[2:4] = c("mixed.CFS", "fullDiv.CFS", "natural.CFS")

summ = left_join(summ, wshedNames, by = "WshedID")
summh = summ[ ,c(1,8,2,4,6)]
colnames(summh)[c(1,2)]<- c("Watershed ID", "Watershed Name")
summh = summh[ ,c(1,2,3,5,4)]
totalOutput = list("WaterInDiversion" = summByDiversion,
                   "WaterInWatersheds" = wshedsOutput,
                   "WaterInNodes" = nodesOutput0,
                   "WaterByLease" = leasesOutput,
                   "taro" = taroOutput,

                   "HabitatInWatersheds" = summh,
                   "AllSpeciesHab.nat.wsheds" = AllSpHab.nat,
                   "AllSpHab.fullDiv.wsheds" = AllSpHab.mix,
                   "AllSpHab.mixed.wsheds" = AllSpHab.0)
file = paste(outpath, "total_output.0.2.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(totalOutput, file = file)

# ----------------------------------------------------------------
# total output with 8 scenarios tested

colnames(nodesOutput)[1]<- "NodeID"
nodesInfo = nodesOutput[, c(1,4,6,7)]
inditch.mgd %>% left_join(nodesInfo, by = "NodeID") -> nodesOutput0
nodesOutput0 = nodesOutput0[which(nodesOutput0$DiversionID %in% c(1:10)), ]

nodesOutput0 %>%
  group_by(DiversionID) %>%
  summarise_at(vars(c(2:9)), sum)  %>%
  left_join(ditchnames, by = "DiversionID")-> summByDiversion
summByDiversion = summByDiversion[ ,c(1,10,2:9)]

inwshedOut = inwshed[ ,-1]
WshedID = inwshed[ ,1]
inwshedOut.mgd = inwshedOut*0.646317
inwshedOut.mgd$WshedID = WshedID
wshedsOutput = left_join(wshedNames, inwshedOut.mgd, by = "WshedID")

leasesByID = waterOutput[ ,c(2,4,7)]
colnames(dsNodes)[3]<- "NodeID"
leases = left_join(dsNodes, leasesByID, by = "BASINID") %>%
  left_join(inditch, by = "NodeID") %>%
  filter(NodeID<45 | NodeID>54) %>%
  group_by(Lease_ID)
leases = leases[complete.cases(leases), ]
leases = leases %>% summarise_at(vars(c(5:12)), sum)

taro = left_join(dsNodes, leasesByID, by = "BASINID") %>%
  left_join(inditch, by = "NodeID") %>%
  filter(NodeID<45 | NodeID>54) %>%
  group_by(Taro_ID)
taro = taro[complete.cases(taro),]
taro = taro %>% summarise_at(vars(c(5:12)), sum)

wshed.hab.total = data.frame(AllSpHab.nat[ ,9],
                             AllSpHab.sugar[ ,9],
                             AllSpHab.IIFS[ ,9],
                             AllSpHab.IIFSplus[ ,9],
                             AllSpHab.mix[ ,9],
                             AllSpHab.mix1[ ,9],
                             AllSpHab.mix2[ ,9],
                             AllSpHab.mix3[ ,9])
wshed.hab.total$WatershedName = wshedNames$Wshed_Name
wshed.hab.total$WshedID = wshedNames$WshedID

colnames(wshed.hab.total)[1:8]<- c("natural", "sugar", "IIFS2008", "IIFS2008plus", "mixed","mixed1","mixed2","mixed3")
wshed.hab.total = wshed.hab.total[ ,c(10,9, 1:8)]

totalOutput8 = list("WaterInDiversion" = summByDiversion,
                   "WaterInWatersheds" = wshedsOutput,
                   "WaterInBasins" = instreamOut,
                   "WaterInNodes" = nodesOutput0,
                   "WaterByLease" = leases,
                   "WaterInTaro" = taro,

                   "HabitatInWatersheds" = wshed.hab.total,
                   "HabitatInBasins" = )
file = paste(outpath, "total_output.0.2.xlsx", sep = "")
Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
openxlsx::write.xlsx(totalOutput, file = file)

