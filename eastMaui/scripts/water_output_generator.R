library(openxlsx)

waterInput = waterInput.mat

test1 = basinWater.fun(nodes.mat, waterInput.mat)
test2 = basinWater.fun(nodes0.mat, waterInput.mat)
test3 = basinWater.fun(nodes1.mat, waterInput.mat)

sets = c("inStream", "inWshed", "inDitch", "springs", "sinks", "WshedCheck")

path = paste(getwd(), "/outputs/", sep = "")
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
instream = instream[ ,-1]
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

output_list = list("inWshed"= inwshed,
                   "inDitch" = inditch,
                   "WshedCheck" = check,
                   "springs" = springs,
                   "sinks" = sinks)
file = paste(path, "water_outputs.xlsx", sep = "")
# Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe")
write.xlsx(output_list, file = file)

