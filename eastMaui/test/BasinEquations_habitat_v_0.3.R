#' @export

basin.fun = function(nodesInput, wshedInput, waterImpactedHab){
  # wshedInput = amount of habitat for fish/inverts
  # nodesInput = passage value through nodes for connectivity
  # waterImpactedHab = percent change of habitat from natural condition

  poutput = list()
  wphabAll = list()
  wphabAB = list()
  WshedHabAllsp = list()
  WshedHabABsp = list()

# --------------------Watershed 1 -------------------------------------------
  poutput$E014 = wshedInput[14, c(13:20)]*nodesInput[1, 16]

  poutput$E013 = wshedInput[13, c(13:20)]*nodesInput[10, 16]*
    nodesInput[1, 16]

  poutput$E015 =






  wphabAll$a001 = poutput$E001[2:8]*allsp.hab(waterImpactedHab[1])
  wphabAB$a001 = poutput$E001[1]*ab.hab(waterImpactedHab[1])

  wphabAll$a002 = poutput$E002[2:8]*allsp.hab(waterImpactedHab[2])
  wphabAB$a002 = poutput$E002[1]*ab.hab(waterImpactedHab[2])

  wphabAll$a003 = poutput$E003[2:8]*allsp.hab(waterImpactedHab[3])
  wphabAB$a003 = poutput$E003[1]*ab.hab(waterImpactedHab[3])

  wphabAll$a004 = poutput$E004[2:8]*allsp.hab(waterImpactedHab[4])
  wphabAB$a004 = poutput$E004[1]*ab.hab(waterImpactedHab[4])

  wphabAll$a500 = poutput$E500[2:8]*allsp.hab(waterImpactedHab[312])
  wphabAB$a500 = poutput$E500[1]*ab.hab(waterImpactedHab[312])

  WshedHabAllsp$w001 = wphabAll$a001 + wphabAll$a002 + wphabAll$a003 +
    wphabAll$a004 + wphabAll$a500

  WshedHabABsp$w001 = wphabAB$a001+ wphabAB$a002 + wphabAB$a003 +
    wphabAB$a004 + wphabAB$a500

  WshedHabABsp$w007 = wphabAB$a029 + wphabAB$a030 + wphabAB$a031 +
    wphabAB$a032+ wphabAB$a033 + wphabAB$a034

  # -------------------------------------------------------------------
  # Save output in dataframe
  output = list("WsedHabAllSp" = WshedHabAllsp, "WshedHabAB" = WshedHabABsp,
                "WaterPassageHabAllSp" = wphabAll,
                "WaterPassageHabAB" = wphabAB,
                "PassageEffectedHab" = poutput)
  return(output)

}




