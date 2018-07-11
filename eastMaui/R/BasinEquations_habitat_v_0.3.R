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
  poutput$E001 = wshedInput[1, c(13:20)]*nodesInput[39, (16)]

  poutput$E002 = wshedInput[2, c(13:20)]*nodesInput[49, (16)]*
    nodesInput[39, (16)]

  poutput$E003 = wshedInput[3, c(13:20)]*nodesInput[228, (16)]*
    nodesInput[49, (16)]*
    nodesInput[39, (16)]*
    nodesInput[312, 16]

  poutput$E004 = wshedInput[4, c(13:20)]*nodesInput[196, (16)]*
    nodesInput[49, (16)]*
    nodesInput[39, (16)]*
    nodesInput[312, 16]

  poutput$E500 = wshedInput[310, c(13:20)]*nodesInput[312, 16]*
    nodesInput[49, (16)]*
    nodesInput[39, (16)]

  wphabAll$a001 = poutput$E001[2:8]*allsp.hab(waterImpactedHab[1])
  wphabAB$a001 = poutput$E001[1]*ab.hab(waterImpactedHab[1])

  wphabAll$a002 = poutput$E002[2:8]*allsp.hab(waterImpactedHab[2])
  wphabAB$a002 = poutput$E002[1]*ab.hab(waterImpactedHab[2])

  wphabAll$a003 = poutput$E003[2:8]*allsp.hab(waterImpactedHab[3])
  wphabAB$a003 = poutput$E003[1]*ab.hab(waterImpactedHab[3])

  wphabAll$a004 = poutput$E004[2:8]*allsp.hab(waterImpactedHab[4])
  wphabAB$a004 = poutput$E004[1]*ab.hab(waterImpactedHab[4])

  wphabAll$a500 = poutput$E500[2:8]*allsp.hab(waterIMpactedHab[312])
  wphabAB$a500 = poutput$E500[1]*ab.hab(waterImpactedHab[312])

  WshedHabAllsp$w001 = wphabAll$a001 + wphabAll$a002 + wphabAll$a003 +
    wphabAll$a004 + wphabAll$a500

  WshedHabABsp$w001 = wphabAB$a001+ wphabAB$a002 + wphabAB$a003 +
    wphabAB$a004 + wphabAB$a500
  # --------------------Watershed 2 -------------------------------------------
  poutput$E005 = wshedInput[5, c(13:20)]*nodesInput[42, (16)]

  poutput$E006 = wshedInput[6, c(13:20)]*nodesInput[50, (16)]*
    nodesInput[42, (16)]

  poutput$E007 = wshedInput[7, c(13:20)]*nodesInput[229, (16)]*
    nodesInput[50, (16)]*
    nodesInput[42, (16)]

  poutput$E008 = wshedInput[8, c(13:20)]*nodesInput[193, (16)]*
    nodesInput[229, (16)]*
    nodesInput[50, (16)]*
    nodesInput[42, (16)]

  poutput$E009 = wshedInput[9, c(13:20)]*nodesInput[192, (16)] *
    nodesInput[229, (16)]*
    nodesInput[50, (16)]*
    nodesInput[42, (16)]

  poutput$E010 = wshedInput[10, c(13:20)]*nodesInput[194, (16)]*
    nodesInput[229, (16)]*
    nodesInput[50, (16)]*
    nodesInput[42, (16)]


  wphabAll$a005 = poutput$E005[2:8]*allsp.hab(waterImpactedHab[5])
  wphabAB$a005 = poutput$E005[1]*ab.hab(waterImpactedHab[5])
  wphabAll$a006 = poutput$E006[2:8]*allsp.hab(waterImpactedHab[6])
  wphabAB$a006 = poutput$E006[1]*ab.hab(waterImpactedHab[6])
  wphabAll$a007 = poutput$E007[2:8]*allsp.hab(waterImpactedHab[7])
  wphabAB$a007 = poutput$E007[1]*ab.hab(waterImpactedHab[7])
  wphabAll$a008 = poutput$E008[2:8]*allsp.hab(waterImpactedHab[4])
  wphabAB$a008 = poutput$E008[1]*ab.hab(waterImpactedHab[8])
  wphabAll$a009 = poutput$E009[2:8]*allsp.hab(waterImpactedHab[9])
  wphabAB$a009 = poutput$E009[1]*ab.hab(waterImpactedHab[9])
  wphabAll$a010 = poutput$E010[2:8]*allsp.hab(waterImpactedHab[10])
  wphabAB$a010 = poutput$E010[1]*ab.hab(waterImpactedHab[10])


  WshedHabAllsp$w002 = wphabAll$a005 + wphabAll$a006 + wphabAll$a007 +
    wphabAll$a008 + wphabAll$a009 + wphabAll$a010

  WshedHabABsp$w002 = wphabAB$a005+ wphabAB$a006 + wphabAB$a007 +
    wphabAB$a008 + wphabAB$a009 + wphabAB$a010

  # --------------------Watershed 3 -------------------------------------------

  poutput$E011 = wshedInput[11, c(13:20)]*nodesInput[44, (16)]

  poutput$E012 = wshedInput[12, c(13:20)]*nodesInput[51, (16)]*
    nodesInput[44, (16)]

  poutput$E013 = wshedInput[13, c(13:20)]*nodesInput[195, (16)]*
    nodesInput[51, (16)]*
    nodesInput[44, (16)]

  poutput$E014 = wshedInput[14, c(13:20)]*nodesInput[186, (16)]*
    nodesInput[51, (16)]*
    nodesInput[44, (16)]

  poutput$E015 = wshedInput[15, c(13:20)]*nodesInput[187, (16)]*
    nodesInput[188, (16)]*
    nodesInput[195, (16)]*
    nodesInput[51, (16)]*
    nodesInput[44, (16)]

  poutput$E016 = wshedInput[16, c(13:20)]*nodesInput[188, (16)]*
    nodesInput[195, (16)]*
    nodesInput[51, (16)]*
    nodesInput[44, (16)]

  wphabAll$a011 = poutput$E011[2:8]*allsp.hab(waterImpactedHab[11])
  wphabAB$a011 = poutput$E011[1]*ab.hab(waterImpactedHab[11])
  wphabAll$a012 = poutput$E012[2:8]*allsp.hab(waterImpactedHab[12])
  wphabAB$a012 = poutput$E012[1]*ab.hab(waterImpactedHab[12])
  wphabAll$a013 = poutput$E013[2:8]*allsp.hab(waterImpactedHab[13])
  wphabAB$a013 = poutput$E013[1]*ab.hab(waterImpactedHab[13])
  wphabAll$a014 = poutput$E014[2:8]*allsp.hab(waterImpactedHab[14])
  wphabAB$a014 = poutput$E014[1]*ab.hab(waterImpactedHab[14])
  wphabAll$a015 = poutput$E015[2:8]*allsp.hab(waterImpactedHab[15])
  wphabAB$a015 = poutput$E015[1]*ab.hab(waterImpactedHab[15])
  wphabAll$a016 = poutput$E016[2:8]*allsp.hab(waterImpactedHab[16])
  wphabAB$a016 = poutput$E016[1]*ab.hab(waterImpactedHab[16])


  WshedHabAllsp$w003 = wphabAll$a011 + wphabAll$a012 + wphabAll$a013 +
    wphabAll$a014 + wphabAll$a015 + wphabAll$a016

  WshedHabABsp$w003 = wphabAB$a011 + wphabAB$a012 + wphabAB$a013 +
    wphabAB$a014 + wphabAB$a015 + wphabAB$a016

  # --------------------Watershed 4 -------------------------------------------

  poutput$E017 = wshedInput[17, c(13:20)]*nodesInput[43, (16)]

  poutput$E018 = wshedInput[18, c(13:20)]*nodesInput[230, (16)]*
    nodesInput[43, (16)]

  wphabAll$a017 = poutput$E017[2:8]*allsp.hab(waterImpactedHab[17])
  wphabAB$a017 = poutput$E017[1]*ab.hab(waterImpactedHab[17])
  wphabAll$a018 = poutput$E018[2:8]*allsp.hab(waterImpactedHab[18])
  wphabAB$a018 = poutput$E018[1]*ab.hab(waterImpactedHab[18])

  WshedHabAllsp$w004 = wphabAll$a017 + wphabAll$a018

  WshedHabABsp$w004 = wphabAB$a017 + wphabAB$a018

  # --------------------Watershed 5 -------------------------------------------

  poutput$E019 = wshedInput[19, c(13:20)]*nodesInput[41, (16)]

  poutput$E020 = wshedInput[20, c(13:20)]*nodesInput[232, (16)]*
    nodesInput[41, (16)]

  poutput$E021 = wshedInput[21, c(13:20)]*nodesInput[231, (16)]*
    nodesInput[41, (16)]

  poutput$E022 = wshedInput[22, c(13:20)]*nodesInput[185, (16)]*
    nodesInput[41, (16)]

  poutput$E023 = wshedInput[23, c(13:20)]*nodesInput[184, (16)]*
    nodesInput[41, (16)]

  wphabAll$a019 = poutput$E019[2:8]*allsp.hab(waterImpactedHab[19])
  wphabAB$a019 = poutput$E019[1]*ab.hab(waterImpactedHab[19])
  wphabAll$a020 = poutput$E020[2:8]*allsp.hab(waterImpactedHab[20])
  wphabAB$a020 = poutput$E020[1]*ab.hab(waterImpactedHab[20])
  wphabAll$a021 = poutput$E021[2:8]*allsp.hab(waterImpactedHab[21])
  wphabAB$a021 = poutput$E021[1]*ab.hab(waterImpactedHab[21])
  wphabAll$a022= poutput$E022[2:8]*allsp.hab(waterImpactedHab[22])
  wphabAB$a022 = poutput$E022[1]*ab.hab(waterImpactedHab[22])
  wphabAll$a023 = poutput$E023[2:8]*allsp.hab(waterImpactedHab[23])
  wphabAB$a023= poutput$E023[1]*ab.hab(waterImpactedHab[23])


  WshedHabAllsp$w005 = wphabAll$a019 + wphabAll$a020 + wphabAll$a021+
    wphabAll$a022 + wphabAll$a023

  WshedHabABsp$w005 = wphabAB$a019 + wphabAB$a020 + wphabAB$a021+
    wphabAB$a022+ wphabAB$a023

   # --------------------Watershed 6 -------------------------------------------

  poutput$E024 = wshedInput[24, c(13:20)]*nodesInput[40, (16)]

  poutput$E025 = wshedInput[25, c(13:20)]*nodesInput[233, (16)]*
    nodesInput[40, (16)]

  poutput$E026 = wshedInput[26, c(13:20)]*nodesInput[234, (16)]*
    nodesInput[40, (16)]

  poutput$E027 = wshedInput[27, c(13:20)]*nodesInput[182, (16)]*
    nodesInput[40, (16)]

  poutput$E028 = wshedInput[28, c(13:20)]*nodesInput[183, (16)]*
    nodesInput[40, (16)]

  wphabAll$a019 = poutput$E019[2:8]*allsp.hab(waterImpactedHab[19])
  wphabAB$a019 = poutput$E019[1]*ab.hab(waterImpactedHab[19])
  wphabAll$a020 = poutput$E020[2:8]*allsp.hab(waterImpactedHab[20])
  wphabAB$a020 = poutput$E020[1]*ab.hab(waterImpactedHab[20])
  wphabAll$a021 = poutput$E021[2:8]*allsp.hab(waterImpactedHab[21])
  wphabAB$a021 = poutput$E021[1]*ab.hab(waterImpactedHab[21])
  wphabAll$a022= poutput$E022[2:8]*allsp.hab(waterImpactedHab[22])
  wphabAB$a022 = poutput$E022[1]*ab.hab(waterImpactedHab[22])
  wphabAll$a023 = poutput$E023[2:8]*allsp.hab(waterImpactedHab[23])
  wphabAB$a023= poutput$E023[1]*ab.hab(waterImpactedHab[23])


  WshedHabAllsp$w006 = wphabAll$a019 + wphabAll$a020 + wphabAll$a021 +
    wphabAll$a022 + wphabAll$a023

  WshedHabABsp$w006 = wphabAB$a019 + wphabAB$a020 + wphabAB$a021 +
    wphabAB$a022 + wphabAB$a023

  # --------------------Watershed 7 -------------------------------------------

  poutput$E029 = wshedInput[29, c(13:20)]*nodesInput[38, (16)]

  poutput$E030 = wshedInput[30, c(13:20)]*nodesInput[237, (16)]*
    nodesInput[38, (16)]

  poutput$E031 = wshedInput[31, c(13:20)]*nodesInput[236, (16)]*
    nodesInput[38, (16)]

  poutput$E032 = wshedInput[32, c(13:20)]*nodesInput[235, (16)]*
    nodesInput[38, (16)]

  poutput$E033 = wshedInput[33, c(13:20)]*nodesInput[180, (16)]*
    nodesInput[235, (16)]*
    nodesInput[38, (16)]

  poutput$E034 = wshedInput[34, c(13:20)]*nodesInput[191, (16)]*
    nodesInput[38, (16)]


  wphabAll$a029 = poutput$E029[2:8]*allsp.hab(waterImpactedHab[29])
  wphabAB$a029 = poutput$E029[1]*ab.hab(waterImpactedHab[29])
  wphabAll$a030 = poutput$E030[2:8]*allsp.hab(waterImpactedHab[30])
  wphabAB$a030 = poutput$E030[1]*ab.hab(waterImpactedHab[30])
  wphabAll$a031 = poutput$E031[2:8]*allsp.hab(waterImpactedHab[31])
  wphabAB$a031 = poutput$E031[1]*ab.hab(waterImpactedHab[31])
  wphabAll$a032= poutput$E032[2:8]*allsp.hab(waterImpactedHab[32])
  wphabAB$a032 = poutput$E032[1]*ab.hab(waterImpactedHab[32])
  wphabAll$a033 = poutput$E033[2:8]*allsp.hab(waterImpactedHab[33])
  wphabAB$a033= poutput$E033[1]*ab.hab(waterImpactedHab[33])
  wphabAll$a034 = poutput$E034[2:8]*allsp.hab(waterImpactedHab[34])
  wphabAB$a034= poutput$E034[1]*ab.hab(waterImpactedHab[34])


  WshedHabAllsp$w007 = wphabAll$a029 + wphabAll$a030 + wphabAll$a031 +
    wphabAll$a032 + wphabAll$a033 + wphabAll$a034

  WshedHabABsp$w007 = wphabAB$a029 + wphabAB$a030 + wphabAB$a031 +
    wphabAB$a032+ wphabAB$a033 + wphabAB$a034
  # --------------------Watershed 8 -------------------------------------------

  poutput$E035 = wshedInput[35, c(13:20)]*nodesInput[37, (16)]

  poutput$E501 = wshedInput[311, c(13:20)]*nodesInput[313, 16]*
    nodesInput[37, 16]

  poutput$E036 = wshedInput[36, c(13:20)]*nodesInput[190, (16)]*
    nodesInput[37, (16)]*
    nodesInput[313, 16]

  wphabAll$a035 = poutput$E035[2:8]*allsp.hab(waterImpactedHab[35])
  wphabAB$a035= poutput$E035[1]*ab.hab(waterImpactedHab[35])

  wphabAll$a501 = poutput$E501[2:8]*allsp.hab(waterImpactedHab[313])
  wphabAB$a501 = poutput$E501[1]*ab.hab(waterImpactedHab[313])

  wphabAll$a036 = poutput$E036[2:8]*allsp.hab(waterImpactedHab[36])
  wphabAB$a036= poutput$E036[1]*ab.hab(waterImpactedHab[36])

  WshedHabAllsp$w008 = wphabAll$a035 + wphabAll$a036 + wphabAll$a501

  WshedHabABsp$w008 = wphabAB$a035 + wphabAB$a036 + wphabAB$a501

  # --------------------Watershed 9 -------------------------------------------

  poutput$E037 = wshedInput[37, c(13:20)]*nodesInput[36, (16)]

  poutput$E502 = wshedInput[312, c(13:20)]*nodesInput[314, 16]*
    nodesInput[36, 16]

  poutput$E038 = wshedInput[38, c(13:20)]*
    nodesInput[223, (16)]*
    nodesinput[314, 16]*
    nodesInput[36, (16)]

  wphabAll$a037 = poutput$E037[2:8]*allsp.hab(waterImpactedHab[37])
  wphabAB$a037 = poutput$E037[1]*ab.hab(waterImpactedHab[37])

  wphabAll$a502 = poutput$E502[2:8]*allsp.hab(waterImpactedHab[314])
  wphabAB$a502 = poutput$E502[1]*ab.hab(waterImpactedHab[314])

  wphabAll$a038 = poutput$E038[2:8]*allsp.hab(waterImpactedHab[38])
  wphabAB$a038= poutput$E038[1]*ab.hab(waterImpactedHab[38])

  WshedHabAllsp$w009 = wphabAll$a037 + wphabAll$a038 + wphabAll$a502

  WshedHabABsp$w009 = wphabAB$a037 + wphabAB$a038 + wphabAB$a502
  # --------------------Watershed 10 -------------------------------------------

  poutput$E039 = wshedInput[39, c(13:20)]*nodesInput[35, (16)]

  poutput$E503 = wshedInput[313, c(13:20)]*nodesInput[315, 16]*
    nodesInput[35, 16]

  poutput$E040 = wshedInput[40, c(13:20)]*nodesInput[218, (16)]*
    nodesinput[315, 16]*
    nodesInput[35, (16)]

  poutput$E041 = wshedInput[41, c(13:20)]*nodesInput[220, (16)]*
    nodesinput[315, 16]*
    nodesInput[35, (16)]

  poutput$E042 = wshedInput[42, c(13:20)]*nodesInput[238, (16)]*
    nodesInput[217, (16)]*
    nodesinput[315, 16]*
    nodesInput[35, (16)]

  poutput$E043 = wshedInput[43, c(13:20)]*
    nodesInput[227, (16)]*
    nodesinput[315, 16]*
    nodesInput[35, (16)]

  poutput$E044 = wshedInput[44 ,c(13:20)]*nodesInput[217, (16)]*
    nodesinput[315, 16]*
    nodesInput[35, (16)]


  wphabAll$a039 = poutput$E039[2:8]*allsp.hab(waterImpactedHab[39])
  wphabAB$a039 = poutput$E039[1]*ab.hab(waterImpactedHab[39])
  wphabAll$a040 = poutput$E040[2:8]*allsp.hab(waterImpactedHab[40])
  wphabAB$a040 = poutput$E040[1]*ab.hab(waterImpactedHab[40])
  wphabAll$a041 = poutput$E041[2:8]*allsp.hab(waterImpactedHab[41])
  wphabAB$a041 = poutput$E041[1]*ab.hab(waterImpactedHab[41])
  wphabAll$a042= poutput$E042[2:8]*allsp.hab(waterImpactedHab[42])
  wphabAB$a042 = poutput$E042[1]*ab.hab(waterImpactedHab[42])
  wphabAll$a043 = poutput$E043[2:8]*allsp.hab(waterImpactedHab[43])
  wphabAB$a043= poutput$E043[1]*ab.hab(waterImpactedHab[43])
  wphabAll$a044 = poutput$E044[2:8]*allsp.hab(waterImpactedHab[44])
  wphabAB$a044= poutput$E044[1]*ab.hab(waterImpactedHab[44])

  wphabAll$a503 = poutput$E503[2:8]*allsp.hab(waterIMpactedHab[315])
  wphabAB$a503 = poutput$E503[1]*ab.hab(waterImpactedHab[315])

  WshedHabAllsp$w010 = wphabAll$a039 + wphabAll$a040 + wphabAll$a041 +
    wphabAll$a042 + wphabAll$a043 + wphabAll$a044 + wphabAll$a503

  WshedHabABsp$w010 = wphabAB$a039 + wphabAB$a040 + wphabAB$a041 +
    wphabAB$a042+ wphabAB$a043 + wphabAB$a044 + wphabAB$a503

   # --------------------Watershed 11 -------------------------------------------

  poutput$E045 = wshedInput[45 ,c(13:20)]*nodesInput[34, (16)]
  + wshedInput[46, c(13:20)]*nodesInput[54, (16)]

  poutput$E046 = wshedInput[46 ,c(13:20)]*nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E047 = wshedInput[47 ,c(13:20)]*nodesInput[240, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E048 = wshedInput[48 ,c(13:20)]*nodesInput[239, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E049 = wshedInput[49 ,c(13:20)]*nodesInput[241, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E050 = wshedInput[50 ,c(13:20)]*nodesInput[242, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E051 = wshedInput[51 ,c(13:20)]*nodesInput[243, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E052 = wshedInput[52 ,c(13:20)]*nodesInput[222, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E053 = wshedInput[53 ,c(13:20)]*nodesInput[225, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]

  poutput$E054 = wshedInput[54 ,c(13:20)]*nodesInput[224, (16)]*
    nodesInput[54, (16)]*
    nodesInput[34, (16)]


  wphabAll$a045 = poutput$E045[2:8]*allsp.hab(waterImpactedHab[45])
  wphabAB$a045 = poutput$E045[1]*ab.hab(waterImpactedHab[45])
  wphabAll$a046 = poutput$E046[2:8]*allsp.hab(waterImpactedHab[46])
  wphabAB$a046 = poutput$E046[1]*ab.hab(waterImpactedHab[46])
  wphabAll$a047 = poutput$E047[2:8]*allsp.hab(waterImpactedHab[47])
  wphabAB$a047 = poutput$E047[1]*ab.hab(waterImpactedHab[47])
  wphabAll$a048= poutput$E048[2:8]*allsp.hab(waterImpactedHab[48])
  wphabAB$a048 = poutput$E048[1]*ab.hab(waterImpactedHab[48])
  wphabAll$a049 = poutput$E049[2:8]*allsp.hab(waterImpactedHab[49])
  wphabAB$a049= poutput$E049[1]*ab.hab(waterImpactedHab[49])
  wphabAll$a050 = poutput$E050[2:8]*allsp.hab(waterImpactedHab[50])
  wphabAB$a050= poutput$E050[1]*ab.hab(waterImpactedHab[50])
  wphabAll$a051 = poutput$E051[2:8]*allsp.hab(waterImpactedHab[51])
  wphabAB$a051 = poutput$E051[1]*ab.hab(waterImpactedHab[51])
  wphabAll$a052= poutput$E052[2:8]*allsp.hab(waterImpactedHab[52])
  wphabAB$a052 = poutput$E052[1]*ab.hab(waterImpactedHab[52])
  wphabAll$a053 = poutput$E053[2:8]*allsp.hab(waterImpactedHab[53])
  wphabAB$a053= poutput$E053[1]*ab.hab(waterImpactedHab[53])
  wphabAll$a054 = poutput$E054[2:8]*allsp.hab(waterImpactedHab[54])
  wphabAB$a054= poutput$E054[1]*ab.hab(waterImpactedHab[54])


  WshedHabAllsp$w011 = wphabAll$a045 + wphabAll$a046 + wphabAll$a047 +
    wphabAll$a048 + wphabAll$a049+ wphabAll$a050 + wphabAll$a051+
    wphabAll$a052 + wphabAll$a053 + wphabAll$a054

  WshedHabABsp$w011 = wphabAB$a045 + wphabAB$a046 + wphabAB$a047+
    wphabAB$a048 + wphabAB$a049 + wphabAB$a050 + wphabAB$a051+
    wphabAB$a052+ wphabAB$a053 + wphabAB$a054

  # --------------------Watershed 12 -------------------------------------------


  poutput$E055 = wshedInput[55 ,c(13:20)]*nodesInput[32, (16)]

  poutput$E504 = wshedInput[314, c(13:20)]*nodesInput[316, 16]*
    nodesInput[32, 16]

  poutput$E056 = wshedInput[56 ,c(13:20)]*nodesInput[52, (16)]*
    nodesInput[316, 16]*
    nodesInput[32, (16)]

  poutput$E057 = wshedInput[57 ,c(13:20)]*nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E058 = wshedInput[58 ,c(13:20)]*nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E059 = wshedInput[59 ,c(13:20)]*nodesInput[207, (16)]*
    nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E060 = wshedInput[60 ,c(13:20)]*nodesInput[208, (16)]*
    nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E061 = wshedInput[61 ,c(13:20)]*nodesInput[215, (16)]*
    nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E062 = wshedInput[62 ,c(13:20)]*nodesInput[205, (16)]*
    nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E063 = wshedInput[63 ,c(13:20)]*nodesInput[214, (16)]*
    nodesInput[53, (16)]*
    nodesInput[47, (16)]*
    nodesInput[32, (16)]

  poutput$E064 = wshedInput[64 ,c(13:20)]*nodesInput[226, (16)]*
    nodesInput[52, (16)]*
    nodesInput[316, 16]*
    nodesInput[32, (16)]

  wphabAll$a055 = poutput$E055[2:8]*allsp.hab(waterImpactedHab[55])
  wphabAB$a055 = poutput$E055[1]*ab.hab(waterImpactedHab[55])
  wphabAll$a056 = poutput$E056[2:8]*allsp.hab(waterImpactedHab[56])
  wphabAB$a056 = poutput$E056[1]*ab.hab(waterImpactedHab[56])
  wphabAll$a057 = poutput$E057[2:8]*allsp.hab(waterImpactedHab[57])
  wphabAB$a057 = poutput$E057[1]*ab.hab(waterImpactedHab[57])
  wphabAll$a058= poutput$E058[2:8]*allsp.hab(waterImpactedHab[58])
  wphabAB$a058 = poutput$E058[1]*ab.hab(waterImpactedHab[58])
  wphabAll$a059 = poutput$E059[2:8]*allsp.hab(waterImpactedHab[59])
  wphabAB$a059= poutput$E059[1]*ab.hab(waterImpactedHab[59])
  wphabAll$a060 = poutput$E060[2:8]*allsp.hab(waterImpactedHab[60])
  wphabAB$a060= poutput$E060[1]*ab.hab(waterImpactedHab[60])
  wphabAll$a061 = poutput$E061[2:8]*allsp.hab(waterImpactedHab[61])
  wphabAB$a061 = poutput$E061[1]*ab.hab(waterImpactedHab[61])
  wphabAll$a062= poutput$E062[2:8]*allsp.hab(waterImpactedHab[62])
  wphabAB$a062 = poutput$E062[1]*ab.hab(waterImpactedHab[62])
  wphabAll$a063 = poutput$E063[2:8]*allsp.hab(waterImpactedHab[63])
  wphabAB$a063= poutput$E063[1]*ab.hab(waterImpactedHab[63])
  wphabAll$a064 = poutput$E064[2:8]*allsp.hab(waterImpactedHab[64])
  wphabAB$a064= poutput$E064[1]*ab.hab(waterImpactedHab[64])

  wphabAll$a504 = poutput$E504[2:8]*allsp.hab(waterImpactedHab[316])
  wphabAB$a504 = poutput$E504[1]*ab.hab(waterImpactedHab[316])


  WshedHabAllsp$w012 = wphabAll$a055 + wphabAll$a056 + wphabAll$a057 +
    wphabAll$a058 + wphabAll$a059 + wphabAll$a060 + wphabAll$a051 +
    wphabAll$a062 + wphabAll$a063 + wphabAll$a064 + wphabAll$a504

  WshedHabABsp$w012 = wphabAB$a055 + wphabAB$a056 + wphabAB$a057+
    wphabAB$a058 + wphabAB$a059 + wphabAB$a060 + wphabAB$a061 +
    wphabAB$a062 + wphabAB$a063 + wphabAB$a064 + wphabAB$a504
  # --------------------Watershed 13 -------------------------------------------

  poutput$E065 = wshedInput[65 ,c(13:20)]*nodesInput[31, (16)]

  poutput$E505 = wshedInput[315, c(13:20)]*nodesInput[317, 16]*
    nodesInput[31, 16]

  poutput$E066 = wshedInput[66 ,c(13:20)]*nodesInput[244, (16)]*
    nodesInput[317, 16]*
    nodesInput[31, (16)]

  poutput$E067 = wshedInput[67 ,c(13:20)]*nodesInput[245, (16)]*
    nodesInput[317, 16]*
    nodesInput[31, (16)]

  poutput$E068 = wshedInput[68 ,c(13:20)]*nodesInput[247, (16)]*
    nodesInput[317, 16]*
    nodesInput[31, (16)]

  poutput$E069 = wshedInput[69 ,c(13:20)]*nodesInput[246, (16)]*
    nodesInput[317, 16]*
    nodesInput[31, (16)]

  poutput$E070 = wshedInput[70 ,c(13:20)]*nodesInput[221, (16)]*
    nodesInput[317, 16]*
    nodesInput[31, (16)]

  wphabAll$a065 = poutput$E065[2:8]*allsp.hab(waterImpactedHab[65])
  wphabAB$a065 = poutput$E065[1]*ab.hab(waterImpactedHab[65])
  wphabAll$a066 = poutput$E066[2:8]*allsp.hab(waterImpactedHab[66])
  wphabAB$a066 = poutput$E066[1]*ab.hab(waterImpactedHab[66])
  wphabAll$a067 = poutput$E067[2:8]*allsp.hab(waterImpactedHab[67])
  wphabAB$a067 = poutput$E067[1]*ab.hab(waterImpactedHab[67])
  wphabAll$a068= poutput$E068[2:8]*allsp.hab(waterImpactedHab[68])
  wphabAB$a068 = poutput$E068[1]*ab.hab(waterImpactedHab[68])
  wphabAll$a069 = poutput$E069[2:8]*allsp.hab(waterImpactedHab[69])
  wphabAB$a069= poutput$E069[1]*ab.hab(waterImpactedHab[69])
  wphabAll$a070 = poutput$E070[2:8]*allsp.hab(waterImpactedHab[70])
  wphabAB$a070= poutput$E070[1]*ab.hab(waterImpactedHab[70])

  wphabAll$a505 = poutput$E505[2:8]*allsp.hab(waterImpactedHab[317])
  wphabAB$a505 = poutput$E505[1]*ab.hab(waterImpactedhab[317])

  WshedHabAllsp$w013 = wphabAll$a065 + wphabAll$a066 + wphabAll$a067 +
    wphabAll$a068 + wphabAll$a069 + wphabAll$a070 + wphabAll$a505

  WshedHabABsp$w013 = wphabAB$a065 + wphabAB$a066 + wphabAB$a067 +
    wphabAB$a068 + wphabAB$a069 + wphabAB$a070 + wphabAB$a505

  # --------------------Watershed 14 -------------------------------------------

  poutput$E071 = wshedInput[71 ,c(13:20)]*nodesInput[30, (16)]

  poutput$E506 = wshedInput[316, c(13:20)]*nodesInput[318, 16]*
    nodesInput[30, 16]

  poutput$E072 = wshedInput[72 ,c(13:20)]*nodesInput[281, (16)]*
    nodesInput[318, 16]*
    nodesInput[30, (16)]

  poutput$E073 = wshedInput[73 ,c(13:20)]*nodesInput[249, (16)]*
    nodesInput[30, (16)]

  poutput$E074 = wshedInput[74 ,c(13:20)]*nodesInput[198, (16)]*
    nodesInput[30, (16)]

  poutput$E075 = wshedInput[75 ,c(13:20)]*nodesInput[206, (16)]*
    nodesInput[318, 16]*
    nodesInput[30, (16)]

  poutput$E076 = wshedInput[76 ,c(13:20)]*nodesInput[199, (16)]*
    nodesInput[318, 16]*
    nodesInput[30, (16)]

  poutput$E077 = wshedInput[77 ,c(13:20)]*nodesInput[200, (16)]*
    nodesInput[318, 16]*
    nodesInput[30, (16)]

  poutput$E078 = wshedInput[78 ,c(13:20)]*nodesInput[181, (16)]*
    nodesInput[318, 16]*
    nodesInput[30, (16)]


  wphabAll$a071 = poutput$E071[2:8]*allsp.hab(waterImpactedHab[71])
  wphabAB$a071 = poutput$E071[1]*ab.hab(waterImpactedHab[71])
  wphabAll$a072 = poutput$E072[2:8]*allsp.hab(waterImpactedHab[72])
  wphabAB$a072= poutput$E072[1]*ab.hab(waterImpactedHab[72])
  wphabAll$a073 = poutput$E073[2:8]*allsp.hab(waterImpactedHab[73])
  wphabAB$a073 = poutput$E073[1]*ab.hab(waterImpactedHab[73])
  wphabAll$a074= poutput$E074[2:8]*allsp.hab(waterImpactedHab[74])
  wphabAB$a074= poutput$E074[1]*ab.hab(waterImpactedHab[74])
  wphabAll$a075 = poutput$E075[2:8]*allsp.hab(waterImpactedHab[75])
  wphabAB$a075= poutput$E075[1]*ab.hab(waterImpactedHab[75])
  wphabAll$a076 = poutput$E076[2:8]*allsp.hab(waterImpactedHab[76])
  wphabAB$a076= poutput$E076[1]*ab.hab(waterImpactedHab[76])
  wphabAll$a077= poutput$E077[2:8]*allsp.hab(waterImpactedHab[77])
  wphabAB$a077= poutput$E077[1]*ab.hab(waterImpactedHab[77])
  wphabAll$a078 = poutput$E078[2:8]*allsp.hab(waterImpactedHab[78])
  wphabAB$a078= poutput$E078[1]*ab.hab(waterImpactedHab[78])

  wphabAll$a506 = poutput$E506[2:8]*allsp.hab(waterImpactedHab[318])
  wphabAB$a506 = poutput$E506[1]*ab.hab(waterImpactedHab[318])

  WshedHabAllsp$w014 = wphabAll$a071 + wphabAll$a072 + wphabAll$a073 +
    wphabAll$a074 + wphabAll$a075+ wphabAll$a076 + wphabAll$a077 +
    wphabAll$a078 + wphabAll$a506

  WshedHabABsp$w014 = wphabAB$a071 + wphabAB$a072 + wphabAB$a073 +
    wphabAB$a074 + wphabAB$a075 + wphabAB$a076+ wphabAB$a077 +
    wphabAB$a078 + wphabAB$a506
  # --------------------Watershed 15 -------------------------------------------

  poutput$E079 = wshedInput[79 ,c(13:20)]*nodesInput[29, (16)]

  poutput$E507 = wshedInput[317, c(13:20)]*nodesInput[319, 16]*
    nodesInput[29, 16]

  poutput$E080 = wshedInput[80 ,c(13:20)]*nodesInput[284, (16)]*
    nodesInput[319, 16]*
    nodesInput[29, (16)]

  poutput$E081 = wshedInput[81 ,c(13:20)]*nodesInput[285, (16)]*
    nodesInput[319, 16]*
    nodesInput[29, (16)]

  poutput$E082 = wshedInput[82 ,c(13:20)]*nodesInput[113, (16)]*
    nodesInput[319, 16]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E083 = wshedInput[83 ,c(13:20)]*nodesInput[280, (16)]*
    nodesInput[319, 16]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E084 = wshedInput[84 ,c(13:20)]*nodesInput[278, (16)]*
    nodesInput[319, 16]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E085 = wshedInput[85 ,c(13:20)]*nodesInput[111, (16)]*
    nodesInput[319, 16]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E086 = wshedInput[86 ,c(13:20)]*nodesInput[252, (16)]*
    nodesInput[319, 16]*
    nodesInput[111, (16)]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E087 = wshedInput[87 ,c(13:20)]*nodesInput[250, (16)]*
    nodesInput[319, 16]*
    nodesInput[111, (16)]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E088 = wshedInput[88 ,c(13:20)]*nodesInput[251, (16)]*
    nodesInput[319, 16]*
    nodesInput[252, (16)]*
    nodesInput[111, (16)]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  poutput$E089 = wshedInput[89 ,c(13:20)]*nodesInput[197, (16)]*
    nodesInput[319, 16]*
    nodesInput[111, (16)]*
    nodesInput[113, (16)]*
    nodesInput[285, (16)]*
    nodesInput[29, (16)]

  wphabAll$a079 = poutput$E079[2:8]*allsp.hab(waterImpactedHab[79])
  wphabAB$a079 = poutput$E079[1]*ab.hab(waterImpactedHab[79])
  wphabAll$a080 = poutput$E080[2:8]*allsp.hab(waterImpactedHab[80])
  wphabAB$a080 = poutput$E080[1]*ab.hab(waterImpactedHab[80])
  wphabAll$a081 = poutput$E081[2:8]*allsp.hab(waterImpactedHab[81])
  wphabAB$a081 = poutput$E081[1]*ab.hab(waterImpactedHab[81])
  wphabAll$a082 = poutput$E082[2:8]*allsp.hab(waterImpactedHab[82])
  wphabAB$a082 = poutput$E082[1]*ab.hab(waterImpactedHab[82])
  wphabAll$a083 = poutput$E083[2:8]*allsp.hab(waterImpactedHab[83])
  wphabAB$a083 = poutput$E083[1]*ab.hab(waterImpactedHab[83])
  wphabAll$a084 = poutput$E084[2:8]*allsp.hab(waterImpactedHab[84])
  wphabAB$a084 = poutput$E084[1]*ab.hab(waterImpactedHab[84])
  wphabAll$a085 = poutput$E085[2:8]*allsp.hab(waterImpactedHab[85])
  wphabAB$a085 = poutput$E085[1]*ab.hab(waterImpactedHab[85])
  wphabAll$a086 = poutput$E086[2:8]*allsp.hab(waterImpactedHab[86])
  wphabAB$a086 = poutput$E086[1]*ab.hab(waterImpactedHab[86])
  wphabAll$a087 = poutput$E087[2:8]*allsp.hab(waterImpactedHab[87])
  wphabAB$a087 = poutput$E087[1]*ab.hab(waterImpactedHab[87])
  wphabAll$a088 = poutput$E088[2:8]*allsp.hab(waterImpactedHab[88])
  wphabAB$a088 = poutput$E088[1]*ab.hab(waterImpactedHab[88])
  wphabAll$a089 = poutput$E089[2:8]*allsp.hab(waterImpactedHab[89])
  wphabAB$a089 = poutput$E089[1]*ab.hab(waterImpactedHab[89])

  wphabAll$a507 = poutput$E507[2:8]*allsp.hab(waterImpactedHab[319])
  wphabAB$a507 = poutput$E507[1]*ab.hab(waterImpactedHab[319])

  WshedHabAllsp$w015 = wphabAll$a079 + wphabAll$a080 + wphabAll$a081 +
    wphabAll$a082 + wphabAll$a083+ wphabAll$a084+ wphabAll$a085 +
    wphabAll$a086 + wphabAll$a087+ wphabAll$a088+ wphabAll$a089 +
    wphabAll$a507


  WshedHabABsp$w015 = wphabAB$a079 + wphabAB$a080 + wphabAB$a081 +
    wphabAB$a082 + wphabAB$a083 + wphabAB$a084 + wphabAB$a085 +
    wphabAB$a086 + wphabAB$a087 + wphabAB$a088 + wphabAB$a089 +
    wphabAB$a507
  # --------------------Watershed 16 -------------------------------------------

  poutput$E090 = wshedInput[90 ,c(13:20)]*nodesInput[27, (16)]

  poutput$E508 = wshedInput[318, c(13:20)]*nodesInput[320, 16]*
    nodesInput[27, 16]

  poutput$E091 = wshedInput[91 ,c(13:20)]*nodesInput[119, (16)]*
    nodesInput[320, 16]*
    nodesInput[27, (16)]

  poutput$E092 = wshedInput[92 ,c(13:20)]*nodesInput[279, (16)]*
    nodesInput[320, 16]*
    nodesInput[119, (16)]*
    nodesInput[27, (16)]

  poutput$E093  = wshedInput[93 ,c(13:20)]*nodesInput[72, (16)]*
    nodesInput[320, 16]*
    nodesInput[279, (16)]*
    nodesInput[119, (16)]*
    nodesInput[27, (16)]

  wphabAll$a090 = poutput$E090[2:8]*allsp.hab(waterImpactedHab[90])
  wphabAB$a090 = poutput$E090[1]*ab.hab(waterImpactedHab[90])
  wphabAll$a091= poutput$E091[2:8]*allsp.hab(waterImpactedHab[91])
  wphabAB$a091 = poutput$E091[1]*ab.hab(waterImpactedHab[91])
  wphabAll$a092 = poutput$E092[2:8]*allsp.hab(waterImpactedHab[92])
  wphabAB$a092 = poutput$E092[1]*ab.hab(waterImpactedHab[92])
  wphabAll$a093 = poutput$E093[2:8]*allsp.hab(waterImpactedHab[93])
  wphabAB$a093 = poutput$E093[1]*ab.hab(waterImpactedHab[93])

  wphabAll$a508 = poutput$E508[2:8]*allsp.hab(waterImpactedHab[320])
  wphabAB$a508 = poutput$E508[1]*ab.hab(waterImpactedHab[320])

  WshedHabAllsp$w016 = wphabAll$a090 + wphabAll$a091 + wphabAll$a092 +
    wphabAll$a093 + wphabAll$a508

  WshedHabABsp$w016 = wphabAB$a090 + wphabAB$a091 + wphabAB$a092+
    wphabAB$a093 + wphabAB$a508

  # --------------------Watershed 17 -------------------------------------------

  poutput$E094 = wshedInput[94 ,c(13:20)]*nodesInput[28, (16)]

  poutput$E509 = wshedInput[319, c(13:20)]*nodesInput[321, 16]*
    nodesInput[28, 16]

  poutput$E095 = wshedInput[95 ,c(13:20)]*nodesInput[109, (16)]*
    nodesInput[321, 16]*
    nodesInput[28, (16)]

  poutput$E096 = wshedInput[96 ,c(13:20)]*nodesInput[256, (16)]*
    nodesInput[321, 16]*
    nodesInput[109, (16)]*
    nodesInput[28, (16)]

  poutput$E097 = wshedInput[97 ,c(13:20)]*nodesInput[255, (16)]*
    nodesInput[321, 16]*
    nodesInput[109, (16)]*
    nodesInput[28, (16)]

  poutput$E098 = wshedInput[98 ,c(13:20)]*nodesInput[254, (16)]*
    nodesInput[321, 16]*
    nodesInput[109, (16)]*
    nodesInput[28, (16)]

  poutput$E099 = wshedInput[99 ,c(13:20)]*nodesInput[100, (16)]*
    nodesInput[321, 16]*
    nodesInput[28, (16)]

  poutput$E100 = wshedInput[100,c(13:20)]*nodesInput[253, (16)]*
    nodesInput[321, 16]*
    nodesInput[100, (16)]*
    nodesInput[28, (16)]

  poutput$E101 = wshedInput[101,c(13:20)]*nodesInput[201, (16)]*
    nodesInput[321, 16]*
    nodesInput[100, (16)]*
    nodesInput[28, (16)]

  poutput$E102 = wshedInput[102,c(13:20)]*nodesInput[73, (16)]*
    nodesInput[321, 16]*
    nodesInput[201, (16)]*
    nodesInput[100, (16)]*
    nodesInput[28, (16)]


  wphabAll$a094 = poutput$E094[2:8]*allsp.hab(waterImpactedHab[94])
  wphabAB$a094 = poutput$E094[1]*ab.hab(waterImpactedHab[94])
  wphabAll$a095 = poutput$E095[2:8]*allsp.hab(waterImpactedHab[95])
  wphabAB$a095 = poutput$E095[1]*ab.hab(waterImpactedHab[95])
  wphabAll$a096 = poutput$E096[2:8]*allsp.hab(waterImpactedHab[96])
  wphabAB$a096 = poutput$E096[1]*ab.hab(waterImpactedHab[96])
  wphabAll$a097 = poutput$E097[2:8]*allsp.hab(waterImpactedHab[97])
  wphabAB$a097 = poutput$E097[1]*ab.hab(waterImpactedHab[97])
  wphabAll$a098= poutput$E098[2:8]*allsp.hab(waterImpactedHab[98])
  wphabAB$a098 = poutput$E098[1]*ab.hab(waterImpactedHab[98])
  wphabAll$a099 = poutput$E099[2:8]*allsp.hab(waterImpactedHab[99])
  wphabAB$a099 = poutput$E099[1]*ab.hab(waterImpactedHab[99])
  wphabAll$a100 = poutput$E100[2:8]*allsp.hab(waterImpactedHab[100])
  wphabAB$a100 = poutput$E100[1]*ab.hab(waterImpactedHab[100])
  wphabAll$a101= poutput$E101[2:8]*allsp.hab(waterImpactedHab[101])
  wphabAB$a101 = poutput$E101[1]*ab.hab(waterImpactedHab[101])
  wphabAll$a102 = poutput$E102[2:8]*allsp.hab(waterImpactedHab[102])
  wphabAB$a102 = poutput$E102[1]*ab.hab(waterImpactedHab[102])

  wphabAll$a509 = poutput$E509[2:8]*allsp.hab(waterImpactedHab[321])
  wphabAB$a509 = poutput$E509[1]*ab.hab(waterImpactedhab[321])

  WshedHabAllsp$w017 = wphabAll$a094 + wphabAll$a095 + wphabAll$a096 +
    wphabAll$a097 + wphabAll$a098 + wphabAll$a099 + wphabAll$a100 +
    wphabAll$a101 + wphabAll$a102 + wphabAll$a509

  WshedHabABsp$w017 = wphabAB$a094 + wphabAB$a095 + wphabAB$a096 +
    wphabAB$a097 + wphabAB$a098 + wphabAB$a099 + wphabAB$a100+
    wphabAB$a101 + wphabAB$a102 + wphabAB$a509

  # --------------------Watershed 18 -------------------------------------------

  poutput$E103 = wshedInput[103,c(13:20)]*nodesInput[26, (16)]

  poutput$E510 = wshedInput[320, c(13:20)]*nodesInput[322, 16]*
    nodesInput[26, 16]

  poutput$E104 = wshedInput[104,c(13:20)]*nodesInput[289, (16)]*
    nodesInput[322, 16]*
    nodesInput[26, (16)]

  poutput$E105 = wshedInput[105,c(13:20)]*nodesInput[286, (16)]*
    nodesInput[322, 16]*
    nodesInput[26, (16)]

  poutput$E106 = wshedInput[106,c(13:20)]*nodesInput[288, (16)]*
    nodesInput[322, 16]*
    nodesInput[26, (16)]

  poutput$E107 = wshedInput[107,c(13:20)]*nodesInput[112, (16)]*
    nodesInput[322, 16]*
    nodesInput[26, (16)]

  poutput$E350 = wshedInput[309, c(13:20)]*nodesInput[287, (16)]*
    nodesInput[322, 16]*
    nodesInput[26, (16)]

  wphabAll$a103 = poutput$E103[2:8]*allsp.hab(waterImpactedHab[103])
  wphabAB$a103 = poutput$E103[1]*ab.hab(waterImpactedHab[103])
  wphabAll$a104 =poutput$E104[2:8]*allsp.hab(waterImpactedHab[104])
  wphabAB$a104 = poutput$E104[1]*ab.hab(waterImpactedHab[104])
  wphabAll$a105= poutput$E105[2:8]*allsp.hab(waterImpactedHab[105])
  wphabAB$a105 = poutput$E105[1]*ab.hab(waterImpactedHab[105])
  wphabAll$a106 = poutput$E106[2:8]*allsp.hab(waterImpactedHab[106])
  wphabAB$a106 = poutput$E106[1]*ab.hab(waterImpactedHab[106])
  wphabAll$a107 = poutput$E107[2:8]*allsp.hab(waterImpactedHab[107])
  wphabAB$a107 = poutput$E107[1]*ab.hab(waterImpactedHab[107])
  wphabAll$a350= poutput$E350[2:8]*allsp.hab(waterImpactedHab[305])
  wphabAB$a350 = poutput$E350[1]*ab.hab(waterImpactedHab[305])

  wphabAll$a510 = poutput$E510[2:8]*allsp.hab(waterImpactedHab[322])
  wphabAB$a510 = poutput$E510[1]*ab.hab(waterImpactedHab[322])

  WshedHabAllsp$w018 = wphabAll$a103 + wphabAll$a104 + wphabAll$a105 +
    wphabAll$a106 + wphabAll$a107 + wphabAll$a350 + wphabAll$a510

  WshedHabABsp$w018 = wphabAB$a103 + wphabAB$a104 + wphabAB$a105 +
    wphabAB$a106 + wphabAB$a107 + wphabAB$a350 + wphabAB$a510

  # --------------------Watershed 19 -------------------------------------------

  poutput$E108 = wshedInput[108,c(13:20)]*nodesInput[25, (16)]

  poutput$E511 = wshedInput[321, c(13:20)]*nodesInput[323, 16]*
    nodesInput[25, 16]

  poutput$E109 = wshedInput[109,c(13:20)]*nodesInput[132, (16)]*
    nodesInput[323, 16]*
    nodesInput[25, (16)]

  poutput$E110 = wshedInput[110,c(13:20)]*nodesInput[257, (16)]*
    nodesInput[323, 16]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  poutput$E111 = wshedInput[111,c(13:20)]*nodesInput[258, (16)]*
    nodesInput[323, 16]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  poutput$E112 = wshedInput[112,c(13:20)]*nodesInput[114, (16)]*
    nodesInput[323, 16]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  poutput$E113 = wshedInput[113,c(13:20)]*nodesInput[74, (16)]*
    nodesInput[323, 16]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  poutput$E114 = wshedInput[114,c(13:20)]*nodesInput[71, (16)]*
    nodesInput[323, 16]*
    nodesInput[114, (16)]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  poutput$E115 = wshedInput[115,c(13:20)]*nodesInput[139, (16)]*
    nodesInput[323, 16]*
    nodesInput[74, (16)]*
    nodesInput[132, (16)]*
    nodesInput[25, (16)]

  wphabAll$a108 = poutput$E108[2:8]*allsp.hab(waterImpactedHab[108])
  wphabAB$a108 = poutput$E108[1]*ab.hab(waterImpactedHab[108])
  wphabAll$a109 =poutput$E109[2:8]*allsp.hab(waterImpactedHab[109])
  wphabAB$a109 = poutput$E109[1]*ab.hab(waterImpactedHab[109])
  wphabAll$a110= poutput$E110[2:8]*allsp.hab(waterImpactedHab[110])
  wphabAB$a110 = poutput$E110[1]*ab.hab(waterImpactedHab[110])
  wphabAll$a111 = poutput$E111[2:8]*allsp.hab(waterImpactedHab[111])
  wphabAB$a111 = poutput$E111[1]*ab.hab(waterImpactedHab[111])
  wphabAll$a112 = poutput$E112[2:8]*allsp.hab(waterImpactedHab[112])
  wphabAB$a112 = poutput$E112[1]*ab.hab(waterImpactedHab[112])
  wphabAll$a113= poutput$E113[2:8]*allsp.hab(waterImpactedHab[113])
  wphabAB$a113= poutput$E113[1]*ab.hab(waterImpactedHab[113])
  wphabAll$a114 = poutput$E114[2:8]*allsp.hab(waterImpactedHab[114])
  wphabAB$a114 = poutput$E114[1]*ab.hab(waterImpactedHab[114])
  wphabAll$a115= poutput$E115[2:8]*allsp.hab(waterImpactedHab[115])
  wphabAB$a115 = poutput$E115[1]*ab.hab(waterImpactedHab[115])

  wphabAll$a511 = poutput$E511[2:8]*allsp.hab(waterImpactedHab[323])
  wphabAB$a511 = poutput$E511[1]*ab.hab(waterImpactedHab[323])

  WshedHabAllsp$w019 = wphabAll$a108 + wphabAll$a109 + wphabAll$a110 +
  wphabAll$a111 + wphabAll$a112 + wphabAll$a113 + wphabAll$a114 +
    wphabAll$a115 + wphabAll$a511

  WshedHabABsp$w019 = wphabAB$a108 + wphabAB$a109 + wphabAB$a110 +
    wphabAB$a111 + wphabAB$a112 + wphabAB$a113 + wphabAB$a114 +
    wphabAB$a115 + wphabAB$a511

  # --------------------Watershed 20 -------------------------------------------

  poutput$E116 = wshedInput[116,c(13:20)]*nodesInput[24, (16)]

  poutput$E117 = wshedInput[117,c(13:20)]*nodesInput[290, (16)]*
    nodesInput[24, (16)]

  poutput$E118 = wshedInput[118,c(13:20)]*nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E119 = wshedInput[119, c(13:20)]*nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E120 = wshedInput[120, c(13:20)]*nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E121 = wshedInput[121, c(13:20)]*nodesInput[115, (16)]*
    nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E122 = wshedInput[122, c(13:20)]*nodesInput[116, (16)]*
    nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E123 = wshedInput[123, c(13:20)]*nodesInput[67, (16)]*
    nodesInput[116, (16)]*
    nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E124 = wshedInput[124, c(13:20)]*nodesInput[68, (16)]*
    nodesInput[115, (16)]*
    nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]

  poutput$E125 = wshedInput[125, c(13:20)]*nodesInput[61, (16)]*
    nodesInput[68, (16)]*
    nodesInput[115, (16)]*
    nodesInput[135, (16)]*
    nodesInput[130, (16)]*
    nodesInput[110, (16)]*
    nodesInput[24, (16)]


  wphabAll$a116 = poutput$E116[2:8]*allsp.hab(waterImpactedHab[116])
  wphabAB$a116= poutput$E116[1]*ab.hab(waterImpactedHab[116])
  wphabAll$a117 =poutput$E117[2:8]*allsp.hab(waterImpactedHab[117])
  wphabAB$a117 = poutput$E117[1]*ab.hab(waterImpactedHab[117])
  wphabAll$a118= poutput$E118[2:8]*allsp.hab(waterImpactedHab[118])
  wphabAB$a118 = poutput$E118[1]*ab.hab(waterImpactedHab[118])
  wphabAll$a119 = poutput$E119[2:8]*allsp.hab(waterImpactedHab[119])
  wphabAB$a119 = poutput$E119[1]*ab.hab(waterImpactedHab[119])
  wphabAll$a120 = poutput$E120[2:8]*allsp.hab(waterImpactedHab[120])
  wphabAB$a120 = poutput$E120[1]*ab.hab(waterImpactedHab[120])
  wphabAll$a121 = poutput$E121[2:8]*allsp.hab(waterImpactedHab[121])
  wphabAB$a121= poutput$E121[1]*ab.hab(waterImpactedHab[121])
  wphabAll$a122= poutput$E122[2:8]*allsp.hab(waterImpactedHab[122])
  wphabAB$a122 = poutput$E122[1]*ab.hab(waterImpactedHab[122])
  wphabAll$a123= poutput$E123[2:8]*allsp.hab(waterImpactedHab[123])
  wphabAB$a123 = poutput$E123[1]*ab.hab(waterImpactedHab[123])
  wphabAll$a124= poutput$E124[2:8]*allsp.hab(waterImpactedHab[124])
  wphabAB$a124 = poutput$E124[1]*ab.hab(waterImpactedHab[124])
  wphabAll$a125= poutput$E125[2:8]*allsp.hab(waterImpactedHab[125])
  wphabAB$a125 = poutput$E125[1]*ab.hab(waterImpactedHab[125])

  WshedHabAllsp$w020 = wphabAll$a116 + wphabAll$a117 + wphabAll$a118 +
    wphabAll$a119 + wphabAll$a120 + wphabAll$a121 + wphabAll$a122 + wphabAll$a123 +
    wphabAll$a124 + wphabAll$a125

  WshedHabABsp$w020 = wphabAB$a116 + wphabAB$a117 + wphabAB$a118 +
    wphabAB$a119 + wphabAB$a120 + wphabAB$a121 + wphabAB$a122 + wphabAB$a123 +
    wphabAB$a124 + wphabAB$a125

  # --------------------Watershed 21 -------------------------------------------

  poutput$E126 = wshedInput[126, c(13:20)]*nodesInput[23, (16)]

  poutput$E127 = wshedInput[127, c(13:20)]*nodesInput[128, (16)]*
    nodesInput[23, (16)]

  wphabAll$a126= poutput$E126[2:8]*allsp.hab(waterImpactedHab[126])
  wphabAB$a126 = poutput$E126[1]*ab.hab(waterImpactedHab[126])
  wphabAll$a127= poutput$E127[2:8]*allsp.hab(waterImpactedHab[127])
  wphabAB$a127 = poutput$E127[1]*ab.hab(waterImpactedHab[127])

  WshedHabAllsp$w021 = wphabAll$a126 + wphabAll$a127
  WshedHabABsp$w021 = wphabAB$a126 + wphabAB$a127
  # --------------------Watershed 22 -------------------------------------------

  poutput$E128 = wshedInput[128, c(13:20)]*nodesInput[22, (16)]

  poutput$E129 = wshedInput[129, c(13:20)]*nodesInput[101, (16)]*
    nodesInput[22, (16)]

  poutput$E130 = wshedInput[130, c(13:20)]*nodesInput[136, (16)]*
    nodesInput[101, (16)]*
    nodesInput[22, (16)]

  poutput$E131 = wshedInput[131, c(13:20)]*nodesInput[282, (16)]*
    nodesInput[136, (16)]*
    nodesInput[101, (16)]*
    nodesInput[22, (16)]

  poutput$E132 = wshedInput[132, c(13:20)]*nodesInput[60, (16)]*
    nodesInput[282, (16)]*
    nodesInput[136, (16)]*
    nodesInput[101, (16)]*
    nodesInput[22, (16)]

  wphabAll$a128 = poutput$E128[2:8]*allsp.hab(waterImpactedHab[128])
  wphabAB$a128= poutput$E128[1]*ab.hab(waterImpactedHab[128])
  wphabAll$a129= poutput$E129[2:8]*allsp.hab(waterImpactedHab[129])
  wphabAB$a129 = poutput$E129[1]*ab.hab(waterImpactedHab[129])
  wphabAll$a130= poutput$E130[2:8]*allsp.hab(waterImpactedHab[130])
  wphabAB$a130= poutput$E130[1]*ab.hab(waterImpactedHab[130])
  wphabAll$a131= poutput$E131[2:8]*allsp.hab(waterImpactedHab[131])
  wphabAB$a131= poutput$E131[1]*ab.hab(waterImpactedHab[131])
  wphabAll$a132= poutput$E132[2:8]*allsp.hab(waterImpactedHab[132])
  wphabAB$a132= poutput$E132[1]*ab.hab(waterImpactedHab[132])

  WshedHabAllsp$w022 = wphabAll$a128 + wphabAll$a129 + wphabAll$a130 +
    wphabAll$a131 + wphabAll$a132

  WshedHabABsp$w022 = wphabAB$a128 + wphabAB$a129 + wphabAB$a130 +
    wphabAB$a131 + wphabAB$a132


  # --------------------Watershed 23 -------------------------------------------

  poutput$E133 = wshedInput[133, c(13:20)]*nodesInput[21, (16)]

  poutput$E134 = wshedInput[134, c(13:20)]*nodesInput[291, (16)]*
    nodesInput[21, (16)]

  poutput$E135 = wshedInput[135, c(13:20)]*nodesInput[102, (16)]*
    nodesInput[21, (16)]

  poutput$E136 = wshedInput[136, c(13:20)]*nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E137 = wshedInput[137, c(13:20)]*nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E138 = wshedInput[138, c(13:20)]*nodesInput[293, (16)]*
    nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E139 = wshedInput[139, c(13:20)]*nodesInput[203, (16)]*
    nodesInput[293, (16)]*
    nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E140 = wshedInput[140, c(13:20)]*nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E141 = wshedInput[141, c(13:20)]*nodesInput[167, (16)]*
    nodesInput[203, (16)]*
    nodesInput[293, (16)]*
    nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E142 = wshedInput[142, c(13:20)]*nodesInput[179, (16)]*
    nodesInput[203, (16)]*
    nodesInput[293, (16)]*
    nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E143 = wshedInput[143, c(13:20)]*nodesInput[166, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E144 = wshedInput[144, c(13:20)]*nodesInput[165, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E145 = wshedInput[145, c(13:20)]*nodesInput[164, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E146 = wshedInput[146, c(13:20)]*nodesInput[56, (16)]*
    nodesInput[164, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E147 = wshedInput[147, c(13:20)]*nodesInput[62, (16)]*
    nodesInput[166, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  poutput$E148 = wshedInput[148, c(13:20)]*nodesInput[83, (16)]*
    nodesInput[179, (16)]*
    nodesInput[203, (16)]*
    nodesInput[293, (16)]*
    nodesInput[106, (16)]*
    nodesInput[21, (16)]

  poutput$E149 = wshedInput[149, c(13:20)]*nodesInput[81, (16)]*
    nodesInput[204, (16)]*
    nodesInput[103, (16)]*
    nodesInput[21, (16)]

  wphabAll$a133 = poutput$E133[2:8]*allsp.hab(waterImpactedHab[133])
  wphabAB$a133= poutput$E133[1]*ab.hab(waterImpactedHab[133])
  wphabAll$a134 =poutput$E134[2:8]*allsp.hab(waterImpactedHab[134])
  wphabAB$a134 = poutput$E134[1]*ab.hab(waterImpactedHab[134])
  wphabAll$a135= poutput$E135[2:8]*allsp.hab(waterImpactedHab[135])
  wphabAB$a135 = poutput$E135[1]*ab.hab(waterImpactedHab[135])
  wphabAll$a136= poutput$E136[2:8]*allsp.hab(waterImpactedHab[136])
  wphabAB$a136 = poutput$E136[1]*ab.hab(waterImpactedHab[136])
  wphabAll$a137 = poutput$E137[2:8]*allsp.hab(waterImpactedHab[137])
  wphabAB$a137= poutput$E137[1]*ab.hab(waterImpactedHab[137])
  wphabAll$a138 = poutput$E138[2:8]*allsp.hab(waterImpactedHab[138])
  wphabAB$a138= poutput$E138[1]*ab.hab(waterImpactedHab[138])
  wphabAll$a139= poutput$E139[2:8]*allsp.hab(waterImpactedHab[139])
  wphabAB$a139 = poutput$E139[1]*ab.hab(waterImpactedHab[139])
  wphabAll$a140= poutput$E140[2:8]*allsp.hab(waterImpactedHab[140])
  wphabAB$a140 = poutput$E140[1]*ab.hab(waterImpactedHab[140])
  wphabAll$a141= poutput$E141[2:8]*allsp.hab(waterImpactedHab[141])
  wphabAB$a141 = poutput$E141[1]*ab.hab(waterImpactedHab[141])
  wphabAll$a142= poutput$E142[2:8]*allsp.hab(waterImpactedHab[142])
  wphabAB$a142= poutput$E142[1]*ab.hab(waterImpactedHab[142])
  wphabAll$a143= poutput$E143[2:8]*allsp.hab(waterImpactedHab[143])
  wphabAB$a143= poutput$E143[1]*ab.hab(waterImpactedHab[143])
  wphabAll$a144= poutput$E144[2:8]*allsp.hab(waterImpactedHab[144])
  wphabAB$a144 = poutput$E144[1]*ab.hab(waterImpactedHab[144])
  wphabAll$a145 = poutput$E145[2:8]*allsp.hab(waterImpactedHab[145])
  wphabAB$a145= poutput$E145[1]*ab.hab(waterImpactedHab[145])
  wphabAll$a146 = poutput$E146[2:8]*allsp.hab(waterImpactedHab[146])
  wphabAB$a146= poutput$E146[1]*ab.hab(waterImpactedHab[146])
  wphabAll$a147= poutput$E147[2:8]*allsp.hab(waterImpactedHab[147])
  wphabAB$a147 = poutput$E147[1]*ab.hab(waterImpactedHab[147])
  wphabAll$a148= poutput$E148[2:8]*allsp.hab(waterImpactedHab[148])
  wphabAB$a148 = poutput$E148[1]*ab.hab(waterImpactedHab[148])
  wphabAll$a149= poutput$E149[2:8]*allsp.hab(waterImpactedHab[124])
  wphabAB$a149 = poutput$E149[1]*ab.hab(waterImpactedHab[149])

  WshedHabAllsp$w023= wphabAll$a133 + wphabAll$a134 + wphabAll$a135 +
    wphabAll$a136 + wphabAll$a137 + wphabAll$a138 + wphabAll$a139 + wphabAll$a140 +
    wphabAll$a141 + wphabAll$a142 + wphabAll$a143 + wphabAll$a144 + wphabAll$a145 +
    wphabAll$a146 + wphabAll$a147 + wphabAll$a148 + wphabAll$a149

  WshedHabABsp$w023 = wphabAB$a133 + wphabAB$a134 + wphabAB$a135 +
    wphabAB$a136 + wphabAB$a137 + wphabAB$a138 + wphabAB$a139 + wphabAB$a140 +
    wphabAB$a141 + wphabAB$a142 + wphabAB$a143 + wphabAB$a144 + wphabAB$a145 +
    wphabAB$a146 + wphabAB$a147 + wphabAB$a148 + wphabAB$a149
  # --------------------Watershed 24 -------------------------------------------

  poutput$E150 = wshedInput[150, c(13:20)]*nodesInput[20, (16)]

  poutput$E151 = wshedInput[151, c(13:20)]*nodesInput[292, (16)]*
    nodesInput[20, (16)]

  poutput$E152 = wshedInput[152, c(13:20)]*nodesInput[107, (16)]*
    nodesInput[20, (16)]

  poutput$E153 =wshedInput[153, c(13:20)]*nodesInput[212, (16)]*
    nodesInput[107, (16)]*
    nodesInput[20, (16)]

  poutput$E154 = wshedInput[154, c(13:20)]*nodesInput[294, (16)]*
    nodesInput[107, (16)]*
    nodesInput[20, (16)]

  poutput$E155 = wshedInput[155, c(13:20)]*nodesInput[176, (16)]*
    nodesInput[107, (16)]*
    nodesInput[20, (16)]

  poutput$E156 = wshedInput[156, c(13:20)]*nodesInput[88, (16)]*
    nodesInput[176, (16)]*
    nodesInput[107, (16)]*
    nodesInput[20, (16)]

  wphabAll$a150= poutput$E150[2:8]*allsp.hab(waterImpactedHab[150])
  wphabAB$a150 = poutput$E150[1]*ab.hab(waterImpactedHab[150])
  wphabAll$a151= poutput$E151[2:8]*allsp.hab(waterImpactedHab[151])
  wphabAB$a151= poutput$E151[1]*ab.hab(waterImpactedHab[151])
  wphabAll$a152= poutput$E152[2:8]*allsp.hab(waterImpactedHab[152])
  wphabAB$a152= poutput$E152[1]*ab.hab(waterImpactedHab[152])
  wphabAll$a153= poutput$E153[2:8]*allsp.hab(waterImpactedHab[153])
  wphabAB$a153 = poutput$E153[1]*ab.hab(waterImpactedHab[153])
  wphabAll$a154 = poutput$E154[2:8]*allsp.hab(waterImpactedHab[154])
  wphabAB$a154= poutput$E154[1]*ab.hab(waterImpactedHab[154])
  wphabAll$a155 = poutput$E155[2:8]*allsp.hab(waterImpactedHab[155])
  wphabAB$a155= poutput$E155[1]*ab.hab(waterImpactedHab[155])
  wphabAll$a156= poutput$E156[2:8]*allsp.hab(waterImpactedHab[156])
  wphabAB$a156 = poutput$E156[1]*ab.hab(waterImpactedHab[156])

  WshedHabAllsp$w024 = wphabAll$a150 + wphabAll$a151 + wphabAll$a152 +
    wphabAll$a153 + wphabAll$a154 + wphabAll$a155 + wphabAll$a156

  WshedHabABsp$w024 = wphabAB$a150 + wphabAB$a151+ wphabAB$a152 +
    wphabAB$a153 + wphabAB$a154 + wphabAB$a155 + wphabAB$a156


  # --------------------Watershed 25 -------------------------------------------

  poutput$E157 = wshedInput[157, c(13:20)]*nodesInput[19, (16)]

  poutput$E158 = wshedInput[158, c(13:20)]*nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E159 = wshedInput[159, c(13:20)]*nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E160 = wshedInput[160, c(13:20)]*nodesInput[159, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E161 = wshedInput[161, c(13:20)]*nodesInput[171, (16)]*
    nodesInput[159, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E162 = wshedInput[162, c(13:20)]*nodesInput[75, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E163 = wshedInput[163, c(13:20)]*nodesInput[172, (16)]*
    nodesInput[75, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E164 =  wshedInput[164, c(13:20)]*nodesInput[173, (16)]*
    nodesInput[75, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E165 = wshedInput[165, c(13:20)]*nodesInput[97, (16)]*
    nodesInput[173, (16)]*
    nodesInput[75, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E166 = wshedInput[166, c(13:20)]*nodesInput[77, (16)]*
    nodesInput[172, (16)]*
    nodesInput[75, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]

  poutput$E167 = wshedInput[167, c(13:20)]*nodesInput[78, (16)]*
    nodesInput[171, (16)]*
    nodesInput[159, (16)]*
    nodesInput[94, (16)]*
    nodesInput[202, (16)]*
    nodesInput[19, (16)]


  wphabAll$a157 = poutput$E157[2:8]*allsp.hab(waterImpactedHab[157])
  wphabAB$a157= poutput$E157[1]*ab.hab(waterImpactedHab[157])
  wphabAll$a158 = poutput$E158[2:8]*allsp.hab(waterImpactedHab[158])
  wphabAB$a158= poutput$E158[1]*ab.hab(waterImpactedHab[158])
  wphabAll$a159= poutput$E159[2:8]*allsp.hab(waterImpactedHab[159])
  wphabAB$a159 = poutput$E159[1]*ab.hab(waterImpactedHab[159])
  wphabAll$a160= poutput$E160[2:8]*allsp.hab(waterImpactedHab[160])
  wphabAB$a160 = poutput$E160[1]*ab.hab(waterImpactedHab[160])
  wphabAll$a161= poutput$E161[2:8]*allsp.hab(waterImpactedHab[161])
  wphabAB$a161 = poutput$E161[1]*ab.hab(waterImpactedHab[161])
  wphabAll$a162= poutput$E162[2:8]*allsp.hab(waterImpactedHab[162])
  wphabAB$a162= poutput$E162[1]*ab.hab(waterImpactedHab[162])
  wphabAll$a163= poutput$E163[2:8]*allsp.hab(waterImpactedHab[163])
  wphabAB$a163= poutput$E163[1]*ab.hab(waterImpactedHab[163])
  wphabAll$a164= poutput$E164[2:8]*allsp.hab(waterImpactedHab[164])
  wphabAB$a164 = poutput$E164[1]*ab.hab(waterImpactedHab[164])
  wphabAll$a165 = poutput$E165[2:8]*allsp.hab(waterImpactedHab[165])
  wphabAB$a165= poutput$E165[1]*ab.hab(waterImpactedHab[165])
  wphabAll$a166 = poutput$E166[2:8]*allsp.hab(waterImpactedHab[166])
  wphabAB$a166= poutput$E166[1]*ab.hab(waterImpactedHab[166])
  wphabAll$a167= poutput$E167[2:8]*allsp.hab(waterImpactedHab[167])
  wphabAB$a167= poutput$E167[1]*ab.hab(waterImpactedHab[167])


  WshedHabAllsp$w025 = wphabAll$a157 + wphabAll$a158 + wphabAll$a159 + wphabAll$a160 +
    wphabAll$a161 + wphabAll$a162 + wphabAll$a163 + wphabAll$a164 + wphabAll$a165 +
    wphabAll$a166 + wphabAll$a167

  WshedHabABsp$w025 =wphabAB$a157 + wphabAB$a158 + wphabAB$a159 + wphabAB$a160 +
    wphabAB$a161 + wphabAB$a162 + wphabAB$a143 + wphabAB$a164 + wphabAB$a165 +
    wphabAB$a166 + wphabAB$a167

  # --------------------Watershed 26 -------------------------------------------

  poutput$E168 = wshedInput[168, c(13:20)]*nodesInput[18, (16)]

  poutput$E169 = wshedInput[169, c(13:20)]*nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E170 = wshedInput[170, c(13:20)]*nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E171 = wshedInput[171, c(13:20)]*nodesInput[95, (16)]*
    nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E172 = wshedInput[172, c(13:20)]*nodesInput[177, (16)]*
    nodesInput[95, (16)]*
    nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E173 = wshedInput[173, c(13:20)]*nodesInput[310, (16)]*
    nodesInput[95, (16)]*
    nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E174 = wshedInput[174, c(13:20)]*nodesInput[89, (16)]*
    nodesInput[177, (16)]*
    nodesInput[95, (16)]*
    nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]

  poutput$E175 = wshedInput[175, c(13:20)]*nodesInput[92, (16)]*
    nodesInput[310, (16)]*
    nodesInput[95, (16)]*
    nodesInput[69, (16)]*
    nodesInput[154, (16)]*
    nodesInput[18, (16)]


  wphabAll$a168 = poutput$E168[2:8]*allsp.hab(waterImpactedHab[168])
  wphabAB$a168= poutput$E168[1]*ab.hab(waterImpactedHab[168])
  wphabAll$a169= poutput$E169[2:8]*allsp.hab(waterImpactedHab[169])
  wphabAB$a169 = poutput$E169[1]*ab.hab(waterImpactedHab[169])
  wphabAll$a170= poutput$E170[2:8]*allsp.hab(waterImpactedHab[170])
  wphabAB$a170 = poutput$E170[1]*ab.hab(waterImpactedHab[170])
  wphabAll$a171= poutput$E171[2:8]*allsp.hab(waterImpactedHab[171])
  wphabAB$a171 = poutput$E171[1]*ab.hab(waterImpactedHab[171])
  wphabAll$a172= poutput$E172[2:8]*allsp.hab(waterImpactedHab[172])
  wphabAB$a172= poutput$E172[1]*ab.hab(waterImpactedHab[172])
  wphabAll$a173= poutput$E173[2:8]*allsp.hab(waterImpactedHab[173])
  wphabAB$a173= poutput$E173[1]*ab.hab(waterImpactedHab[173])
  wphabAll$a174= poutput$E174[2:8]*allsp.hab(waterImpactedHab[174])
  wphabAB$a174 = poutput$E174[1]*ab.hab(waterImpactedHab[174])
  wphabAll$a175 = poutput$E175[2:8]*allsp.hab(waterImpactedHab[175])
  wphabAB$a175= poutput$E175[1]*ab.hab(waterImpactedHab[175])

  WshedHabAllsp$w026 =  wphabAll$a168 + wphabAll$a169 + wphabAll$a170 +
    wphabAll$a171 + wphabAll$a172 + wphabAll$a173 + wphabAll$a174 + wphabAll$a175

  WshedHabABsp$w026 = wphabAB$a168 + wphabAB$a169 + wphabAB$a170 +
    wphabAB$a171 + wphabAB$a172 + wphabAB$a173 + wphabAB$a174 + wphabAB$a175

  # --------------------Watershed 27 -------------------------------------------

  poutput$E176 = wshedInput[176, c(13:20)]*nodesInput[17, (16)]

  poutput$E177 = wshedInput[177, c(13:20)]*nodesInput[86, (16)]*
    nodesInput[17, (16)]

  poutput$E178 = wshedInput[178, c(13:20)]*nodesInput[178, (16)]*
    nodesInput[86, (16)]*
    nodesInput[17, (16)]

  wphabAll$a176= poutput$E176[2:8]*allsp.hab(waterImpactedHab[176])
  wphabAB$a176= poutput$E176[1]*ab.hab(waterImpactedHab[176])
  wphabAll$a177= poutput$E177[2:8]*allsp.hab(waterImpactedHab[177])
  wphabAB$a177 = poutput$E177[1]*ab.hab(waterImpactedHab[177])
  wphabAll$a178 = poutput$E178[2:8]*allsp.hab(waterImpactedHab[178])
  wphabAB$a178= poutput$E178[1]*ab.hab(waterImpactedHab[178])

  WshedHabAllsp$w027 =  wphabAll$a176 + wphabAll$a177 + wphabAll$a178
  WshedHabABsp$w027 = wphabAB$a176 + wphabAB$a177 + wphabAB$a178

  # --------------------Watershed 28 -------------------------------------------

  poutput$E179 = wshedInput[179, c(13:20)]*nodesInput[16, (16)]

  poutput$E180 = wshedInput[180, c(13:20)]*nodesInput[134, (16)]*
   nodesInput[16, (16)]

  poutput$E181 = wshedInput[181, c(13:20)]*nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E182 = wshedInput[182, c(13:20)]*nodesInput[263, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E183 = wshedInput[183, c(13:20)]*nodesInput[262, (16)]*
    nodesInput[263, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E184 = wshedInput[184, c(13:20)]*nodesInput[261, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E185 = wshedInput[185, c(13:20)]*nodesInput[149, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E186 = wshedInput[186, c(13:20)]*nodesInput[259, (16)]*
    nodesInput[260, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E187 = wshedInput[187, c(13:20)]*nodesInput[260, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  poutput$E188 = wshedInput[188, c(13:20)]*nodesInput[93, (16)]*
    nodesInput[269, (16)]*
    nodesInput[55, (16)]*
    nodesInput[134, (16)]*
    nodesInput[16, (16)]

  wphabAll$a179= poutput$E179[2:8]*allsp.hab(waterImpactedHab[179])
  wphabAB$a179 = poutput$E179[1]*ab.hab(waterImpactedHab[179])
  wphabAll$a180= poutput$E180[2:8]*allsp.hab(waterImpactedHab[180])
  wphabAB$a180 = poutput$E180[1]*ab.hab(waterImpactedHab[180])
  wphabAll$a181= poutput$E181[2:8]*allsp.hab(waterImpactedHab[181])
  wphabAB$a181 = poutput$E181[1]*ab.hab(waterImpactedHab[181])
  wphabAll$a182= poutput$E182[2:8]*allsp.hab(waterImpactedHab[182])
  wphabAB$a182= poutput$E182[1]*ab.hab(waterImpactedHab[182])
  wphabAll$a183= poutput$E183[2:8]*allsp.hab(waterImpactedHab[183])
  wphabAB$a183= poutput$E183[1]*ab.hab(waterImpactedHab[183])
  wphabAll$a184= poutput$E184[2:8]*allsp.hab(waterImpactedHab[184])
  wphabAB$a184 = poutput$E184[1]*ab.hab(waterImpactedHab[184])
  wphabAll$a185 = poutput$E185[2:8]*allsp.hab(waterImpactedHab[185])
  wphabAB$a185= poutput$E185[1]*ab.hab(waterImpactedHab[185])
  wphabAll$a186 = poutput$E186[2:8]*allsp.hab(waterImpactedHab[186])
  wphabAB$a186= poutput$E186[1]*ab.hab(waterImpactedHab[186])
  wphabAll$a187= poutput$E187[2:8]*allsp.hab(waterImpactedHab[187])
  wphabAB$a187= poutput$E187[1]*ab.hab(waterImpactedHab[187])
  wphabAll$a188= poutput$E188[2:8]*allsp.hab(waterImpactedHab[188])
  wphabAB$a188= poutput$E188[1]*ab.hab(waterImpactedHab[188])

  WshedHabAllsp$w028 =  wphabAll$a179+wphabAll$a180+
    wphabAll$a181 + wphabAll$a182 + wphabAll$a183 + wphabAll$a184 + wphabAll$a185+
    wphabAll$a186 + wphabAll$a187 + wphabAll$a188

  WshedHabABsp$w028 = wphabAB$a179 + wphabAB$a180 + wphabAB$a181 +
    wphabAB$a182 + wphabAB$a183 + wphabAB$a184 + wphabAB$a185 +
    wphabAB$a186 + wphabAB$a187 + wphabAB$a188

  # --------------------Watershed 29 -------------------------------------------

  poutput$E189 = wshedInput[189, c(13:20)]*nodesInput[15, (16)]

  poutput$E190 = wshedInput[190, c(13:20)]*nodesInput[131, (16)]*
    nodesInput[15, (16)]

  poutput$E191 = wshedInput[191, c(13:20)]*nodesInput[299, (16)]*
    nodesInput[131, (16)]*
    nodesInput[15, (16)]

  poutput$E192 = wshedInput[192, c(13:20)]*nodesInput[298, (16)]*
    nodesInput[131, (16)]*
    nodesInput[15, (16)]

  poutput$E193 = wshedInput[193, c(13:20)]*nodesInput[265, (16)]*
    nodesInput[298, (16)]*
    nodesInput[131, (16)]*
    nodesInput[15, (16)]

  poutput$E194 = wshedInput[194, c(13:20)]*nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E195 = wshedInput[195, c(13:20)]*nodesInput[297, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E196 = wshedInput[196, c(13:20)]*nodesInput[296, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E197 = wshedInput[197, c(13:20)]*nodesInput[146, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E198 = wshedInput[198, c(13:20)]*nodesInput[66, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E199 = wshedInput[199, c(13:20)]*nodesInput[264, (16)]*
    nodesInput[66, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E200 = wshedInput[200, c(13:20)]*nodesInput[144, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E201 = wshedInput[201, c(13:20)]*nodesInput[168, (16)]*
    nodesInput[144, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  poutput$E202 = wshedInput[202, c(13:20)]*nodesInput[98, (16)]*
    nodesInput[168, (16)]*
    nodesInput[144, (16)]*
    nodesInput[123, (16)]*
    nodesInput[15, (16)]

  wphabAll$a189= poutput$E189[2:8]*allsp.hab(waterImpactedHab[189])
  wphabAB$a189 = poutput$E189[1]*ab.hab(waterImpactedHab[189])
  wphabAll$a190= poutput$E190[2:8]*allsp.hab(waterImpactedHab[190])
  wphabAB$a190 = poutput$E190[1]*ab.hab(waterImpactedHab[190])
  wphabAll$a191= poutput$E191[2:8]*allsp.hab(waterImpactedHab[191])
  wphabAB$a191 = poutput$E191[1]*ab.hab(waterImpactedHab[191])
  wphabAll$a192= poutput$E192[2:8]*allsp.hab(waterImpactedHab[192])
  wphabAB$a192= poutput$E192[1]*ab.hab(waterImpactedHab[192])
  wphabAll$a193= poutput$E193[2:8]*allsp.hab(waterImpactedHab[193])
  wphabAB$a193= poutput$E193[1]*ab.hab(waterImpactedHab[193])
  wphabAll$a194= poutput$E194[2:8]*allsp.hab(waterImpactedHab[194])
  wphabAB$a194 = poutput$E194[1]*ab.hab(waterImpactedHab[194])
  wphabAll$a195 = poutput$E195[2:8]*allsp.hab(waterImpactedHab[195])
  wphabAB$a195= poutput$E195[1]*ab.hab(waterImpactedHab[195])
  wphabAll$a196 = poutput$E196[2:8]*allsp.hab(waterImpactedHab[196])
  wphabAB$a196= poutput$E196[1]*ab.hab(waterImpactedHab[196])
  wphabAll$a197= poutput$E197[2:8]*allsp.hab(waterImpactedHab[197])
  wphabAB$a197= poutput$E197[1]*ab.hab(waterImpactedHab[197])
  wphabAll$a198= poutput$E198[2:8]*allsp.hab(waterImpactedHab[198])
  wphabAB$a198= poutput$E198[1]*ab.hab(waterImpactedHab[198])
  wphabAll$a199 = poutput$E199[2:8]*allsp.hab(waterImpactedHab[199])
  wphabAB$a199= poutput$E199[1]*ab.hab(waterImpactedHab[199])
  wphabAll$a200 = poutput$E200[2:8]*allsp.hab(waterImpactedHab[200])
  wphabAB$a200=poutput$E200[1]*ab.hab(waterImpactedHab[200])
  wphabAll$a201= poutput$E201[2:8]*allsp.hab(waterImpactedHab[201])
  wphabAB$a201= poutput$E201[1]*ab.hab(waterImpactedHab[201])
  wphabAll$a202= poutput$E202[2:8]*allsp.hab(waterImpactedHab[202])
  wphabAB$a202= poutput$E202[1]*ab.hab(waterImpactedHab[202])

  WshedHabAllsp$w029 = wphabAll$a189 + wphabAll$a190 +
    wphabAll$a191 + wphabAll$a192 + wphabAll$a193 + wphabAll$a194 + wphabAll$a195 +
    wphabAll$a196 + wphabAll$a197 + wphabAll$a198 + wphabAll$a199 + wphabAll$a200 +
    wphabAll$a201 + wphabAll$a202

  WshedHabABsp$w029 = wphabAB$a189 + wphabAB$a190+wphabAB$a191 +
    wphabAB$a192 + wphabAB$a193 + wphabAB$a194 + wphabAB$a195 +
    wphabAB$a196 + wphabAB$a197 + wphabAB$a198 + wphabAB$a199 + wphabAB$a200 +
    wphabAB$a201 + wphabAB$a202


  # --------------------Watershed 30 -------------------------------------------

  poutput$E203 = wshedInput[203, c(13:20)]*nodesInput[14, (16)]

  wphabAll$a203= poutput$E203[2:8]*allsp.hab(waterImpactedHab[203])
  wphabAB$a203 = poutput$E203[1]*ab.hab(waterImpactedHab[203])

  WshedHabAllsp$w030 =  wphabAll$a203
  WshedHabABsp$w030 = wphabAB$a203

  # --------------------Watershed 31 -------------------------------------------

  poutput$E204 = wshedInput[204, c(13:20)]*nodesInput[13, (16)]

  poutput$E205 = wshedInput[205, c(13:20)]*nodesInput[122, (16)]*
    nodesInput[13, (16)]

  # poutput$E206 =  list()
  # poutput$E206 = MISSING FROM GIS FILoutput$ES

  poutput$E207 = wshedInput[207, c(13:20)]*nodesInput[143, (16)]*
    nodesInput[122, (16)]*
    nodesInput[13, (16)]

  poutput$E208 = wshedInput[208, c(13:20)]*nodesInput[142, (16)]*
    nodesInput[122, (16)]*
    nodesInput[13, (16)]

  poutput$E209 = wshedInput[209, c(13:20)]*nodesInput[108, (16)]*
    nodesInput[142, (16)]*
    nodesInput[122, (16)]*
    nodesInput[13, (16)]

  poutput$E210 = wshedInput[210, c(13:20)]*nodesInput[266, (16)]*
    nodesInput[108, (16)]*
    nodesInput[142, (16)]*
    nodesInput[122, (16)]*
    nodesInput[13, (16)]

  wphabAll$a204= poutput$E204[2:8]*allsp.hab(waterImpactedHab[204])
  wphabAB$a204 = poutput$E204[1]*ab.hab(waterImpactedHab[204])
  wphabAll$a205 = poutput$E205[2:8]*allsp.hab(waterImpactedHab[205])
  wphabAB$a205= poutput$E205[1]*ab.hab(waterImpactedHab[205])
  wphabAll$a207= poutput$E207[2:8]*allsp.hab(waterImpactedHab[207])
  wphabAB$a207= poutput$E207[1]*ab.hab(waterImpactedHab[207])
  wphabAll$a208= poutput$E208[2:8]*allsp.hab(waterImpactedHab[208])
  wphabAB$a208= poutput$E208[1]*ab.hab(waterImpactedHab[208])
  wphabAll$a209 = poutput$E209[2:8]*allsp.hab(waterImpactedHab[209])
  wphabAB$a209= poutput$E209[1]*ab.hab(waterImpactedHab[209])
  wphabAll$a210 = poutput$E210[2:8]*allsp.hab(waterImpactedHab[210])
  wphabAB$a210=poutput$E210[1]*ab.hab(waterImpactedHab[210])

  WshedHabAllsp$w031 =  wphabAll$a204 + wphabAll$a205 +
    wphabAll$a207 + wphabAll$a208 + wphabAll$a209 + wphabAll$a210

  WshedHabABsp$w031 = wphabAB$a204 + wphabAB$a205 +
    wphabAB$a207 + wphabAB$a208 + wphabAB$a209 + wphabAB$a210


  # --------------------Watershed 32 -------------------------------------------

  poutput$E211 = wshedInput[211, c(13:20)]*nodesInput[309, (16)]

  poutput$E212 = wshedInput[212, c(13:20)]*nodesInput[120, (16)]*
    nodesInput[309, (16)]

  poutput$E213 = wshedInput[213, c(13:20)]*nodesInput[175, (16)]*
    nodesInput[120, (16)]*
    nodesInput[309, (16)]

  poutput$E214 = wshedInput[214, c(13:20)]*nodesInput[174, (16)]*
    nodesInput[120, (16)]*
    nodesInput[309, (16)]

  wphabAll$a211= poutput$E211[2:8]*allsp.hab(waterImpactedHab[211])
  wphabAB$a211= poutput$E211[1]*ab.hab(waterImpactedHab[211])
  wphabAll$a212= poutput$E212[2:8]*allsp.hab(waterImpactedHab[212])
  wphabAB$a212= poutput$E212[1]*ab.hab(waterImpactedHab[212])
  wphabAll$a213= poutput$E213[2:8]*allsp.hab(waterImpactedHab[213])
  wphabAB$a213= poutput$E213[1]*ab.hab(waterImpactedHab[213])
  wphabAll$a214 = poutput$E214[2:8]*allsp.hab(waterImpactedHab[214])
  wphabAB$a214=poutput$E214[1]*ab.hab(waterImpactedHab[214])

  WshedHabAllsp$w032 = wphabAll$a211 + wphabAll$a212+
    wphabAll$a213 + wphabAll$a214

  WshedHabABsp$w032 = wphabAB$a211 + wphabAB$a212 +
    wphabAB$a213 + wphabAB$a214
  # --------------------Watershed 33 -------------------------------------------

  poutput$E215 = wshedInput[215, c(13:20)]*nodesInput[12, (16)]

  wphabAll$a215= poutput$E215[2:8]*allsp.hab(waterImpactedHab[215])
  wphabAB$a215= poutput$E215[1]*ab.hab(waterImpactedHab[215])

  WshedHabAllsp$w033 =  wphabAll$a215
  WshedHabABsp$w033 = wphabAB$a215

  # --------------------Watershed 34 -------------------------------------------

  poutput$E216 = wshedInput[216, c(13:20)]*nodesInput[11, (16)]

  poutput$E217 = wshedInput[217, c(13:20)]*nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E218 = wshedInput[218, c(13:20)]*nodesInput[300, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E219 = wshedInput[219, c(13:20)]*nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E220 = wshedInput[220, c(13:20)]*nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E221 = wshedInput[221, c(13:20)]*nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E222 = wshedInput[222, c(13:20)]*nodesInput[147, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E223 = wshedInput[223, c(13:20)]*nodesInput[268, (16)]*
    nodesInput[138, (16)]*
    nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E224 = wshedInput[224, c(13:20)]*nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E225 = wshedInput[225, c(13:20)]*nodesInput[148, (16)]*
    nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E226 = wshedInput[226, c(13:20)]*nodesInput[138, (16)]*
    nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E227 = wshedInput[227, c(13:20)]*nodesInput[57, (16)]*
    nodesInput[148, (16)]*
    nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E228 = wshedInput[228, c(13:20)]*nodesInput[59, (16)]*
    nodesInput[147, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  poutput$E229 = wshedInput[229, c(13:20)]*nodesInput[58, (16)]*
    nodesInput[138, (16)]*
    nodesInput[158, (16)]*
    nodesInput[140, (16)]*
    nodesInput[141, (16)]*
    nodesInput[121, (16)]*
    nodesInput[11, (16)]

  wphabAll$a216= poutput$E216[2:8]*allsp.hab(waterImpactedHab[216])
  wphabAB$a216 = poutput$E216[1]*ab.hab(waterImpactedHab[216])
  wphabAll$a217= poutput$E217[2:8]*allsp.hab(waterImpactedHab[217])
  wphabAB$a217 = poutput$E217[1]*ab.hab(waterImpactedHab[217])
  wphabAll$a218= poutput$E218[2:8]*allsp.hab(waterImpactedHab[218])
  wphabAB$a218 = poutput$E218[1]*ab.hab(waterImpactedHab[218])
  wphabAll$a219= poutput$E219[2:8]*allsp.hab(waterImpactedHab[219])
  wphabAB$a219= poutput$E219[1]*ab.hab(waterImpactedHab[219])
  wphabAll$a220= poutput$E220[2:8]*allsp.hab(waterImpactedHab[220])
  wphabAB$a220= poutput$E220[1]*ab.hab(waterImpactedHab[220])
  wphabAll$a221= poutput$E221[2:8]*allsp.hab(waterImpactedHab[221])
  wphabAB$a221= poutput$E221[1]*ab.hab(waterImpactedHab[221])
  wphabAll$a222 = poutput$E222[2:8]*allsp.hab(waterImpactedHab[222])
  wphabAB$a222= poutput$E222[1]*ab.hab(waterImpactedHab[222])
  wphabAll$a223 = poutput$E223[2:8]*allsp.hab(waterImpactedHab[223])
  wphabAB$a223= poutput$E223[1]*ab.hab(waterImpactedHab[223])
  wphabAll$a224= poutput$E224[2:8]*allsp.hab(waterImpactedHab[224])
  wphabAB$a224= poutput$E224[1]*ab.hab(waterImpactedHab[224])
  wphabAll$a225= poutput$E225[2:8]*allsp.hab(waterImpactedHab[225])
  wphabAB$a225= poutput$E225[1]*ab.hab(waterImpactedHab[225])
  wphabAll$a226 = poutput$E226[2:8]*allsp.hab(waterImpactedHab[226])
  wphabAB$a226= poutput$E226[1]*ab.hab(waterImpactedHab[226])
  wphabAll$a227 = poutput$E227[2:8]*allsp.hab(waterImpactedHab[227])
  wphabAB$a227= poutput$E227[1]*ab.hab(waterImpactedHab[227])
  wphabAll$a228= poutput$E228[2:8]*allsp.hab(waterImpactedHab[228])
  wphabAB$a228= poutput$E228[1]*ab.hab(waterImpactedHab[228])
  wphabAll$a229= poutput$E229[2:8]*allsp.hab(waterImpactedHab[229])
  wphabAB$a229= poutput$E229[1]*ab.hab(waterImpactedHab[229])

  WshedHabAllsp$w034 = wphabAll$a216 + wphabAll$a217 +
    wphabAll$a218 + wphabAll$a219 + wphabAll$a220 + wphabAll$a221 + wphabAll$a222 +
    wphabAll$a223 + wphabAll$a224 + wphabAll$a225 + wphabAll$a226 +
    wphabAll$a227 + wphabAll$a228 + wphabAll$a229

  WshedHabABsp$w034 = wphabAB$a216 + wphabAB$a217 + wphabAB$a218 +
    wphabAB$a219 + wphabAB$a220+wphabAB$a221 + wphabAB$a222 +
    wphabAB$a223 + wphabAB$a224+wphabAB$a225 + wphabAB$a226 +
    wphabAB$a227 + wphabAB$a228+wphabAB$a229

  # --------------------Watershed 35 -------------------------------------------

  poutput$E230 = wshedInput[230, c(13:20)]*nodesInput[7, (16)]

  poutput$E231 = wshedInput[231, c(13:20)]*nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E232 = wshedInput[232, c(13:20)]*nodesInput[170, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E233 = wshedInput[233, c(13:20)]*nodesInput[161, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E234 = wshedInput[234, c(13:20)]*nodesInput[117, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E235 = wshedInput[235, c(13:20)]*nodesInput[150, (16)]*
    nodesInput[117, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E236 = wshedInput[236, c(13:20)]*nodesInput[151, (16)]*
    nodesInput[117, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E237 = wshedInput[237, c(13:20)]*nodesInput[269, (16)]*
    nodesInput[117, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  poutput$E238 = wshedInput[238, c(13:20)]*nodesInput[63, (16)]*
    nodesInput[151, (16)]*
    nodesInput[117, (16)]*
    nodesInput[96, (16)]*
    nodesInput[7, (16)]

  wphabAll$a230= poutput$E230[2:8]*allsp.hab(waterImpactedHab[230])
  wphabAB$a230= poutput$E230[1]*ab.hab(waterImpactedHab[230])
  wphabAll$a231= poutput$E231[2:8]*allsp.hab(waterImpactedHab[231])
  wphabAB$a231= poutput$E231[1]*ab.hab(waterImpactedHab[231])
  wphabAll$a232 = poutput$E232[2:8]*allsp.hab(waterImpactedHab[232])
  wphabAB$a232= poutput$E232[1]*ab.hab(waterImpactedHab[232])
  wphabAll$a233 = poutput$E233[2:8]*allsp.hab(waterImpactedHab[233])
  wphabAB$a233= poutput$E233[1]*ab.hab(waterImpactedHab[233])
  wphabAll$a234= poutput$E234[2:8]*allsp.hab(waterImpactedHab[234])
  wphabAB$a234= poutput$E234[1]*ab.hab(waterImpactedHab[234])
  wphabAll$a235= poutput$E235[2:8]*allsp.hab(waterImpactedHab[235])
  wphabAB$a235= poutput$E235[1]*ab.hab(waterImpactedHab[235])
  wphabAll$a236 = poutput$E236[2:8]*allsp.hab(waterImpactedHab[236])
  wphabAB$a236= poutput$E236[1]*ab.hab(waterImpactedHab[236])
  wphabAll$a237 = poutput$E237[2:8]*allsp.hab(waterImpactedHab[237])
  wphabAB$a237= poutput$E237[1]*ab.hab(waterImpactedHab[237])
  wphabAll$a238= poutput$E238[2:8]*allsp.hab(waterImpactedHab[238])
  wphabAB$a238= poutput$E238[1]*ab.hab(waterImpactedHab[238])

  WshedHabAllsp$w035 =  wphabAll$a230 + wphabAll$a231 + wphabAll$a232 +
    wphabAll$a233 + wphabAll$a234 + wphabAll$a235 + wphabAll$a236 +
    wphabAll$a237 + wphabAll$a238

  WshedHabABsp$w035 = wphabAB$a230 + wphabAB$a231 + wphabAB$a232 +
    wphabAB$a233 + wphabAB$a234 + wphabAB$a235 + wphabAB$a236 +
    wphabAB$a237 + wphabAB$a238


  # --------------------Watershed 36 -------------------------------------------

  poutput$E239 = wshedInput[239, c(13:20)]*nodesInput[10, (16)]

  poutput$E240 = wshedInput[240, c(13:20)]*nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E241 = wshedInput[241, c(13:20)]*nodesInput[303, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E242 = wshedInput[242, c(13:20)]*nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E243 = wshedInput[243, c(13:20)]*nodesInput[124, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E244 = wshedInput[244, c(13:20)]*nodesInput[302, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E245 = wshedInput[245, c(13:20)]*nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E246 = wshedInput[246, c(13:20)]*nodesInput[105, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E247 = wshedInput[247, c(13:20)]*nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E248 = wshedInput[248, c(13:20)]*nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E249 = wshedInput[249, c(13:20)]*nodesInput[295, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E250 = wshedInput[250, c(13:20)]*nodesInput[301, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E251 = wshedInput[251, c(13:20)]*nodesInput[118, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E252 = wshedInput[252, c(13:20)]*nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E253 = wshedInput[253, c(13:20)]*nodesInput[125, (16)]*
    nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E254 = wshedInput[254, c(13:20)]*nodesInput[270, (16)]*
    nodesInput[125, (16)]*
    nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E255 = wshedInput[255, c(13:20)]*nodesInput[155, (16)]*
    nodesInput[105, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E256 = wshedInput[256, c(13:20)]*nodesInput[156, (16)]*
    nodesInput[125, (16)]*
    nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E257 = wshedInput[257, c(13:20)]*nodesInput[160, (16)]*
    nodesInput[156, (16)]*
    nodesInput[125, (16)]*
    nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E258 = wshedInput[258, c(13:20)]*nodesInput[209, (16)]*
    nodesInput[105, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E259 = wshedInput[259, c(13:20)]*nodesInput[153, (16)]*
    nodesInput[209, (16)]*
    nodesInput[105, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E260 = wshedInput[260, c(13:20)]*nodesInput[211, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E261 = wshedInput[261, c(13:20)]*nodesInput[271, (16)]*
    nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E262 = wshedInput[262, c(13:20)]*nodesInput[276, (16)]*
    nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E263 = wshedInput[263, c(13:20)]*nodesInput[210, (16)]
    nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E264 = wshedInput[264, c(13:20)]*nodesInput[273, (16)]*
    nodesInput[211, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E265 = wshedInput[265, c(13:20)]*nodesInput[272, (16)]*
    nodesInput[276, (16)]*
    nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E266 = wshedInput[266, c(13:20)]*nodesInput[274, (16)]*
    nodesInput[211, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E267 = wshedInput[267, c(13:20)]*nodesInput[64, (16)]*
    nodesInput[160, (16)]*
    nodesInput[156, (16)]*
    nodesInput[125, (16)]*
    nodesInput[91, (16)]*
    nodesInput[90, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E268 = wshedInput[268, c(13:20)]*nodesInput[152, (16)]*
    nodesInput[211, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E269 = wshedInput[269, c(13:20)]*nodesInput[70, (16)]*
    nodesInput[152, (16)]*
    nodesInput[211, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]

  poutput$E270 = wshedInput[270, c(13:20)]*nodesInput[275, (16)]*
    nodesInput[210, (16)]*
    nodesInput[126, (16)]*
    nodesInput[87, (16)]*
    nodesInput[46, (16)]*
    nodesInput[10, (16)]


  wphabAll$a239= poutput$E239[2:8]*allsp.hab(waterImpactedHab[239])
  wphabAB$a239= poutput$E239[1]*ab.hab(waterImpactedHab[239])
  wphabAll$a240= poutput$E240[2:8]*allsp.hab(waterImpactedHab[240])
  wphabAB$a240= poutput$E240[1]*ab.hab(waterImpactedHab[240])
  wphabAll$a241= poutput$E241[2:8]*allsp.hab(waterImpactedHab[241])
  wphabAB$a241= poutput$E241[1]*ab.hab(waterImpactedHab[241])
  wphabAll$a242 = poutput$E242[2:8]*allsp.hab(waterImpactedHab[242])
  wphabAB$a242= poutput$E242[1]*ab.hab(waterImpactedHab[242])
  wphabAll$a243 = poutput$E243[2:8]*allsp.hab(waterImpactedHab[243])
  wphabAB$a243= poutput$E243[1]*ab.hab(waterImpactedHab[243])
  wphabAll$a244= poutput$E244[2:8]*allsp.hab(waterImpactedHab[244])
  wphabAB$a244= poutput$E244[1]*ab.hab(waterImpactedHab[244])
  wphabAll$a245= poutput$E245[2:8]*allsp.hab(waterImpactedHab[245])
  wphabAB$a245= poutput$E245[1]*ab.hab(waterImpactedHab[245])
  wphabAll$a246 = poutput$E246[2:8]*allsp.hab(waterImpactedHab[246])
  wphabAB$a246= poutput$E246[1]*ab.hab(waterImpactedHab[246])
  wphabAll$a247 = poutput$E247[2:8]*allsp.hab(waterImpactedHab[247])
  wphabAB$a247= poutput$E247[1]*ab.hab(waterImpactedHab[247])
  wphabAll$a248= poutput$E248[2:8]*allsp.hab(waterImpactedHab[248])
  wphabAB$a248= poutput$E248[1]*ab.hab(waterImpactedHab[248])
  wphabAll$a249= poutput$E249[2:8]*allsp.hab(waterImpactedHab[249])
  wphabAB$a249= poutput$E249[1]*ab.hab(waterImpactedHab[249])
  wphabAll$a250= poutput$E250[2:8]*allsp.hab(waterImpactedHab[250])
  wphabAB$a250= poutput$E250[1]*ab.hab(waterImpactedHab[250])
  wphabAll$a251= poutput$E251[2:8]*allsp.hab(waterImpactedHab[251])
  wphabAB$a251= poutput$E251[1]*ab.hab(waterImpactedHab[251])
  wphabAll$a252 = poutput$E252[2:8]*allsp.hab(waterImpactedHab[252])
  wphabAB$a252= poutput$E252[1]*ab.hab(waterImpactedHab[252])
  wphabAll$a253 = poutput$E253[2:8]*allsp.hab(waterImpactedHab[253])
  wphabAB$a253= poutput$E253[1]*ab.hab(waterImpactedHab[253])
  wphabAll$a254= poutput$E254[2:8]*allsp.hab(waterImpactedHab[254])
  wphabAB$a254= poutput$E254[1]*ab.hab(waterImpactedHab[254])
  wphabAll$a255= poutput$E255[2:8]*allsp.hab(waterImpactedHab[255])
  wphabAB$a255= poutput$E255[1]*ab.hab(waterImpactedHab[255])
  wphabAll$a256 = poutput$E256[2:8]*allsp.hab(waterImpactedHab[256])
  wphabAB$a256= poutput$E256[1]*ab.hab(waterImpactedHab[256])
  wphabAll$a257 = poutput$E257[2:8]*allsp.hab(waterImpactedHab[257])
  wphabAB$a257= poutput$E257[1]*ab.hab(waterImpactedHab[257])
  wphabAll$a258= poutput$E258[2:8]*allsp.hab(waterImpactedHab[258])
  wphabAB$a258= poutput$E258[1]*ab.hab(waterImpactedHab[258])
  wphabAll$a259= poutput$E259[2:8]*allsp.hab(waterImpactedHab[259])
  wphabAB$a259= poutput$E259[1]*ab.hab(waterImpactedHab[259])
  wphabAll$a260= poutput$E260[2:8]*allsp.hab(waterImpactedHab[260])
  wphabAB$a260= poutput$E260[1]*ab.hab(waterImpactedHab[260])
  wphabAll$a261= poutput$E261[2:8]*allsp.hab(waterImpactedHab[261])
  wphabAB$a261= poutput$E261[1]*ab.hab(waterImpactedHab[261])
  wphabAll$a262 = poutput$E262[2:8]*allsp.hab(waterImpactedHab[262])
  wphabAB$a262= poutput$E262[1]*ab.hab(waterImpactedHab[262])
  wphabAll$a263 = poutput$E263[2:8]*allsp.hab(waterImpactedHab[263])
  wphabAB$a263= poutput$E263[1]*ab.hab(waterImpactedHab[263])
  wphabAll$a264= poutput$E264[2:8]*allsp.hab(waterImpactedHab[264])
  wphabAB$a264= poutput$E264[1]*ab.hab(waterImpactedHab[264])
  wphabAll$a265= poutput$E265[2:8]*allsp.hab(waterImpactedHab[265])
  wphabAB$a265= poutput$E265[1]*ab.hab(waterImpactedHab[265])
  wphabAll$a266 = poutput$E266[2:8]*allsp.hab(waterImpactedHab[266])
  wphabAB$a266= poutput$E266[1]*ab.hab(waterImpactedHab[266])
  wphabAll$a267 = poutput$E267[2:8]*allsp.hab(waterImpactedHab[267])
  wphabAB$a267= poutput$E267[1]*ab.hab(waterImpactedHab[267])
  wphabAll$a268= poutput$E268[2:8]*allsp.hab(waterImpactedHab[268])
  wphabAB$a268= poutput$E268[1]*ab.hab(waterImpactedHab[268])
  wphabAll$a269= poutput$E269[2:8]*allsp.hab(waterImpactedHab[269])
  wphabAB$a269= poutput$E269[1]*ab.hab(waterImpactedHab[269])
  wphabAll$a270= poutput$E270[2:8]*allsp.hab(waterImpactedHab[270])
  wphabAB$a270= poutput$E270[1]*ab.hab(waterImpactedHab[270])

  WshedHabAllsp$w036 = wphabAll$a239 + wphabAll$a240 + wphabAll$a241 +
    wphabAll$a242 + wphabAll$a243 + wphabAll$a244 + wphabAll$a245 +
    wphabAll$a246 + wphabAll$a247 + wphabAll$a248 + wphabAll$a249 +
    wphabAll$a250 + wphabAll$a251 + wphabAll$a252 + wphabAll$a253 +
    wphabAll$a254 + wphabAll$a255 + wphabAll$a256 + wphabAll$a247 +
    wphabAll$a258 + wphabAll$a259 + wphabAll$a260 + wphabAll$a261 +
    wphabAll$a262 + wphabAll$a263 + wphabAll$a264 + wphabAll$a265 +
    wphabAll$a266 + wphabAll$a267 + wphabAll$a268 + wphabAll$a269 +
    wphabAll$a270

  WshedHabABsp$w036 = wphabAB$a239 + wphabAB$a240 + wphabAB$a241 +
    wphabAB$a242 + wphabAB$a243 + wphabAB$a244 + wphabAB$a245 +
    wphabAB$a246 + wphabAB$a247 + wphabAB$a248 + wphabAB$a249 +
    wphabAB$a250 + wphabAB$a251 + wphabAB$a252 + wphabAB$a253 +
    wphabAB$a254 + wphabAB$a255 + wphabAB$a256 + wphabAB$a247 +
    wphabAB$a258 + wphabAB$a259 + wphabAB$a260 + wphabAB$a261 +
    wphabAB$a262 + wphabAB$a263 + wphabAB$a264 + wphabAB$a265 +
    wphabAB$a266 + wphabAB$a267 + wphabAB$a268 + wphabAB$a269 +
    wphabAB$a270


  # --------------------Watershed 37 -------------------------------------------

  poutput$E271 = wshedInput[271, c(13:20)]*0

  wphabAll$a271= poutput$E271[2:8]*allsp.hab(waterImpactedHab[271])
  wphabAB$a271= poutput$E271[1]*ab.hab(waterImpactedHab[271])

  WshedHabAllsp$w037 = wphabAll$a271
  WshedHabABsp$w037 = wphabAB$a271

  # --------------------Watershed 38 -------------------------------------------

  poutput$E272 = wshedInput[272, c(13:20)]*nodesInput[6, (16)]

  poutput$E273 = wshedInput[273, c(13:20)]*nodesInput[82, (16)]*
    nodesInput[6, (16)]

  wphabAll$a272= poutput$E272[2:8]*allsp.hab(waterImpactedHab[272])
  wphabAB$a272= poutput$E272[1]*ab.hab(waterImpactedHab[272])
  wphabAll$a273= poutput$E273[2:8]*allsp.hab(waterImpactedHab[273])
  wphabAB$a273= poutput$E273[1]*ab.hab(waterImpactedHab[273])

  WshedHabAllsp$w038 = wphabAll$a272 + wphabAll$a273
  WshedHabABsp$w038 = wphabAB$a272 + wphabAB$a273

  # --------------------Watershed 39 -------------------------------------------

  poutput$E274 = wshedInput[274, c(13:20)]*nodesInput[9, (16)]

  poutput$E275 = wshedInput[275, c(13:20)]*nodesInput[79, (16)]*
    nodesInput[9, (16)]

  poutput$E276 = wshedInput[276, c(13:20)]*nodesInput[127, (16)]*
    nodesInput[79, (16)]*
    nodesInput[9, (16)]

  poutput$E277 = wshedInput[277, c(13:20)]*nodesInput[80, (16)]*
    nodesInput[79, (16)]*
    nodesInput[9, (16)]

  wphabAll$a274= poutput$E274[2:8]*allsp.hab(waterImpactedHab[274])
  wphabAB$a274= poutput$E274[1]*ab.hab(waterImpactedHab[274])
  wphabAll$a275= poutput$E275[2:8]*allsp.hab(waterImpactedHab[275])
  wphabAB$a275= poutput$E275[1]*ab.hab(waterImpactedHab[275])
  wphabAll$a276= poutput$E276[2:8]*allsp.hab(waterImpactedHab[276])
  wphabAB$a276= poutput$E276[1]*ab.hab(waterImpactedHab[276])
  wphabAll$a277= poutput$E277[2:8]*allsp.hab(waterImpactedHab[277])
  wphabAB$a277= poutput$E277[1]*ab.hab(waterImpactedHab[277])

  WshedHabAllsp$w039 = wphabAll$a274 + wphabAll$a275 + wphabAll$a276 +
    wphabAll$a277
  WshedHabABsp$w039 = wphabAB$a274 + wphabAB$a275 + wphabAB$a276 +
    wphabAB$a277
  # --------------------Watershed 40 -------------------------------------------

  poutput$E278 = wshedInput[278, c(13:20)]*nodesInput[311, (16)]

  poutput$E279 =  wshedInput[279, c(13:20)]*nodesInput[133, (16)]*
    nodesInput[311, (16)]

  poutput$E280 = wshedInput[280, c(13:20)]*nodesInput[104, (16)]*
    nodesInput[311, (16)]

  wphabAll$a278= poutput$E278[2:8]*allsp.hab(waterImpactedHab[278])
  wphabAB$a278= poutput$E278[1]*ab.hab(waterImpactedHab[278])
  wphabAll$a279= poutput$E279[2:8]*allsp.hab(waterImpactedHab[279])
  wphabAB$a279= poutput$E279[1]*ab.hab(waterImpactedHab[279])
  wphabAll$a280= poutput$E280[2:8]*allsp.hab(waterImpactedHab[280])
  wphabAB$a280= poutput$E280[1]*ab.hab(waterImpactedHab[280])

  WshedHabAllsp$w040 = wphabAll$a278 + wphabAll$a279 + wphabAll$a280

  WshedHabABsp$w040 = wphabAB$a278 + wphabAB$a279 + wphabAB$a280

  # --------------------Watershed 41 -------------------------------------------

  poutput$E281 = wshedInput[281, c(13:20)]*nodesInput[104, (16)]

  wphabAll$a281= poutput$E281[2:8]*allsp.hab(waterImpactedHab[281])
  wphabAB$a281= poutput$E281[1]*ab.hab(waterImpactedHab[281])

  WshedHabAllsp$w041 = wphabAll$a281
  WshedHabABsp$w041 = wphabAB$a281

  # --------------------Watershed 42 -------------------------------------------

  poutput$E282 = wshedInput[282, c(13:20)]*nodesInput[1, (16)]

  poutput$E283 = wshedInput[283, c(13:20)]*nodesInput[304, (16)]*
    nodesInput[1, (16)]

  poutput$E284 = wshedInput[284, c(13:20)]*nodesInput[305, (16)]*
    nodesInput[1, (16)]

  poutput$E285 = wshedInput[285, c(13:20)]*nodesInput[306, (16)]*
    nodesInput[1, (16)]

  poutput$E286 = wshedInput[286, c(13:20)]*nodesInput[169, (16)]*
    nodesInput[1, (16)]

  wphabAll$a282= poutput$E282[2:8]*allsp.hab(waterImpactedHab[282])
  wphabAB$a282= poutput$E282[1]*ab.hab(waterImpactedHab[282])
  wphabAll$a283= poutput$E283[2:8]*allsp.hab(waterImpactedHab[283])
  wphabAB$a283= poutput$E283[1]*ab.hab(waterImpactedHab[283])
  wphabAll$a284= poutput$E284[2:8]*allsp.hab(waterImpactedHab[284])
  wphabAB$a284= poutput$E284[1]*ab.hab(waterImpactedHab[284])
  wphabAll$a285= poutput$E285[2:8]*allsp.hab(waterImpactedHab[285])
  wphabAB$a285= poutput$E285[1]*ab.hab(waterImpactedHab[285])
  wphabAll$a286= poutput$E286[2:8]*allsp.hab(waterImpactedHab[286])
  wphabAB$a286= poutput$E286[1]*ab.hab(waterImpactedHab[286])

  WshedHabAllsp$w042 = wphabAll$a282 + wphabAll$a283 + wphabAll$a284 +
    wphabAll$a285 + wphabAll$a286

  WshedHabABsp$w042 = wphabAB$a282 + wphabAB$a283 + wphabAB$a284 +
    wphabAB$a285 + wphabAB$a286

  # --------------------Watershed 43 -------------------------------------------

  poutput$E287 = wshedInput[287, c(13:20)]*nodesInput[4, (16)]

  poutput$E288 = wshedInput[288, c(13:20)]*nodesInput[157, (16)]*
    nodesInput[4, (16)]

  poutput$E289 = wshedInput[289, c(13:20)]*nodesInput[162, (16)]*
    nodesInput[157, (16)]*
    nodesInput[4, (16)]

  poutput$E290 = wshedInput[290, c(13:20)]*nodesInput[163, (16)]*
    nodesInput[162, (16)]*
    nodesInput[157, (16)]*
    nodesInput[4, (16)]

  poutput$E291 = wshedInput[291, c(13:20)]*nodesInput[163, (16)]*
    nodesInput[162, (16)]*
    nodesInput[157, (16)]*
    nodesInput[4, (16)]

  poutput$E292 = wshedInput[292, c(13:20)]*nodesInput[189, (16)]*
    nodesInput[163, (16)]*
    nodesInput[162, (16)]*
    nodesInput[157, (16)]*
    nodesInput[4, (16)]

  poutput$E293 = wshedInput[293, c(13:20)]*nodesInput[219, (16)]*
    nodesInput[163, (16)]*
    nodesInput[162, (16)]*
    nodesInput[157, (16)]*
    nodesInput[4, (16)]

  wphabAll$a287= poutput$E287[2:8]*allsp.hab(waterImpactedHab[287])
  wphabAB$a287= poutput$E287[1]*ab.hab(waterImpactedHab[287])
  wphabAll$a288= poutput$E288[2:8]*allsp.hab(waterImpactedHab[288])
  wphabAB$a288= poutput$E288[1]*ab.hab(waterImpactedHab[288])
  wphabAll$a289= poutput$E289[2:8]*allsp.hab(waterImpactedHab[289])
  wphabAB$a289= poutput$E289[1]*ab.hab(waterImpactedHab[289])
  wphabAll$a290= poutput$E290[2:8]*allsp.hab(waterImpactedHab[290])
  wphabAB$a290= poutput$E290[1]*ab.hab(waterImpactedHab[290])
  wphabAll$a291= poutput$E291[2:8]*allsp.hab(waterImpactedHab[291])
  wphabAB$a291= poutput$E291[1]*ab.hab(waterImpactedHab[291])
  wphabAll$a292= poutput$E292[2:8]*allsp.hab(waterImpactedHab[292])
  wphabAB$a292= poutput$E292[1]*ab.hab(waterImpactedHab[292])
  wphabAll$a293= poutput$E293[2:8]*allsp.hab(waterImpactedHab[293])
  wphabAB$a293= poutput$E293[1]*ab.hab(waterImpactedHab[293])

  WshedHabAllsp$w043 = wphabAll$a287 + wphabAll$a288 + wphabAll$a289 +
    wphabAll$a290 + wphabAll$a291 + wphabAll$a292 + wphabAll$a293

  WshedHabABsp$w043 = wphabAB$a287 + wphabAB$a288 + wphabAB$a289 +
    wphabAB$a290 + wphabAB$a291 + wphabAB$a292 + wphabAB$a293



  # --------------------Watershed 44 -------------------------------------------

  poutput$E294 = wshedInput[294, c(13:20)]*nodesInput[5, (16)]

  poutput$E295 = wshedInput[295, c(13:20)]*nodesInput[308, (16)]*
    nodesInput[5, (16)]

  wphabAll$a294= poutput$E294[2:8]*allsp.hab(waterImpactedHab[294])
  wphabAB$a294= poutput$E294[1]*ab.hab(waterImpactedHab[294])
  wphabAll$a295= poutput$E295[2:8]*allsp.hab(waterImpactedHab[295])
  wphabAB$a295= poutput$E295[1]*ab.hab(waterImpactedHab[295])

  WshedHabAllsp$w044 = wphabAll$a294 + wphabAll$a295

  WshedHabABsp$w044 = wphabAB$a294 + wphabAB$a295

  # --------------------Watershed 45 -------------------------------------------

  poutput$E296 = wshedInput[296, c(13:20)]*nodesInput[3, (16)]

  poutput$E297 = wshedInput[297, c(13:20)]*nodesInput[84, (16)]*
    nodesInput[3, (16)]

  poutput$E298 = wshedInput[298, c(13:20)]*nodesInput[213, (16)]*
    nodesInput[84, (16)]*
    nodesInput[3, (16)]

  wphabAll$a296= poutput$E296[2:8]*allsp.hab(waterImpactedHab[296])
  wphabAB$a296= poutput$E296[1]*ab.hab(waterImpactedHab[296])
  wphabAll$a297= poutput$E297[2:8]*allsp.hab(waterImpactedHab[297])
  wphabAB$a297= poutput$E297[1]*ab.hab(waterImpactedHab[297])
  wphabAll$a298= poutput$E298[2:8]*allsp.hab(waterImpactedHab[298])
  wphabAB$a298= poutput$E298[1]*ab.hab(waterImpactedHab[298])

  WshedHabAllsp$w045 = wphabAll$a296 + wphabAll$a297 + wphabAll$a298

  WshedHabABsp$w045 = wphabAB$a296 + wphabAB$a297 + wphabAB$a298

  # --------------------Watershed 46 -------------------------------------------

  poutput$E299 = wshedInput[299, c(13:20)]*nodesInput[2, (16)]

  poutput$E300 = wshedInput[300, c(13:20)]*nodesInput[307, (16)]*
    nodesInput[2, (16)]

  poutput$E301 = wshedInput[301, c(13:20)]*nodesInput[85, (16)]*
    nodesInput[2, (16)]

  poutput$E302 = wshedInput[302, c(13:20)]*nodesInput[99, (16)]*
    nodesInput[85, (16)]*
    nodesInput[2, (16)]

  poutput$E303 = wshedInput[303, c(13:20)]*nodesInput[129, (16)]*
    nodesInput[99, (16)]*
    nodesInput[85, (16)]*
    nodesInput[2, (16)]

  poutput$E304 = wshedInput[304, c(13:20)]*nodesInput[145, (16)]*
    nodesInput[129, (16)]*
    nodesInput[99, (16)]*
    nodesInput[85, (16)]*
    nodesInput[2, (16)]

  poutput$E305 = wshedInput[305, c(13:20)]*nodesInput[76, (16)]*
    nodesInput[2, (16)]

  poutput$E306 = wshedInput[306, c(13:20)]*nodesInput[137, (16)]*
    nodesInput[2, (16)]

  poutput$E307 = wshedInput[307, c(13:20)]*nodesInput[277, (16)]*
    nodesInput[137, (16)]*
    nodesInput[2, (16)]

  poutput$E308 = wshedInput[308, c(13:20)]*nodesInput[65, (16)]*
    nodesInput[137, (16)]*
    nodesInput[2, (16)]

  wphabAll$a299= poutput$E299[2:8]*allsp.hab(waterImpactedHab[299])
  wphabAB$a299= poutput$E299[1]*ab.hab(waterImpactedHab[299])
  wphabAll$a300= poutput$E300[2:8]*allsp.hab(waterImpactedHab[300])
  wphabAB$a300= poutput$E300[1]*ab.hab(waterImpactedHab[300])
  wphabAll$a301= poutput$E301[2:8]*allsp.hab(waterImpactedHab[301])
  wphabAB$a301= poutput$E301[1]*ab.hab(waterImpactedHab[301])
  wphabAll$a302 = poutput$E302[2:8]*allsp.hab(waterImpactedHab[302])
  wphabAB$a302= poutput$E302[1]*ab.hab(waterImpactedHab[302])
  wphabAll$a303 = poutput$E303[2:8]*allsp.hab(waterImpactedHab[303])
  wphabAB$a303= poutput$E303[1]*ab.hab(waterImpactedHab[303])
  wphabAll$a304= poutput$E304[2:8]*allsp.hab(waterImpactedHab[304])
  wphabAB$a304= poutput$E304[1]*ab.hab(waterImpactedHab[304])
  wphabAll$a305= poutput$E305[2:8]*allsp.hab(waterImpactedHab[305])
  wphabAB$a305= poutput$E305[1]*ab.hab(waterImpactedHab[305])
  wphabAll$a306 = poutput$E306[2:8]*allsp.hab(waterImpactedHab[306])
  wphabAB$a306= poutput$E306[1]*ab.hab(waterImpactedHab[306])
  wphabAll$a307 = poutput$E307[2:8]*allsp.hab(waterImpactedHab[307])
  wphabAB$a307= poutput$E307[1]*ab.hab(waterImpactedHab[307])
  wphabAll$a308= poutput$E308[2:8]*allsp.hab(waterImpactedHab[308])
  wphabAB$a308= poutput$E308[1]*ab.hab(waterImpactedHab[308])


  WshedHabAllsp$w046 = wphabAll$a299 + wphabAll$a300 + wphabAll$a301 +
    wphabAll$a302 + wphabAll$a303 + wphabAll$a304 + wphabAll$a305 +
    wphabAll$a306 + wphabAll$a307 + wphabAll$a308

  WshedHabABsp$w046 = wphabAB$a299 + wphabAB$a300 + wphabAB$a301 +
    wphabAB$a302 + wphabAB$a303 + wphabAB$a304 + wphabAB$a305 +
    wphabAB$a306 + wphabAB$a307 + wphabAB$a308

  # --------------------Watershed 47 -------------------------------------------

  poutput$E600 = wshedInput[322, c(13:20)]*nodesInput[33, 15]

  wphabAll$a600 = poutput$E600[2:8]*allsp.hab(waterImpactedHab[33])
  wphabAb$a600 = poutput$E600[1]*ab.hab(waterImpactedHab[33])

  WshedHabAllsp$w047 = wphabAll$a600
  WshedhabABsp$w047 = wphabAB$a600

  # -------------------------------------------------------------------
  # Save output in dataframe
  output = list("WsedHabAllSp" = WshedHabAllsp, "WshedHabAB" = WshedHabABsp,
                "WaterPassageHabAllSp" = wphabAll,
                "WaterPassageHabAB" = wphabAB,
                "PassageEffectedHab" = poutput)
  return(output)

}




