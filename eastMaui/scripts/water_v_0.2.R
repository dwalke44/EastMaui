#' @export
# Function to calculate amount of water remaining in basins after diversion

# convert all nodeInputs[..., c(8:11)] to nodeInputs[..., 2]
basinWater.fun = function(nodesInput, waterInput){
  inStream = list()
  spsk = list()
  inDitch = list()
  inWshed = list()
  # bfPercent = list()
  WshedCheck = list()

# ------------------ Watershed 1 -------------------------------------

  inStream$E003 = waterInput[3, 3]

  inStream$E004 = waterInput[4, 3]

  inStream$E002 = waterInput[2, 3] +
    inStream$E003*nodesInput[228, 3] +
    inStream$E004*nodesInput[196, 3]

  # node 49 = sink
  spsk$E001s = (inStream$E002*nodesInput[49, 3] + nodesInput[49, 13])

  spsk$E001 = ifelse(spsk$E001s<0, 0, spsk$E001s)

  inStream$E001 = waterInput[1,3] + spsk$E001

  inDitch$n228 = inStream$E003*nodesInput[228, 2]

  inDitch$n196 = inStream$E004*nodesInput[196, 2]

  inDitch$n49 = inStream$E002 - spsk$E001

  inWshed$w1 = inStream$E001

  # bfPercent$b3 = inStream$E003/waterInput[3, 3]
  # bfPercent$b4 = inStream$E004/waterInput[4, 3]
  # bfPercent$b2 = inStream$E002/(waterInput[2, 3]+waterInput[4, 3]+waterInput[3, 3])
  # bfPercent$b1 =

  WshedCheck$w1 = (waterInput[3, 3] +
                     waterInput[4, 3] +
                     waterInput[2, 3] +
                     waterInput[1,3]) -
    (spsk$E001 + inDitch$n196 + inDitch$n228 +inDitch$n49 + inWshed$w1)


  # ------------------ Watershed 2 -------------------------------------


  inStream$E008 = waterInput[8, 3]

  inStream$E009 = waterInput[9, 3]

  inStream$E010 = waterInput[10, 3]

  inDitch$n192 = inStream$E009*nodesInput[192, 2]

  inDitch$n193 = inStream$E008*nodesInput[193, 2]

  inDitch$n194 = inStream$E010*nodesInput[194, 2]

  inStream$E007 = waterInput[7, 3] + inStream$E009*nodesInput[192, 3] + inStream$E008*nodesInput[193, 3] + inStream$E010*nodesInput[194, 3]

  inDitch$n229 = inStream$E007*nodesInput[229, 2]

  inStream$E006 = waterInput[6, 3] + inStream$E007*nodesInput[229, 3]
  # node 50 = spring
  spsk$E005s =  (nodesInput[50,13] + inStream$E006*nodesInput[50,3])

  spsk$E005 = ifelse(spsk$E005s<0, 0, spsk$E005s)

  inStream$E005 = waterInput[5, 3] + spsk$E005

  inDitch$n50 = inStream$E006 - spsk$E005

  inWshed$w2 = inStream$E005

  WshedCheck$w2 = (waterInput[8, 3] + waterInput[9, 3] + waterInput[10, 3] +  waterInput[7,3] +  waterInput[6, 3] +  waterInput[5,3]+nodesInput[50,13]) -(inDitch$n192 + inDitch$n193 +inDitch$n194+ inDitch$n229 + inWshed$w2)

  # ------------------ Watershed 3 -------------------------------------

  inStream$E014 = waterInput[14, 3]

  inStream$E015 = waterInput[15, 3]

  inDitch$n187 = inStream$E015*nodesInput[187,2]

  inStream$E016 = waterInput[16, 3] + inStream$E015*nodesInput[187, 3]

  inDitch$n188 = inStream$E016*nodesInput[188, 2]

  inStream$E013 = waterInput[13, 3] + inStream$E016*nodesInput[188, 3]

  inStream$E012 = waterInput[12, 3] +
    inStream$E013*nodesInput[195, 3] +
    inStream$E014*nodesInput[186, 3]

  inDitch$n195 = inStream$E013*nodesInput[195, 2]
  inDitch$n186 = inStream$E014*nodesInput[186, 2]

  # node 51 = spring

  spsk$E011s =  (nodesInput[51,13] + inStream$E012*nodesInput[51,3])

  spsk$E011 = ifelse(spsk$E011s<0, 0, spsk$E011s)

  inStream$E011 = waterInput[11, 3] + spsk$E011

  inWshed$w3 = inStream$E011

  WshedCheck$w3 = (waterInput[12, 3] + waterInput[11, 3] + waterInput[13, 3] +  waterInput[14,3] +  waterInput[15, 3] +  waterInput[16,3] + nodesInput[51,13]) -(inDitch$n186 + inDitch$n187 +inDitch$n188+ inDitch$n195 + inWshed$w3)

  # ------------------ Watershed 4 -------------------------------------

  inStream$E018 = waterInput[18, 3]

  inDitch$n230 = inStream$E018*nodesInput[230, 2]

  inStream$E017 = waterInput[17, 3] +  inStream$E018*nodesInput[230, 3]

  inWshed$w4 = inStream$E017

  WshedCheck$w4 = (waterInput[17, 3] + waterInput[18, 3]) - (inDitch$n230 + inWshed$w4)

  # ------------------ Watershed 5 -------------------------------------

  inStream$E020 = waterInput[20, 3]

  inStream$E021 = waterInput[21, 3]

  inStream$E022 = waterInput[22, 3]

  inStream$E023 = waterInput[23, 3]

  inDitch$n232 = inStream$E020*nodesInput[232, 2]

  inDitch$n185 = inStream$E022*nodesInput[185, 2]

  inDitch$n231 = inStream$E021*nodesInput[231, 2]

  inDitch$n184 = inStream$E023*nodesInput[184, 2]

  inStream$E019 = waterInput[19, 3] +
    inStream$E023*nodesInput[184, 3]+
    inStream$E022*nodesInput[185, 3]+
    inStream$E021*nodesInput[231, 3]+
    inStream$E020*nodesInput[232, 3]

  inWshed$w5 = inStream$E019

  WshedCheck$w5 = (waterInput[19, 3] + waterInput[20, 3] + waterInput[21, 3] + waterInput[22, 3] + waterInput[23, 3]) - (inDitch$n232 + inDitch$n185 + inDitch$n231 + inDitch$n184 + inWshed$w5)

  # ------------------ Watershed 6 -------------------------------------

  inStream$E025 = waterInput[25, 3]

  inStream$E026 = waterInput[26, 3]

  inStream$E027 = waterInput[27, 3]

  inStream$E028 = waterInput[28, 3]

  inDitch$n233 = inStream$E025* nodesInput[223, 2]

  inDitch$n234 = inStream$E026* nodesInput[224, 2]

  inDitch$n182 = inStream$E027* nodesInput[182, 2]

  inDitch$n183 = inStream$E028* nodesInput[183, 2]

  inStream$E024 = waterInput[24, 3] +
    inStream$E025*nodesInput[233, 3] +
    inStream$E026*nodesInput[234, 3] +
    inStream$E027*nodesInput[182, 3]+
    inStream$E028*nodesInput[183, 3]

  inWshed$w6 = inStream$E024

  WshedCheck$w6 = (waterInput[24, 3] + waterInput[25, 3] + waterInput[26, 3] + waterInput[27, 3] + waterInput[28, 3]) - (inDitch$n182 + inDitch$n183 + inDitch$n233 + inDitch$n234 + inWshed$w6)

  # ------------------ Watershed 7 -------------------------------------
  inStream$E030 = waterInput[30, 3]

  inStream$E031 = waterInput[31, 3]

  inStream$E033 = waterInput[33, 3]

  inStream$E034 = waterInput[34, 3]

  inDitch$n180 = inStream$E033*nodesInput[180, 2]

  inDitch$n191 = inStream$E034*nodesInput[191, 2]

  inDitch$n237 = inStream$E030*nodesInput[237,2]

  inDitch$n236 = inStream$E031*nodesInput[236, 2]

  inStream$E032 = waterInput[32, 3]  + inStream$E033*nodesInput[180, 3]

  inDitch$n235 = inStream$E032*nodesInput[235, 2]

  inStream$E029 = waterInput[29, 3] +
    inStream$E030*nodesInput[237, 3] +
    inStream$E031*nodesInput[236, 3] +
    inStream$E032*nodesInput[235, 3]+
    inStream$E034*nodesInput[191, 3]

  inWshed$w7 = inStream$E029

  WshedCheck$w7 = (waterInput[29, 3] + waterInput[30, 3] + waterInput[31, 3] + waterInput[32, 3] + waterInput[33, 3] + waterInput[34,3]) - (inDitch$n191 + inDitch$n237 + inDitch$n236 + inDitch$n235 + inDitch$n180 + inWshed$w7)

  # ------------------ Watershed 8 -------------------------------------

  inStream$E036 = waterInput[36, 3]

  inDitch$n190 = inStream$E036*nodesInput[190, 2]

  inStream$E035 = waterInput[35, 3] + inStream$E036*nodesInput[190, 3]

  inWshed$w8 = inStream$E035

  WshedCheck$w8 = (waterInput[36, 3] + waterInput[35, 3]) - (inDitch$n190 + inWshed$w8)

  # ------------------ Watershed 9 -------------------------------------

  inStream$E038 = waterInput[38, 3]

  inDitch$n223 = inStream$E038*nodesInput[223, 2]

  inStream$E037 = waterInput[37, 3] + inStream$E038*nodesInput[223, 3]

  inWshed$w9 = inStream$E037

  WshedCheck$w9 = (waterInput[38, 3] + waterInput[37, 3]) - (inDitch$n223 + inWshed$w9)

  # ------------------ Watershed 10 ------------------------------------
  inStream$E040 = waterInput[40, 3]

  inStream$E041 = waterInput[41, 3]

  inStream$E042 = waterInput[42, 3]

  inStream$E043 = waterInput[43, 3]

  inDitch$n227 = inStream$E043*nodesInput[227, 2]

  inDitch$n220 = inStream$E041*nodesInput[220, 2]

  inDitch$n218 = inStream$E040*nodesInput[218, 2]

  inDitch$n238 = inStream$E042*nodesInput[238, 2]

  inStream$E044 = waterInput[44 ,3]  + inStream$E042*nodesInput[238, 3]

  inDitch$n217 = inStream$E044*nodesInput[217, 2]

  inStream$E039 = waterInput[39, 3] +
    inStream$E040*nodesInput[218, 3] +
    inStream$E041*nodesInput[220, 3] +
    inStream$E043*nodesInput[227, 3] +
    inStream$E044*nodesInput[217, 3]

  inWshed$w10 = inStream$E039

  WshedCheck$w10 = (waterInput[39, 3] + waterInput[40, 3] + waterInput[41, 3] + waterInput[42, 3] + waterInput[43, 3] + waterInput[44, 3]) -
    (inDitch$n227 + inDitch$n220 + inDitch$n218 + inDitch$n217 + inDitch$n238 + inWshed$w10)


  # ------------------ Watershed 11 ------------------------------------
  inStream$E047 = waterInput[47,3]

  inStream$E048 = waterInput[48, 3]

  inStream$E049 = waterInput[49, 3]

  inStream$E050 = waterInput[50, 3]

  inStream$E051 = waterInput[51, 3]

  inStream$E052 = waterInput[52, 3]

  inStream$E053 = waterInput[53, 3]

  inStream$E054 = waterInput[54, 3]

  inDitch$n222 = inStream$E052*nodesInput[222, 2]

  inDitch$n239 = inStream$E048*nodesInput[239, 2]

  inDitch$n240 = inStream$E047*nodesInput[240, 2]

  inDitch$n241 = inStream$E049*nodesInput[241, 2]

  inDitch$n242 = inStream$E050*nodesInput[242, 2]

  inDitch$n243 = inStream$E051*nodesInput[243, 2]

  inDitch$n225 = inStream$E053*nodesInput[225, 2]

  # basin 54 has two diversions on it
  inDitch$n216 = inStream$E054*nodesInput[216, 2]
  inStream$E0541 = inStream$E054*nodesInput[216, 3]

  inDitch$n224 = inStream$E0541*nodesInput[224, 2]

  inStream$E046 = waterInput[46 ,3] +
    inStream$E0541*nodesInput[224, 3] +
    inStream$E053*nodesInput[225,3] +
    inStream$E051*nodesInput[243, 3] +
    inStream$E050*nodesInput[242, 3] +
    inStream$E049*nodesInput[241, 3] +
    inStream$E047*nodesInput[240, 3] +
    inStream$E048*nodesInput[239, 3] +
    inStream$E052*nodesInput[222, 3]

  # node 54 = sink
  spsk$E046s =  (inStream$E046*nodesInput[54, 3] + nodesInput[54,13])
  spsk$E046 = ifelse(spsk$E046s<0 , 0, spsk$E046s)

  inDitch$n054 = inStream$E046 - spsk$E046

  inStream$E045 = waterInput[45 ,3] + spsk$E046

  inWshed$w011 = inStream$E045

  WshedCheck$w011 = (waterInput[45, 3] + waterInput[46, 3] +
                      waterInput[47, 3] + waterInput[48, 3] +
                      waterInput[49, 3] + waterInput[50, 3] +
                      waterInput[51, 3] + waterInput[52, 3] +
                      waterInput[53, 3] + waterInput[54, 3]) -
    (inDitch$n222 + inDitch$n225 + inDitch$n239 + inDitch$n240 +
       inDitch$n241 + inDitch$n242 + inDitch$n243 + inDitch$n216 +
       inDitch$n224 + inDitch$n054 + spsk$E046 + inWshed$w011)

  # ------------------ Watershed 12 ------------------------------------

  inStream$E059 = waterInput[59 ,3]

  inStream$E060 = waterInput[60 ,3]

  inStream$E061 = waterInput[61 ,3]

  inStream$E062 = waterInput[62 ,3]

  inStream$E063 = waterInput[63 ,3]

  inStream$E064 = waterInput[64 ,3]

  inDitch$n207 = inStream$E059*nodesInput[207,2]

  inDitch$n205 = inStream$E062*nodesInput[205, 2]

  inDitch$n208 = inStream$E060*nodesInput[208, 2]

  inDitch$n215 = inStream$E061*nodesInput[215, 2]

  inDitch$n214 = inStream$E063*nodesInput[214, 2]

  inDitch$n226 = inStream$E064*nodesInput[226, 2]

  inStream$E058 = waterInput[58 ,3] +
    inStream$E063*nodesInput[214, 3] +
    inStream$E061*nodesInput[215, 3] +
    inStream$E060*nodesInput[208, 3] +
    inStream$E059*nodesInput[207, 3] +
    inStream$E062*nodesInput[205, 3]

  inStream$E056 = waterInput[56 ,3] + inStream$E064*nodesInput[226, 3]

  # node 53 = sink
  spsk$E058s = inStream$E058*nodesInput[53, 3] + nodesInput[53, 13]
  spsk$E058 = ifelse(spsk$E058s<0, 0, spsk$E028s)
  inStream$E057 = waterInput[57 ,3] + spsk$E058
  inDitch$n053 = inStream$E058 - spsk$E058

  # node 52 = sink
  spsk$E056s = inStream$E056*nodesInput[52,3] + nodesInput[52,13]
  spsk$E056 = ifelse(spsk$E056s<0, 0, spsk$E056s)

  inStream$E055 =
    waterInput[55 ,3] +
    spsk$E056 +
    inStream$E057*nodesInput[47,3]

  inDitch$n052 = inStream$E056 - spsk$E056

  inDitch$n047 = inStream$E057*nodesInput[47,2]

  inWshed$w012 = inStream$E055

  WshedCheck$w012 = (waterInput[55 ,3] + waterInput[56 ,3] +
                      waterInput[57 ,3] + waterInput[58 ,3] +
                      waterInput[59 ,3] + waterInput[60 ,3] +
                      waterInput[61 ,3] + waterInput[62 ,3] +
                      waterInput[63 ,3] + waterInput[64 ,3]
                    ) -
    (inDitch$n205 + inDitch$n207 + inDitch$n208 + inDitch$n215 +
       inDitch$n214 + inDitch$n226 + inDitch$n053 + inDitch$n052 +
       inDitch$n047 + inWshed$w012 + spsk$E056 + spsk$E058)

  # ------------------ Watershed 13 ------------------------------------

  inStream$E066 = waterInput[66 ,3]

  inStream$E067 = waterInput[67 ,3]

  inStream$E068 = waterInput[68 ,3]

  inStream$E069 = waterInput[69 ,3]

  inStream$E070 = waterInput[70 ,3]

  inDitch$n247 = inStream$E068*nodesInput[247, 2]

  inDitch$n221 = inStream$E070*nodesInput[221, 2]

  inDitch$n246 = inStream$E069*nodesInput[246, 2]

  inDitch$n245 = inStream$E067*nodesInput[245, 2]

  inDitch$n244 = inStream$E066*nodesInput[244, 2]

  inStream$E065 = waterInput[65 ,3] +
    inStream$E066*nodesInput[244, 3] +
    inStream$E067*nodesInput[245, 3] +
    inStream$E068*nodesInput[247, 3] +
    inStream$E069*nodesInput[246, 3] +
    inStream$E070*nodesInput[221, 3]

  inWshed$w013 = inStream$E065

  WshedCheck$w013 = (waterInput[65, 3] + waterInput[66, 3] +
                      waterInput[67, 3] + waterInput[68, 3] +
                      waterInput[69, 3] + waterInput[70, 3]
                    ) -
    (inDitch$n247 + inDitch$n221 + inDitch$n246 + inDitch$n245 +
       inDitch$n244 + inWshed$w013)

  # ------------------ Watershed 14 ------------------------------------
  inStream$E073 = waterInput[73 ,3]

  inStream$E074 = waterInput[74 ,3]

  inStream$E075 = waterInput[75 ,3]

  inStream$E076 = waterInput[76 ,3]

  # basin 77 has two diversions

  inDitch$n200 = waterInput[77, 3]*nodesInput[200, 2]
  inStream$E0771 = waterInput[77, 3]*nodesInput[200, 3]
  inDitch$n248 = inStream$E0771*nodesInput[248, 2]

  inStream$E078 = waterInput[78, 3]

  inDitch$n206 = inStream$E075*nodesInput[206, 2]
  inDitch$n249 = inStream$E073*nodesInput[249, 2]
  inDitch$n199 = inStream$E076*nodesInput[199, 2]
  inDitch$n181 = inStream$E078*nodesInput[181, 2]
  inDitch$n198 = inStream$E074*nodesInput[198, 2]

  inStream$E072 = waterInput[72, 3] + inStream$E075*nodesInput[206, 3]

  inDitch$n281 = inStream$E072*nodesInput[281, 2]

  inStream$E071 = waterInput[71 ,3] +
    inStream$E072*nodesInput[281, 3] +
    inStream$E073*nodesInput[249, 3] +
    inStream$E074*nodesInput[198, 3] +
    inStream$E076*nodesInput[199, 3] +
    inStream$E078*nodesInput[181, 3] +
    inStream$E0771*nodesInput[248, 3]

  inWshed$w014 = inStream$E071

  WshedCheck$w014 = (waterInput[71, 3] + waterInput[72,3] +
                      waterInput[73, 3] + waterInput[74,3] +
                      waterInput[75, 3] + waterInput[76,3] +
                      waterInput[77, 3] + waterInput[78,3]
                    ) -
    (inDitch$n206 + inDitch$n249 + inDitch$n200 + inDitch$n248 +
       inDitch$n199 + inDitch$n181 + inDitch$n198 + inDitch$n281 +
       inWshed$w014)

  # ------------------ Watershed 15 ------------------------------------


  inStream$E080 = waterInput[80 ,3]

  inStream$E083 = waterInput[83 ,3]

  inStream$E084 = waterInput[84 ,3]

  inStream$E087 = waterInput[87 ,3]

  inStream$E088 = waterInput[88 ,3]

  inStream$E089 = waterInput[89 ,3]

  inDitch$n197 = inStream$E089*nodesInput[197, 2]
  inDitch$n251 = inStream$E088*nodesInput[251, 2]
  inDitch$n250 = inStream$E087*nodesInput[250, 2]
  inDitch$n280 = inStream$E083*nodesInput[280,2]
  inDitch$n278 = inStream$E084*nodesInput[278,2]

  inStream$E086 = waterInput[86 ,3] + inStream$E088*nodesInput[251, 3]

  inDitch$n252 = inStream$E086*nodesInput[252,2]

  inStream$E085 = waterInput[85 ,3] +
    inStream$E089*nodesInput[197, 3] +
    inStream$E086*nodesInput[252, 3] +
    inStream$E087*nodesInput[250, 3]

  inStream$E082 = waterInput[82 ,3] +
    inStream$E083*nodesInput[280, 3] +
    inStream$E084*nodesInput[278, 3] +
    inStream$E085*nodesInput[111, 3]

  inDitch$n111 = inStream$E085*nodesInput[111, 2]

  inStream$E081 = waterInput[81 ,3] + inStream$E082*nodesInput[113, 3]

  inDitch$n113 = inStream$E082*nodesInput[113, 2]
  inDitch$n285 = inStream$E081*nodesInput[285,2]
  inDitch$n284 = inStream$E080*nodesInput[284, 2]

  inStream$E079 = waterInput[79 ,3] +
    inStream$E080*nodesInput[284, 3] +
    inStream$E081*nodesInput[285, 3]

  inWshed$w15 = inStream$E079

  WshedCheck$w15 = (waterInput[79, 3] + waterInput[80, 3] +
                      waterInput[81, 3] + waterInput[82, 3] +
                      waterInput[83, 3] + waterInput[84, 3] +
                      waterInput[85, 3] + waterInput[86, 3] +
                      waterInput[87, 3] + waterInput[88, 3] +
                      waterInput[89, 3]
                      ) -
    (inDitch$n284 + inDitch$n285 + inDitch$n113 + inDitch$n280 + inDitch$n278 +
       inDitch$n111 + inDitch$n197 + inDitch$n252 + inDitch$n251 +
       inDitch$n250 + inWshed$w15)

  # ------------------ Watershed 16 ------------------------------------
  inStream$E093  = waterInput[93 ,3]

  inDitch$n072 = inStream$E093*nodesInput[72,2]

  inStream$E092 = waterInput[92 ,3] + inStream$E093*nodesInput[72, 3]

  inDitch$n279 = inStream$E092*nodesInput[279, 2]

  inStream$E091 = waterInput[91 ,3]+ inStream$E092*nodesInput[279, 3]

  inDitch$n119 = inStream$E091*nodesInput[119, 2]

  inStream$E090 = waterInput[90 ,3] + inStream$E091*nodesInput[119, 3]

  inWshed$w016 = inStream$E090

  WshedCheck$w016 = (waterInput[90, 3] + waterInput[91, 3] +
                       waterInput[92, 3] + waterInput[93, 3]
                     ) -
    (inDitch$n072 + inDitch$n279 + inDitch$n119 + inWshed$w016)

  # ------------------ Watershed 17 ------------------------------------

  inStream$E096 = waterInput[96 ,3]

  inStream$E097 = waterInput[97 ,3]

  inStream$E098 = waterInput[98 ,3]

  inDitch$n256 = inStream$E096*nodesInput[256, 2]
  inDitch$n255 = inStream$E097*nodesInput[255, 2]
  inDitch$n254 = inStream$E098*nodesInput[254, 2]

  inStream$E102 = waterInput[102,3]

  inDitch$n073 = inStream$E102*nodesInput[73, 2]

  inStream$E100 = waterInput[100,3]

  inDitch$n253 = inStream$E100*nodesInput[253, 2]

  inStream$E095 = waterInput[95 ,3] +
    inStream$E096*nodesInput[256, 3] +
    inStream$E097*nodesInput[255, 3] +
    inStream$E098*nodesInput[254, 3]

  inDitch$n109 = inStream$E095*nodesInput[109, 2]

  inStream$E101 = waterInput[101,3] + inStream$E102*nodesInput[73, 3]

  inDitch$n201 = inStream$E101*nodesInput[201, 2]

  inStream$E099 = waterInput[99 ,3] +
    inStream$E100*nodesInput[253, 3] +
    inStream$E101*nodesInput[201, 3]

  inDitch$n100 = inStream$E099*nodesInput[99, 2]

  inStream$E094 = waterInput[94 ,3] +
    inStream$E095*nodesInput[109, 3] +
    inStream$E099*nodesInput[100, 3]

  inWshed$w017 = inStream$E094

  WshedCheck$w017 = (waterInput[94, 3] + waterInput[95, 3] +
                      waterInput[96, 3] + waterInput[97, 3] +
                      waterInput[98, 3] + waterInput[99, 3] +
                      waterInput[100, 3] + waterInput[101, 3] +
                      waterInput[102, 3]
                    ) -
    (inDitch$n253 + inDitch$n254 + inDitch$n255 + inDitch$n256 + inDitch$n073 +
       inDitch$n201 + inDitch$n109 + inDitch$n100 + inWshed$w017)

  # ------------------ Watershed 18 ------------------------------------
  inStream$E104 = waterInput[104,3]

  inStream$E105 = waterInput[105,3]

  inStream$E106 = waterInput[106,3]

  inStream$E107 = waterInput[107,3]

  inStream$E350 = waterInput[309, 3]

  inDitch$n287 = inStream$E350*nodesInput[287, 2]
  inDitch$n289 = inStream$E104*nodesInput[289, 2]
  inDitch$n112 = inStream$E107*nodesInput[112, 2]
  inDitch$n286 = inStream$E105*nodesInput[286, 2]
  inDitch$n288 = inStream$E106*nodesInput[288, 2]

  inStream$E103 = waterInput[103,3] +
    inStream$E104*nodesInput[289, 3] +
    inStream$E105*nodesInput[286, 3] +
    inStream$E350*nodesInput[287, 3] +
    inStream$E106*nodesInput[288, 3] +
    inStream$E107*nodesInput[112, 3]

  inWshed$w018 = inStream$E103

  WshedCheck$w018 = (waterInput[103, 3] + waterInput[104, 3] +
                       waterInput[105, 3] + waterInput[106, 3] +
                       waterInput[107, 3] + waterInput[309, 3]
                     ) -
    (inDitch$n286 + inDitch$n287 + inDitch$n288 + inDitch$n289 +
       inDitch$n112 + inWshed$w018)

  # ------------------ Watershed 19 ------------------------------------

  inStream$E110 = waterInput[110,3]

  inStream$E111 = waterInput[111,3]

  inStream$E114 = waterInput[114,3]

  inStream$E115 = waterInput[115,3]

  inDitch$n071 = inStream$E114*nodesInput[71, 2]
  inDitch$n139 = inStream$E115*nodesInput[139, 2]
  inDitch$n258 = inStream$E111*nodesInput[258, 2]
  inDitch$n257 = inStream$E110*nodesInput[257, 2]

  inStream$E112 = waterInput[112,3] + inStream$E114*nodesInput[71, 3]

  inDitch$n114 = inStream$E112*nodesInput[114, 2]

  inStream$E113 = waterInput[113,3] + inStream$E115*nodesInput[139, 3]

  inDitch$n074 = inStream$E113*nodesInput[74, 2]

  inStream$E109 = waterInput[109,3] +
    inStream$E113*nodesInput[74, 3] +
    inStream$E112*nodesInput[114, 3] +
    inStream$E111*nodesInput[258, 3] +
    inStream$E110*nodesInput[257, 3]

  inDitch$n132 = inStream$E109*nodesInput[132, 2]

  inStream$E108 = waterInput[108,3] + inStream$E109*nodesInput[132, 3]

  inWshed$w019 = inStream$E108

  WshedCheck$w019 = (waterInput[108, 3] + waterInput[109, 3] +
                       waterInput[110, 3] + waterInput[111, 3] +
                       waterInput[112, 3] + waterInput[113, 3] +
                       waterInput[114, 3] + waterInput[115, 3]
                     ) -
    (inDitch$n139 + inDitch$n074 + inDitch$n071 + inDitch$n114 +
       inDitch$n258 + inDitch$n257 + inDitch$n132 + inWshed$w019)
  # ------------------ Watershed 20 ------------------------------------

  inStream$E117 = waterInput[117,3]

  inDitch$n290 = inStream$E117*nodesInput[290, 2]

  inStream$E125 = waterInput[125, 3]

  inDitch$n061 = inStream$E125*nodesInput[61, 2]

  inStream$E123 = waterInput[123, 3]

  inDitch$n067 = inStream$E123*nodesInput[67, 2]

  inStream$E124 = waterInput[124, 3] + inStream$E125*nodesInput[61, 3]

  inDitch$n068 = inStream$E124*nodesInput[68, 2]

  inStream$E122 = waterInput[122, 3] + inStream$E123*nodesInput[67, 3]

  inDitch$n116 = inStream$E122*nodesInput[116, 2]

  inStream$E121 = waterInput[121, 3] + inStream$E124*nodesInput[68, 3]

  inDitch$n115 = inStream$E121*nodesInput[115, 2]

  inStream$E120 = waterInput[120, 3] +
    inStream$E122*nodesInput[116, 3] +
    inStream$E121*nodesInput[115, 3]

  inDitch$n135 = inStream$E120*nodesInput[135, 2]

  inStream$E119 = waterInput[119, 3] + inStream$E120*nodesInput[135, 3]
  inDitch$n130 = inStream$E119*nodesInput[130, 2]

  inStream$E118 = waterInput[118,3] + inStream$E119*nodesInput[130, 3]
  inDitch$n110 = inStream$E118*nodesInput[110, 2]

  inStream$E116 = waterInput[116,3] +
    inStream$E117*nodesInput[290, 3] +
    inStream$E118*nodesInput[110, 3]

  inWshed$w020 = inStream$E116

  WshedCheck$w020 = (waterInput[116,3] + waterInput[117,3] + waterInput[118,3] +
                       waterInput[119,3] + waterInput[120,3] + waterInput[121,3] +
                       waterInput[122,3] + waterInput[123,3] + waterInput[124,3] +
                       waterInput[125, 3]
                     )-
    (inDitch$n290 + inDitch$n110 + inDitch$n130 + inDitch$n135 +
       inDitch$n068 + inDitch$n115 + inDitch$n061 + inDitch$n116 + inDitch$n067 +
       inWshed$w020)
  # ------------------ Watershed 21 ------------------------------------

  inStream$E127 = waterInput[127, 3]

  inDitch$n128 = inStream$E127*nodesInput[128, 2]

  inStream$E126 = waterInput[126, 3] + inStream$E127*nodesInput[128, 3]

  inWshed$w021 = inStream$E126

  WshedCheck$w021 = (waterInput[127, 3] + waterInput[126, 3]) -
    (inDitch$n128 + inWshed$w021)

  # ------------------ Watershed 22 ------------------------------------

  inStream$E132 = waterInput[132, 3]

  inDitch$n060 = inStream$E132*nodesInput[60, 2]

  inStream$E131 = waterInput[131, 3] + inStream$E132*nodesInput[60, 3]

  inDitch$n282 = inStream$E131*nodesInput[282, 2]

  inStream$E130 = waterInput[130, 3] + inStream$E131*nodesInput[282, 3]

  inDitch$n136 = inStream$E130*nodesInput[136, 2]

  inStream$E129 = waterInput[129, 3] + inStream$E130*nodesInput[136, 3]

  inDitch$n101 = inStream$E129*nodesInput[101, 2]

  inStream$E128 = waterInput[128, 3]+ inStream$E129*nodesInput[101, 3]

  inWshed$w022 = inStream$E128

  WshedCheck$w022 = (waterInput[128, 3] + waterInput[129, 3] +
                       waterInput[130, 3] + waterInput[131, 3] + waterInput[132, 3]
                     ) -
    (inDitch$n101 + inDitch$n136 + inDitch$n282 + inDitch$n060 + inWshed$w022)

  # ------------------ Watershed 23 ------------------------------------

  inStream$E146 = waterInput[146, 3]

  inStream$E147 = waterInput[147, 3]

  inStream$E148 = waterInput[148, 3]

  inStream$E149 = waterInput[149, 3]

  inDitch$n056 = inStream$E146*nodesInput[56, 2]
  inDitch$n062 = inStream$E147*nodesInput[62, 2]
  inDitch$n081 = inStream$E149*nodesInput[81, 2]
  inDitch$n083 = inStream$E148*nodesInput[83, 2]

  inStream$E144 = waterInput[144, 3]

  inDitch$n165 = inStream$E144*nodesInput[165, 2]

  inStream$E141 = waterInput[141, 3]

  inDitch$n167 = inStream$E141*nodesInput[167, 2]

  inStream$E134 = waterInput[134, 3]

  inDitch$n291 = inStream$E134*nodesInput[291, 2]

  inStream$E135 = waterInput[135, 3]

  inDitch$n102 = inStream$E135*nodesInput[102, 2]

  inStream$E145 = waterInput[145, 3] + inStream$E146*nodesInput[56, 3]

  inDitch$n164 = inStream$E145*nodesInput[165, 2]

  inStream$E143 = waterInput[143, 3] + inStream$E147*nodesInput[62, 3]

  inDitch$n166 = inStream$E143*nodesInput[166, 2]

  inStream$E142 = waterInput[142, 3] + inStream$E148*nodesInput[83, 3]

  inDitch$n179 = inStream$E142*nodesInput[179, 2]

  inStream$E140 = waterInput[140, 3] +
    inStream$E145*nodesInput[164, 3]+
    inStream$E144*nodesInput[165, 3]+
    inStream$E143*nodesInput[166, 3]+
    inStream$E149*nodesInput[81, 3]

  inDitch$n204 = inStream$E140*nodesInput[204, 2]

  inStream$E139 = waterInput[139, 3] +
    inStream$E141*nodesInput[167, 3] +
    inStream$E142*nodesInput[179, 3]

  inDitch$n203 = inStream$E139*nodesInput[203, 2]

  inStream$E138 = waterInput[138, 3]  + inStream$E139*nodesInput[203, 3]

  inDitch$n293 = inStream$E138*nodesInput[293, 2]

  inStream$E137 = waterInput[137, 3] + inStream$E140*nodesInput[204, 3]

  inDitch$n103 =  inStream$E137*nodesInput[103, 2]

  inStream$E136 = waterInput[136, 3] + inStream$E138*nodesInput[293, 3]

  inDitch$n106 = inStream$E136*nodesInput[106, 2]

  inStream$E133 = waterInput[133, 3] +
    inStream$E134*nodesInput[291, 3] +
    inStream$E135*nodesInput[102, 3] +
    inStream$E137*nodesInput[103, 3] +
    inStream$E136*nodesInput[106, 3]

  inWshed$w023 = inStream$E133

  WshedCheck$w023 = (waterInput[133, 3] + waterInput[134, 3] + waterInput[135,3] +
                       waterInput[136, 3] + waterInput[137, 3] + waterInput[138,3] +
                       waterInput[139, 3] + waterInput[140, 3] + waterInput[141,3] +
                       waterInput[142, 3] + waterInput[143, 3] + waterInput[144,3] +
                       waterInput[145, 3] + waterInput[146, 3] + waterInput[147,3] +
                       waterInput[148, 3] + waterInput[149, 3]
                     ) -
    (inDitch$n102 + inDitch$n291 + inDitch$n106 + inDitch$n293 + inDitch$n203 +
       inDitch$n204 + inDitch$n103 + inDitch$n164 + inDitch$n165 + inDitch$n166+
       inDitch$n062 + inDitch$n056 + inDitch$n081 + inDitch$n167 + inDitch$n179 +
       inDitch$n083 + inWshed$w023)


  # ------------------ Watershed 24 ------------------------------------


  inStream$E156 = waterInput[156, 3]

  inStream$E153 = waterInput[153, 3]

  inStream$E154 = waterInput[154, 3]

  inStream$E151 = waterInput[151, 3]

  inDitch$n294 = inStream$E154*nodesInput[294, 2]
  inDitch$n212 = inStream$E153*nodesInput[212, 2]
  inDitch$n292 = inStream$E151*nodesInput[292, 2]
  inDitch$n088 = inStream$E156*nodesInput[88, 2]

  inStream$E155 = waterInput[155, 3] + inStream$E156*nodesInput[88, 3]
  inDitch$n176 = inStream$E155*nodesInput[176, 2]

  inStream$E152 = waterInput[152, 3] +
    inStream$E153*nodesInput[212, 3] +
    inStream$E154*nodesInput[294, 3] +
    inStream$E155*nodesInput[176, 3]
  inDitch$n107 = inStream$E152*nodesInput[107,2]

  inStream$E150 = waterInput[150, 3] +
    inStream$E151*nodesInput[292, 3] +
    inStream$E152*nodesInput[107,3]
  inWshed$w024 = inStream$E150

  WshedCheck$w024 = (waterInput[150, 3] + waterInput[151,3] + waterInput[152, 3] +
                       waterInput[153, 3] + waterInput[154,3] + waterInput[155, 3] +
                       waterInput[156, 3]
                     ) -
    (inDitch$n292 + inDitch$n107+ inDitch$n294 + inDitch$n212 + inDitch$n176 +
       inDitch$n088 + inWshed$w024)
  # ------------------ Watershed 25 -----------------------------------

  inStream$E165 = waterInput[165, 3]

  inStream$E166 = waterInput[166, 3]

  inStream$E167 = waterInput[167, 3]

  inDitch$n078 = inStream$E167*nodesInput[78, 2]
  inDitch$n077 = inStream$E166*nodesInput[77, 2]
  inDitch$n097 = inStream$E165*nodesInput[97, 2]

  inStream$E163 = waterInput[163, 3] + inStream$E166*nodesInput[77, 3]
  inDitch$n172 = inStream$E163*nodesInput[172, 2]

  inStream$E164 = waterInput[164, 3] + inStream$E165*nodesInput[97, 3]
  inDitch$n173 = inStream$E164*nodesInput[173, 2]

  inStream$E161 = waterInput[161, 3] + inStream$E167*nodesInput[78, 3]
  inDitch$n171 = inStream$E161*nodesInput[171, 2]

  inStream$E160 = waterInput[160, 3] + inStream$E161*nodesInput[171, 3]
  inDitch$n159 = inStream$E160*nodesInput[159, 2]

  inStream$E162 = waterInput[162, 3] +
    inStream$E163*nodesInput[172, 3] +
    inStream$E164*nodesInput[173, 3]
  inDitch$n075 = inStream$E162*nodesInput[75, 2]

  inStream$E159 = waterInput[159, 3] +
    inStream$E160*nodesInput[159, 3] +
    inStream$E162*nodesInput[75, 3]
  inDitch$n094 = inStream$E159*nodesInput[94, 2]

  inStream$E158 = waterInput[158, 3]+ inStream$E159*nodesInput[94, 3]
  inDitch$n202 = inStream$E158*nodesInput[202, 2]

  inStream$E157 = waterInput[157, 3]+ inStream$E158*nodesInput[202, 3]

  inWshed$w025 = inStream$E157

  WshedCheck$w025 = (waterInput[157, 3] + waterInput[158, 3] + waterInput[159, 3] +
                       waterInput[160, 3] + waterInput[161, 3] + waterInput[162, 3] +
                       waterInput[163, 3] + waterInput[164, 3] + waterInput[165, 3] +
                       waterInput[166, 3] + waterInput[167, 3]
                     ) -
    (inDitch$n202 + inDitch$n094 + inDitch$n159 + inDitch$n075 + inDitch$n172 +
       inDitch$n173 + inDitch$n097 + inDitch$n077 + inDitch$n171 + inDitch$n078 +
       inWshed$w025)

  # ------------------ Watershed 26 -----------------------------------

  inStream$E174 = waterInput[174, 3]

  inStream$E175 = waterInput[175, 3]

  inDitch$n089 = inStream$E174*nodesInput[89, 2]
  inDitch$n092 = inStream$E175*nodesInput[92, 2]

  # added new node 310 for basin 173
  inStream$E173 = waterInput[173, 3] + inStream$E175*nodesInput[92, 3]
  inDitch$n310 = inStream$E173*nodesInput[310, 2]

  inStream$E172 = waterInput[172, 3] + inStream$E174*nodesInput[89, 3]
  inDitch$n177 = inStream$E172*nodesInput[177, 2]

  inStream$E171 = waterInput[171, 3] +
    inStream$E172*nodesInput[177, 3] +
    inStream$E173*nodesInput[310, 3]
  inDitch$n095 = inStream$E171*nodesInput[95, 2]

  inStream$E170 = waterInput[170, 3] + inStream$E171*nodesInput[95, 3]
  inDitch$n069 = inStream$E170*nodesInput[69, 2]

  inStream$E169 = waterInput[169, 3] + inStream$E170*nodesInput[69, 3]
  inDitch$n154 = inStream$E169*nodesInput[154, 2]

  inStream$E168 = waterInput[168, 3] + inStream$E169*nodesInput[154, 3]
  inWshed$w026 = inStream$E168

  WshedCheck$w026 = (waterInput[168, 3] + waterInput[169, 3] + waterInput[170, 3] +
                       waterInput[171, 3] + waterInput[172, 3] + waterInput[173, 3] +
                       waterInput[174, 3] + waterInput[175, 3]
  ) -
    (inDitch$n154 + inDitch$n069 + inDitch$n095 + inDitch$n177 + inDitch$n089 +
       inDitch$n092 + inWshed$w026)

  # -------------------- Watershed 27 -----------------------------------

  inStream$E178 = waterInput[178, 3]

  inDitch$n178 = inStream$E178*nodesInput[178, 2]

  inStream$E177 = waterInput[177, 3] + inStream$E178*nodesInput[178, 3]

  inDitch$n086 = inStream$E177*nodesInput[86, 2]

  inStream$E176 = waterInput[176, 3] + inStream$E177*nodesInput[86, 3]

  inWshed$w027 = inStream$E176

  WshedCheck$w027 = (waterInput[178, 3]+waterInput[177,3] + waterInput[176, 3]) -
    (inDitch$n086+inDitch$n178 + inWshed$w027)

  # -------------------- Watershed 28 -----------------------------------
  inStream$E188 = waterInput[188, 3]

  inStream$E186 = waterInput[186, 3]

  inStream$E183 = waterInput[183, 3]

  inStream$E184 = waterInput[184, 3]

  inDitch$n093 = inStream$E188*nodesInput[93, 2]
  inDitch$n261 = inStream$E184*nodesInput[261, 2]
  inDitch$n259 = inStream$E186*nodesInput[259, 2]
  inDitch$n263 = inStream$E183*nodesInput[263, 2]

  inStream$E182 = waterInput[182, 3] + inStream$E183*nodesInput[263, 3]
  inDitch$n262 = inStream$E182*nodesInput[262, 2]

  inStream$E185 = waterInput[185, 3] + inStream$E188*nodesInput[93, 3]
  inDitch$n149 = inStream$E185*nodesInput[149, 2]

  inStream$E187 = waterInput[187, 3] + inStream$E186*nodesInput[259, 3]
  inDitch$n260 = inStream$E187*nodesInput[260, 2]

  inStream$E181 = waterInput[181, 3] +
    inStream$E184*nodesInput[261, 3] +
    inStream$E185*nodesInput[149, 3] +
    inStream$E187*nodesInput[260, 3] +
    inStream$E182*nodesInput[262, 3]
  inDitch$n055 = inStream$E181*nodesInput[55, 2]

  inStream$E180 = waterInput[180, 3] + inStream$E181*nodesInput[55, 3]
  inDitch$n134 = inStream$E180*nodesInput[134, 2]

  inStream$E179 = waterInput[179, 3] + inStream$E180*nodesInput[134, 3]

  inWshed$w028 = inStream$E179

  WshedCheck$w028 = (waterInput[179, 3] + waterInput[180, 3]+waterInput[181, 3] +
                       waterInput[182, 3] + waterInput[183, 3]+waterInput[184, 3] +
                       waterInput[185, 3] + waterInput[186, 3]+waterInput[187, 3] +
                       waterInput[188, 3]) -
    (inDitch$n134 + inDitch$n055 + inDitch$n261 + inDitch$n260 + inDitch$n259 +
       inDitch$n149 + inDitch$n263 + inDitch$n262 + inDitch$n093 + inWshed$w028)

  # -------------------- Watershed 29 -----------------------------------
  inStream$E191 = waterInput[191, 3]
  inStream$E193 = waterInput[193, 3]

  inDitch$n299 = inStream$E191*nodesInput[299, 2]
  inDitch$n265 = inStream$E193*nodesInput[265, 2]

  inStream$E192 = waterInput[192, 3] + inStream$E193*nodesInput[265, 3]

  inDitch$n298 = inStream$E192*nodesInput[298, 2]

  inStream$E202 = waterInput[202, 3]

  inDitch$n098 = inStream$E202*nodesInput[98, 2]

  inStream$E201 = waterInput[201, 3]+ inStream$E202*nodesInput[98, 3]
  inDitch$n168 = inStream$E201*nodesInput[168, 2]

  inStream$E200 = waterInput[200, 3] + inStream$E201*nodesInput[168, 3]
  inDitch$n144 = inStream$E200*nodesInput[144, 2]

  inStream$E195 = waterInput[195, 3]

  inStream$E196 = waterInput[196, 3]

  inStream$E197 = waterInput[197, 3]

  inStream$E199 = waterInput[199, 3]

  inDitch$n264 = inStream$E199*nodesInput[264, 2]
  inDitch$n297 = inStream$E195*nodesInput[297, 2]
  inDitch$n296 = inStream$E196*nodesInput[296, 2]
  inDitch$n146 = inStream$E197*nodesInput[146, 2]

  inStream$E198 = waterInput[198, 3] + inStream$E199*nodesInput[264, 3]
  inDitch$n066 = inStream$E198*nodesInput[66, 2]

  inStream$E194 = waterInput[194, 3] +
    inStream$E195*nodesInput[297, 3] +
    inStream$E196*nodesInput[296, 3] +
    inStream$E197*nodesInput[146, 3] +
    inStream$E198*nodesInput[66, 3] +
    inStream$E200*nodesInput[144, 3]

  inDitch$n123 = inStream$E194*nodesInput[123, 2]

  inStream$E190 = waterInput[190, 3] +
    inStream$E191*nodesInput[299, 3] +
    inStream$E192*nodesInput[298, 3]
  inDitch$n131 = inStream$E190*nodesInput[131, 2]

  inStream$E189 = waterInput[189, 3] +
    inStream$E194*nodesInput[123, 3] +
    inStream$E190*nodesInput[131, 3]

  inWshed$w029 = inStream$E189

  WshedCheck$w029 = (waterInput[189, 3] + waterInput[190, 3] + waterInput[191, 3] +
                       waterInput[192, 3] + waterInput[193, 3] + waterInput[194, 3] +
                       waterInput[195, 3] + waterInput[196, 3] + waterInput[197, 3] +
                       waterInput[198, 3] + waterInput[199, 3] + waterInput[200, 3] +
                       waterInput[202, 3] + waterInput[201, 3]
                     ) -
    (inDitch$n123 + inDitch$n066 + inDitch$n131 + inDitch$n299 + inDitch$n298 +
       inDitch$n146 + inDitch$n296 + inDitch$n297 + inDitch$n144 + inDitch$n265 +
       inDitch$n264 + inDitch$n168 + inDitch$n098 + inWshed$w029)

  # -------------------- Watershed 30 -----------------------------------

  inStream$E203 = waterInput[203, 3]
  inWshed$w030 = inStream$E203

  # -------------------- Watershed 31 -----------------------------------

  inStream$E210 = waterInput[210, 3]

  inStream$E207 = waterInput[207, 3]

  inDitch$n266 = inStream$E210*nodesInput[266, 2]
  inDitch$n143 = inStream$E207*nodesInput[207, 2]

  inStream$E209 = waterInput[209, 3]+  inStream$E210*nodesInput[266, 3]

  inDitch$n108 = inStream$E209*nodesInput[108, 2]

  inStream$E208 = waterInput[208, 3] + inStream$E209*nodesInput[108, 3]

  inDitch$n142 = inStream$E208*nodesInput[142, 2]

  inStream$E205 = waterInput[205, 3] +
    inStream$E208*nodesInput[142, 3] +
    inStream$E207*nodesInput[143, 3]

  inDitch$n122 = inStream$E205*nodesInput[122, 2]

  inStream$E204 = waterInput[204, 3] + inStream$E205*nodesInput[122, 3]

  inWshed$w031 = inStream$E204

  WshedCheck$w031 = (waterInput[204, 3] + waterInput[205, 3] + waterInput[207, 3] +
                       waterInput[208, 3] + waterInput[209, 3] + waterInput[210, 3]
                     ) -
    (inDitch$n122 + inDitch$n142 + inDitch$n143 + inDitch$n108 + inDitch$n266 +
       inWshed$w031)

  # -------------------- Watershed 32 -----------------------------------

  # added new node 309 for basin 211
  inStream$E213 = waterInput[213, 3]

  inStream$E214 = waterInput[214, 3]

  inDitch$n174 = inStream$E214*nodesInput[174, 2]
  inDitch$n175 = inStream$E213*nodesInput[175, 2]

  inStream$E212 = waterInput[212, 3] +
    inStream$E214*nodesInput[174, 3] +
    inStream$E213*nodesInput[175, 3]
  inDitch$n120 = inStream$E212*nodesInput[120, 2]

  inStream$E211 = waterInput[211, 3] + inStream$E212*nodesInput[120,3]
  inWshed$w032 = inStream$E211

  WshedCheck$w032 = (waterInput[211, 3] + waterInput[212,3] + waterInput[213, 3] +
                       waterInput[214, 3]
                     ) -
    (inDitch$n120 + inDitch$n175 + inDitch$n174 + inWshed$w032)
  # -------------------- Watershed 33 -----------------------------------

  inStream$E215 = waterInput[215, 3]
  inWshed$w033 = inStream$E215

  # -------------------- Watershed 34 -----------------------------------

  inStream$E227 = waterInput[227, 3]

  inStream$E228 = waterInput[228, 3]

  inStream$E229 = waterInput[229, 3]

  inStream$E218 = waterInput[218, 3]

  inStream$E219 = waterInput[219, 3]

  inStream$E223 = waterInput[223, 3]

  inDitch$n057 = inStream$E227*nodesInput[57, 2]
  inDitch$n058 = inStream$E229*nodesInput[58, 2]
  inDitch$n268 = inStream$E223*nodesInput[268, 2]
  inDitch$n059 = inStream$E228*nodesInput[59, 2]
  inDitch$n141 = inStream$E219*nodesInput[141, 2]
  inDitch$n300 = inStream$E218*nodesInput[300, 2]

  inStream$E226 = waterInput[226, 3] +
    inStream$E223*nodesInput[268, 3] +
    inStream$E229*nodesInput[58, 3]

  inDitch$n138 = inStream$E226*nodesInput[138, 2]

  inStream$E225 = waterInput[225, 3] + inStream$E227*nodesInput[57, 3]
  inDitch$n148 = inStream$E225*nodesInput[148, 2]

  inStream$E2251 = inStream$E225*nodesInput[148, 3]
  inDitch$n267 = inStream$E2251*nodesInput[267, 2]

  inStream$E2252 = inStream$E2251*nodesInput[267, 3]

  # assigned two diversions to basin 225
  inStream$E224 = waterInput[224, 3]+
    inStream$E2252 +
    inStream$E226*nodesInput[138, 3]

  inDitch$n158 = inStream$E224*nodesInput[158, 2]

  inStream$E221 = waterInput[221, 3] + inStream$E224*nodesInput[158, 3]
  inDitch$n140 = inStream$E221*nodesInput[140, 2]

  inStream$E222 = waterInput[222, 3] + inStream$E228*nodesInput[59, 3]
  inDitch$n147 = inStream$E222*nodesInput[147, 2]

  inStream$E220 = waterInput[220, 3] + inStream$E221*nodesInput[140, 3]
  inDitch$n141 = (inStream$E219*nodesInput[141,2])+(inStream$E220*nodesInput[141,2])

  inStream$E217 = waterInput[217, 3] +
    inStream$E218*nodesInput[300, 3] +
    inStream$E219*nodesInput[141, 3] +
    inStream$E220*nodesInput[141, 3] +
    inStream$E222*nodesInput[147, 3]

  inDitch$n121 = inStream$E217*nodesInput[121,2]

  inStream$E216 = waterInput[216, 3] + inStream$E217*nodesInput[121, 3]

  inWshed$w034 = inStream$E216

  WshedCheck$w034 = (waterInput[216, 3] + waterInput[217, 3] + waterInput[218,3] +
                       waterInput[219, 3] + waterInput[220, 3] + waterInput[221,3] +
                       waterInput[222, 3] + waterInput[223, 3] + waterInput[224,3] +
                       waterInput[225, 3] + waterInput[226, 3] + waterInput[227,3] +
                       waterInput[228, 3] + waterInput[229, 3]
                     ) -
    (inDitch$n121 + inDitch$n300 + inDitch$n141 + inDitch$n140 + inDitch$n147 +
       inDitch$n059 + inDitch$n268 + inDitch$n138 + inDitch$n058 + inDitch$n267 +
       inDitch$n148 + inDitch$n057 + inDitch$n158 + inWshed$w034)

  # -------------------- Watershed 35 -----------------------------------

  inStream$E237 = waterInput[237, 3]

  inStream$E238 = waterInput[238, 3]

  inStream$E232 = waterInput[232, 3]

  inStream$E233 = waterInput[233, 3]

  inStream$E235 = waterInput[235, 3]

  inDitch$n170 = inStream$E232*nodesInput[170, 2]
  inDitch$n161 = inStream$E233*nodesInput[161, 2]
  inDitch$n150 = inStream$E235*nodesInput[150, 2]
  inDitch$n269 = inStream$E237*nodesInput[269, 2]
  inDitch$n063 = inStream$E238*nodesInput[63, 2]

  inStream$E236 = waterInput[236, 3] + inStream$E238*nodesInput[63, 3]

  inDitch$n151 = inStream$E236*nodesInput[151, 2]

  inStream$E234 = waterInput[234, 3] +
    inStream$E235*nodesInput[150, 3] +
    inStream$E236*nodesInput[151, 3] +
    inStream$E237*nodesInput[269, 3]

  inDitch$n117 = inStream$E234*nodesInput[117,2]

  inStream$E231 = waterInput[231, 3] +
    inStream$E234*nodesInput[117, 3] +
    inStream$E232*nodesInput[170, 3] +
    inStream$E233*nodesInput[161, 3]
  inDitch$n096 = inStream$E231*nodesInput[96, 2]

  inStream$E230 = waterInput[230, 3] + inStream$E231*nodesInput[96, 3]

  inWshed$w035 = inStream$E230

  WshedCheck$w035 = (waterInput[230,3] + waterInput[231,3] + waterInput[232, 3] +
                       waterInput[233,3] + waterInput[234,3] + waterInput[235, 3] +
                       waterInput[236,3] + waterInput[237,3] + waterInput[238, 3]
                     ) -
    (inDitch$n096 + inDitch$n170 + inDitch$n161 + inDitch$n117 + inDitch$n150 +
       inDitch$n151 + inDitch$n269 + inDitch$n063 + inWshed$w035)

  # -------------------- Watershed 36 -----------------------------------
  inStream$E269 = waterInput[269, 3]

  inStream$E270 = waterInput[270, 3]

  inStream$E264 = waterInput[264, 3]

  inStream$E265 = waterInput[265, 3]

  inStream$E266 = waterInput[266, 3]

  inDitch$n070 = inStream$E269*nodesInput[70, 2]
  inDitch$n275 = inStream$E270*nodesInput[275, 2]
  inDitch$n273 = inStream$E264*nodesInput[273, 2]
  inDitch$n272 = inStream$E265*nodesInput[272, 2]
  inDitch$n274 = inStream$E266*nodesInput[274, 2]

  inStream$E267 = waterInput[267, 3]

  inStream$E249 = waterInput[249, 3]

  inStream$E250 = waterInput[250, 3]

  inStream$E251 = waterInput[251, 3]

  inStream$E243 = waterInput[243, 3]

  inDitch$n064 = inStream$E267*nodesInput[64, 2]
  inDitch$n295 = inStream$E249*nodesInput[295, 2]
  inDitch$n301 = inStream$E250*nodesInput[301, 2]
  inDitch$n118 = inStream$E251*nodesInput[118, 2]
  inDitch$n124 = inStream$E243*nodesInput[124, 2]

  inStream$E244 = waterInput[244, 3]

  inStream$E241 = waterInput[241, 3]

  inStream$E254 = waterInput[254, 3]

  inStream$E255 = waterInput[255, 3]

  inStream$E261 = waterInput[261, 3]

  inStream$E259 = waterInput[259, 3]

  inDitch$n302 = inStream$E244*nodesInput[302, 2]
  inDitch$n303 = inStream$E241*nodesInput[303, 2]
  inDitch$n270 = inStream$E254*nodesInput[270, 2]
  inDitch$n155 = inStream$E255*nodesInput[155, 2]
  inDitch$n271 = inStream$E261*nodesInput[271, 2]
  inDitch$n153 = inStream$E259*nodesInput[153, 2]

  inStream$E258 = waterInput[258, 3] + inStream$E259*nodesInput[153, 3]
  inDitch$n209 = inStream$E258*nodesInput[209, 2]

  inStream$E257 = waterInput[257, 3] + inStream$E267*nodesInput[64, 3]
  inDitch$n160 = inStream$E257*nodesInput[160, 2]

  inStream$E263 = waterInput[263, 3] + inStream$E270*nodesInput[275, 3]
  inDitch$n210 = inStream$E263*nodesInput[210, 2]

  inStream$E256 = waterInput[256, 3] + inStream$E257*nodesInput[160, 3]
  inDitch$n156 = inStream$E256*nodesInput[156, 2]

  inStream$E262 = waterInput[262, 3] + inStream$E265*nodesInput[272, 3]
  inDitch$n276 = inStream$E262*nodesInput[276, 2]

  inStream$E268 = waterInput[268, 3] + inStream$E269*nodesInput[70, 3]
  inDitch$n152 = inStream$E268*nodesInput[152, 2]

  inStream$E253 = waterInput[253, 3] +
    inStream$E256*nodesInput[156, 3] +
    inStream$E254*nodesInput[270, 3]
  inDitch$n125 = inStream$E253*nodesInput[125, 2]

  inStream$E246 = waterInput[246, 3] +
    inStream$E258*nodesInput[209, 3] +
    inStream$E255*nodesInput[155, 3]
  inDitch$n105 = inStream$E246*nodesInput[105, 2]

  inStream$E252 = waterInput[252, 3] + inStream$E253*nodesInput[125, 3]
  inDitch$n091 = inStream$E252*nodesInput[91, 2]

  inStream$E260 = waterInput[260, 3] +
    inStream$E266*nodesInput[274, 3] +
    inStream$E264*nodesInput[273, 3] +
    inStream$E268*nodesInput[152, 3]
  inDitch$n211 = inStream$E260*nodesInput[211, 2]

  inStream$E245 = waterInput[245, 3] +
    inStream$E261*nodesInput[271, 3] +
    inStream$E262*nodesInput[276, 3] +
    inStream$E263*nodesInput[210, 3]
  inDitch$n126 = inStream$E245*nodesInput[126,2]

  inStream$E248 = waterInput[248, 3] +
    inStream$E250*nodesInput[301, 3] +
    inStream$E252*nodesInput[91, 3]
  inDitch$n090 = inStream$E248*nodesInput[90, 2]

  inStream$E247 = waterInput[247, 3] +
    inStream$E248*nodesInput[90, 3] +
    inStream$E249*nodesInput[295, 3] +
    inStream$E251*nodesInput[118, 3]

  inStream$E242 = waterInput[242, 3] +
    inStream$E241*nodesInput[303, 3] +
    inStream$E245*nodesInput[126, 3] +
    inStream$E246*nodesInput[105, 3] +
    inStream$E260*nodesInput[211,3]
  inDitch$n087 = inStream$E242*nodesInput[87, 2]

  inStream$E240 = waterInput[240, 3] +
    inStream$E242*nodesInput[87, 3] +
    inStream$E243*nodesInput[124, 3] +
    inStream$E244*nodesInput[302, 3]
  inDitch$n046 = (inStream$E247*nodesInput[46, 2])+(inStream$E240*nodesInput[46,2])

  inStream$E239 = waterInput[239, 3] +
    inStream$E240*nodesInput[46, 3] +
    inStream$E247*nodesInput[46, 3]

  inWshed$w036 = inStream$E239

  WshedCheck$w036 = (waterInput[239, 3] + waterInput[240, 3] + waterInput[241, 3] +
                       waterInput[242, 3] + waterInput[243, 3] + waterInput[244, 3] +
                       waterInput[245, 3] + waterInput[246, 3] + waterInput[247, 3] +
                       waterInput[248, 3] + waterInput[249, 3] + waterInput[250, 3] +
                       waterInput[251, 3] + waterInput[252, 3] + waterInput[253, 3] +
                       waterInput[254, 3] + waterInput[255, 3] + waterInput[256, 3] +
                       waterInput[257, 3] + waterInput[258, 3] + waterInput[259, 3] +
                       waterInput[260, 3] + waterInput[261, 3] + waterInput[262, 3] +
                       waterInput[263, 3] + waterInput[264, 3] + waterInput[265, 3] +
                       waterInput[266, 3] + waterInput[267, 3] + waterInput[268, 3] +
                       waterInput[269, 3] + waterInput[270, 3]
                     ) -
    (inDitch$n046 + inDitch$n087+ inDitch$n090 + inDitch$n126 + inDitch$n211 +
       inDitch$n091 + inDitch$n105 + inDitch$n125 + inDitch$n152 + inDitch$n276 +
       inDitch$n156 + inDitch$n210 + inDitch$n160 + inDitch$n209 + inDitch$n153 +
       inDitch$n271 + inDitch$n155 + inDitch$n270 + inDitch$n303 + inDitch$n302 +
       inDitch$n124 + inDitch$n118 + inDitch$n301 + inDitch$n295 + inDitch$n064 +
       inDitch$n272 + inDitch$n273 + inDitch$n274 + inDitch$n275 + inDitch$n070 +
       inWshed$w036)

  # -------------------- Watershed 37 -----------------------------------

  # inStream$E271 = 0
  # -------------------- Watershed 38 -----------------------------------

  inStream$E273 = waterInput[273, 3]

  inDitch$n082 = inStream$E273*nodesInput[82, 2]

  inStream$E272 = waterInput[272, 3] + inStream$E273*nodesInput[82, 3]

  inWshed$w038 = inStream$E272

  WshedCheck$w038 = (waterInput[273, 3] + waterInput[272,3]) -
    (inDitch$n082 + inWshed$w038)

  # -------------------- Watershed 39 -----------------------------------

  inStream$E276 = waterInput[276, 3]

  inStream$E277 = waterInput[277, 3]

  inDitch$n127 = inStream$E276*nodesInput[127, 2]
  inDitch$n080 = inStream$E277*nodesInput[80, 2]

  inStream$E275 = waterInput[275, 3] +
    inStream$E277*nodesInput[80, 3] +
    inStream$E276*nodesInput[127, 3]

  inDitch$n079 = inStream$E275*nodesInput[79, 2]

  inStream$E274 = waterInput[274, 3] +inStream$E275*nodesInput[79, 3]

  inWshed$w039 = inStream$E274

  WshedCheck$w039 = (waterInput[274, 3] + waterInput[275, 3] + waterInput[276, 3] +
                       waterInput[277, 3]
                     ) -
    (inDitch$n079 + inDitch$n080 + inDitch$n127 + inWshed$w039)

  # -------------------- Watershed 40 -----------------------------------

  # converted nodeID = 0 to nodeID = 311

  inStream$E279 = waterInput[279, 3]

  inStream$E280 = waterInput[280, 3]

  inDitch$n133 = inStream$E279*nodesInput[133, 2]
  inDitch$n104 = inStream$E280*nodesInput[104, 2]

  inStream$E278 = waterInput[278, 3] +
    inStream$E280*nodesInput[104, 3] +
    inStream$E279*nodesInput[133, 3]

  inWshed$w040 = inStream$E278

  WshedCheck$w040 = (waterInput[279, 3] + waterInput[278, 3] + waterInput[280, 3]
                    ) -
    (inDitch$n133 + inDitch$n104 + inWshed$w040)

  # -------------------- Watershed 41 -----------------------------------

  # inStream$E281 = 0
  inWshed$w041 = waterInput[281, 0]

  # -------------------- Watershed 42 -----------------------------------

  inStream$E283 = waterInput[283, 3]

  inStream$E284 = waterInput[284, 3]

  inStream$E285 = waterInput[285, 3]

  inStream$E286 = waterInput[286, 3]

  inDitch$n304 = inStream$E283*nodesInput[304, 2]
  inDitch$n305 = inStream$E284*nodesInput[305, 2]
  inDitch$n306 = inStream$E285*nodesInput[306, 2]
  inDitch$n169 = inStream$E286*nodesInput[169, 2]

  inStream$E282 = waterInput[282, 3] +
    inStream$E283*nodesInput[304, 3] +
    inStream$E284*nodesInput[305, 3] +
    inStream$E285*nodesInput[306, 3] +
    inStream$E286*nodesInput[169, 3]

  inWshed$w042 = inStream$E282

  WshedCheck$w042 = (waterInput[282, 3] + waterInput[283, 3] + waterInput[284, 3] +
                       waterInput[285, 3] + waterInput[286, 3]
                     ) -
    (inDitch$n304 + inDitch$n305 + inDitch$n306 + inDitch$n169 + inWshed$w042)

  # -------------------- Watershed 43 -----------------------------------

  inStream$E292 = waterInput[292, 3]

  inStream$E293 = waterInput[293, 3]

  inDitch$n219 = inStream$E293*nodesInput[219, 2]
  inDitch$n189 = inStream$E292*nodesInput[189, 2]

  inStream$E291 = waterInput[291, 3] + inStream$E293*nodesInput[219, 3]

  inStream$E290 = waterInput[290, 3]+ inStream$E292*nodesInput[189, 3]

  inDitch$n163 = (inStream$E290*nodesInput[163, 2])+(inStream$E291*nodesInput[163, 2])

  inStream$E289 = waterInput[289, 3] +
    inStream$E290*nodesInput[163, 3] +
    inStream$E291*nodesInput[163, 3]

  inDitch$n162 = inStream$E289*nodesInput[162, 2]

  inStream$E288 = waterInput[288, 3] + inStream$E289*nodesInput[162, 3]

  inDitch$n157 = inStream$E288*nodesInput[157, 2]

  inStream$E287 = waterInput[287, 3] + inStream$E288*nodesInput[157,3]

  inWshed$w043 = inStream$E287

  WshedCheck$w043 = (waterInput[287, 3] + waterInput[288,3] + waterInput[289, 3] +
                       waterInput[290, 3] + waterInput[291,3] + waterInput[292, 3] +
                       waterInput[293, 3]
                     ) -
    (inDitch$n157 + inDitch$n162 + inDitch$n163 + inDitch$n189 + inDitch$n219 +
       inWshed$w043)

  # -------------------- Watershed 44 -----------------------------------


  inStream$E295 = waterInput[295, 3]

  inDitch$n308 = inStream$E295*nodesInput[308, 2]

  inStream$E294 = waterInput[294, 3] + inStream$E295*nodesInput[308, 3]

  inWshed$w044 = inStream$E294

  WshedCheck$w044 = (waterInput[294, 3] + waterInput[295, 3]) -
    (inDitch$n308 + inWshed$w044)

  # -------------------- Watershed 45 -----------------------------------

  inStream$E298 = waterInput[298, 3]

  inDitch$n213 = inStream$E298*nodesInput[213, 2]

  inStream$E297 = waterInput[297, 3] + inStream$E298*nodesInput[213, 3]

  inDitch$n084 = inStream$E297*nodesInput[84, 2]

  inStream$E296 = waterInput[296, 3] + inStream$E297*nodesInput[84, 3]

  inWshed$w045 = inStream$E296

  WshedCheck$w045 = (waterInput[298, 3] + waterInput[297, 3] + waterInput[296, 3]) -
    (inDitch$n213 + inDitch$n084 + inWshed$w045)

  # -------------------- Watershed 46 -----------------------------------

  inStream$E300 = waterInput[300, 3]

  inStream$E307 = waterInput[307, 3]

  inStream$E308 = waterInput[308, 3]

  inStream$E304 = waterInput[304, 3]

  inStream$E305 = waterInput[305, 3]

  inDitch$n307 = inStream$E300*nodesInput[307, 2]
  inDitch$n277 = inStream$E307*nodesInput[277, 2]
  inDitch$n065 = inStream$E308*nodesInput[65, 2]
  inDitch$n145 = inStream$E304*nodesInput[145, 2]
  inDitch$n076 = inStream$E305*nodesInput[76, 2]

  inStream$E306 = waterInput[306, 3] +
    inStream$E307*nodesInput[277, 3] +
    inStream$E308*nodesInput[65, 3]

  inDitch$n137 = inStream$E306*nodesInput[137, 2]

  inStream$E303 = waterInput[303, 3] + inStream$E304*nodesInput[145, 3]

  inDitch$n129 = inStream$E303*nodesInput[129, 2]

  inStream$E302 = waterInput[302, 3] + inStream$E303*nodesInput[129, 3]

  inDitch$n099 = inStream$E302*nodesInput[99, 2]

  inStream$E301 = waterInput[301, 3] + inStream$E302*nodesInput[99, 3]

  inDitch$n085 = inStream$E301*nodesInput[85, 2]

  inStream$E299 = waterInput[299, 3] +
    inStream$E300*nodesInput[307,3] +
    inStream$E301*nodesInput[85, 3] +
    inStream$E305*nodesInput[76,3] +
    inStream$E306*nodesInput[137, 3]

  inWshed$w046 = inStream$E299

  WshedCheck$w046 = (waterInput[299, 3] + waterInput[300, 3] + waterInput[301, 3] +
                       waterInput[302, 3] + waterInput[303, 3] + waterInput[304, 3] +
                       waterInput[305, 3] + waterInput[306, 3] + waterInput[307, 3] +
                       waterInput[308, 3]
                     ) -
    (inDitch$n085 + inDitch$n076 + inDitch$n307 + inDitch$n137 + inDitch$n099 +
       inDitch$n129 + inDitch$n145 + inDitch$n277 + inDitch$n065 + inWshed$w046)

  # -------------------------------------------------------------------
  # Save output in list

  output = list("inStream" = inStream, "spsk" = spsk,
                "inDitch" = inDitch, "WshedCheck" = WshedCheck,
                "inWshed" = inWshed)

  return(output)
}

