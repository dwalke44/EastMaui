#' @export
# Function to calculate amount of water remaining in basins after diversion

# convert all nodeInputs[..., c(8:11)] to nodeInputs[..., 2]
basinWater.fun = function(nodesInput, waterInput){
  diverted = list()
  spsk = list()

  diverted$E003 = waterInput[3, 3]

  diverted$E004 = waterInput[4, 3]

  diverted$E002 = waterInput[2, 3] + diverted$E003*nodesInput[228, 3] + diverted$E004*nodesInput[196, 3]
  # node 49 = sink
  spsk$E001s = (diverted$E002*nodesInput[49, 3] + nodesInput[49, 13])

  spsk$E001 = ifelse(spsk$E001s<0, 0, spsk$E001s)

  diverted$E001 = waterInput[1,3] + spsk$E001
# --------------------------------------------------------------------

  diverted$E008 = waterInput[8, 3]

  diverted$E009 = waterInput[9, 3]

  diverted$E010 = waterInput[10, 3]

  diverted$E007 = waterInput[7, 3] + diverted$E009*nodesInput[192, 3] + diverted$E008*nodesInput[193, 3] + diverted$E010*nodesInput[194, 3]

  diverted$E006 = waterInput[6, 3] + diverted$E007*nodesInput[229, 3]
  # node 50 = spring
  spsk$E005s =  (nodesInput[50,13] + diverted$E006*nodesInput[50,3])

  spsk$E005 = ifelse(spsk$E005s<0, 0, spsk$E005s)

  diverted$E005 = waterInput[5, 3] + spsk$E005

  # --------------------------------------------------------------------
  diverted$E014 = waterInput[14, 3]

  diverted$E015 = waterInput[15, 3]

  diverted$E016 = waterInput[16, 3] + diverted$E015*nodesInput[187, 3]

  diverted$E013 = waterInput[13, 3] + diverted$E016*nodesInput[188, 3]

  diverted$E012 = waterInput[12, 3] +
    diverted$E013*nodesInput[195, 3] +
    diverted$E014*nodesInput[186, 3]

  # node 51 = spring

  spsk$E011s =  (nodesInput[51,13] + diverted$E012*nodesInput[51,3])

  spsk$E011 = ifelse(spsk$E011s<0, 0, spsk$E011s)

  diverted$E011 = waterInput[11, 3] + spsk$E011

  # --------------------------------------------------------------------

  diverted$E018 = waterInput[18, 3]

  diverted$E017 = waterInput[17, 3] +  diverted$E018*nodesInput[230, 3]

  # ---------------------------------------------------------------------

  diverted$E020 = waterInput[20, 3]

  diverted$E021 = waterInput[21, 3]

  diverted$E022 = waterInput[22, 3]

  diverted$E023 = waterInput[23, 3]

  diverted$E019 = waterInput[19, 3] +
    diverted$E023*nodesInput[184, 3]+
    diverted$E022*nodesInput[185, 3]+
    diverted$E021*nodesInput[231, 3]+
    diverted$E020*nodesInput[232, 3]

  # -----------------------------------------------------------------------
  diverted$E025 = waterInput[25, 3]

  diverted$E026 = waterInput[26, 3]

  diverted$E027 = waterInput[27, 3]

  diverted$E028 = waterInput[28, 3]

  diverted$E024 = waterInput[24, 3] + diverted$E025*nodesInput[233, 3] + diverted$E026*nodesInput[234, 3] + diverted$E027*nodesInput[182, 3]+ diverted$E028*nodesInput[183, 3]

  # -----------------------------------------------------------------------

  diverted$E030 = waterInput[30, 3]

  diverted$E031 = waterInput[31, 3]

  diverted$E033 = waterInput[33, 3]

  diverted$E034 = waterInput[34, 3]

  diverted$E032 = waterInput[32, 3]  + diverted$E033*nodesInput[180, 3]

  diverted$E029 = waterInput[29, 3] + diverted$E030*nodesInput[237, 3] + diverted$E031*nodesInput[236, 3] + diverted$E032*nodesInput[235, 3]+ diverted$E034*nodesInput[191, 3]

  # -----------------------------------------------------------------------

  diverted$E036 = waterInput[36, 3]

  diverted$E035 = waterInput[35, 3] + diverted$E036*nodesInput[190, 3]

  # -----------------------------------------------------------------------

  diverted$E038 = waterInput[38, 3]

  diverted$E037 = waterInput[37, 3] + diverted$E038*nodesInput[223, 3]

  # -----------------------------------------------------------------------

  diverted$E040 = waterInput[40, 3]

  diverted$E041 = waterInput[41, 3]

  diverted$E042 = waterInput[42, 3]

  diverted$E043 = waterInput[43, 3]

  diverted$E044 = waterInput[44 ,3]  + diverted$E042*nodesInput[238, 3]

  diverted$E039 = waterInput[39, 3] +
    diverted$E040*nodesInput[218, 3] +
    diverted$E041*nodesInput[220, 3] +
    diverted$E043*nodesInput[227, 3] +
    diverted$E044*nodesInput[217, 3]

  # -----------------------------------------------------------------------

  diverted$E047 = waterInput[47 ,3]
  diverted$E048 = waterInput[48 ,3]

  diverted$E049 = waterInput[49 ,3]

  diverted$E050 = waterInput[50 ,3]

  diverted$E051 = waterInput[51 ,3]

  diverted$E052 = waterInput[52 ,3]

  diverted$E053 = waterInput[53 ,3]

  diverted$E054 = waterInput[54 ,3]

  # basin 54 has two diversions on it
  spsk$E054s = waterInput[54, 3]*nodesInput[224, 3]*nodesInput[216, 3]
  spsk$E054 = ifelse(spsk$E054s<0, 0, spsk$E054s)

  diverted$E046 = waterInput[46 ,3] + spsk$E054 +
    diverted$E053*nodesInput[225,3] +
    diverted$E051*nodesInput[243, 3] +
    diverted$E050*nodesInput[242, 3] +
    diverted$E049*nodesInput[241, 3] +
    diverted$E047*nodesInput[240, 3] +
    diverted$E048*nodesInput[239, 3] +
    diverted$E052*nodesInput[222, 3]

  #  (diverted$E002*nodesInput[49, 3] + nodesInput[49, 13])

  # node 54 = sink
  spsk$E046s =  waterInput[46, 3]*nodesInput[54, 3] + nodesInput[54,13]
  spsk$E046 = ifelse(spsk$E046s<0 , 0, spsk$E046s)

  diverted$E045 = waterInput[45 ,3] + spsk$E046

  # -----------------------------------------------------------------------


  diverted$E059 = waterInput[59 ,3]

  diverted$E060 = waterInput[60 ,3]

  diverted$E061 = waterInput[61 ,3]

  diverted$E062 = waterInput[62 ,3]

  diverted$E063 = waterInput[63 ,3]

  diverted$E064 = waterInput[64 ,3]

  diverted$E058 = waterInput[58 ,3] +
    diverted$E063*nodesInput[214, 3] +
    diverted$E061*nodesInput[215, 3] +
    diverted$E060*nodesInput[208, 3] +
    diverted$E059*nodesInput[207, 3] +
    diverted$E062*nodesInput[205, 3]

  diverted$E056 = waterInput[56 ,3] + diverted$E064*nodesInput[226, 3]


  # node 53 = sink
  spsk$E058s = diverted$E058*nodesInput[53, 3] + nodesInput[53, 13]
  spsk$E058 = ifelse(spsk$E058s<0, 0, spsk$E028s)
  diverted$E057 = waterInput[57 ,3] + spsk$E058

  # node 52 = sink.
  spsk$E056s = diverted$E056*nodesInput[52,3] + nodesInput[52,13]
  spsk$E056 = ifelse(spsk$E056s<0, 0, spsk$E056s)
  diverted$E055 = waterInput[55 ,3] + spsk$E056
  # -----------------------------------------------------------------------
  diverted$E066 = waterInput[66 ,3]

  diverted$E067 = waterInput[67 ,3]

  diverted$E068 = waterInput[68 ,3]

  diverted$E069 = waterInput[69 ,3]

  diverted$E070 = waterInput[70 ,3]

  diverted$E065 = waterInput[65 ,3] +
    diverted$E066*nodesInput[204, 3] +
    diverted$E067*nodesInput[245, 3] +
    diverted$E068*nodesInput[247, 3] +
    diverted$E069*nodesInput[246, 3] +
    diverted$E070*nodesInput[221, 3]

  # -----------------------------------------------------------------------

  diverted$E073 = waterInput[73 ,3]

  diverted$E074 = waterInput[74 ,3]

  diverted$E075 = waterInput[75 ,3]

  diverted$E076 = waterInput[76 ,3]

  # basin 77 has two diversions
  diverted$E077 = waterInput[77 ,3]
  spsk$E077s = waterInput[77 ,3]*nodesInput[248,3]*nodesInput[200, 3]

  diverted$E078 = waterInput[78 ,3]

  diverted$E072 = waterInput[72 ,3] + diverted$E075*nodesInput[206, 3]

  diverted$E071 = waterInput[71 ,3] +
    diverted$E072*nodesInput[281, 3] +
    diverted$E073*nodesInput[249, 3] +
    diverted$E074*nodesInput[198, 3] +
    diverted$E076*nodesInput[199, 3] +
    spsk$E077s
  # -----------------------------------------------------------------------

  diverted$E080 = waterInput[80 ,3]

  diverted$E083 = waterInput[83 ,3]

  diverted$E084 = waterInput[84 ,3]

  diverted$E087 = waterInput[87 ,3]

  diverted$E088 = waterInput[88 ,3]

  diverted$E089 = waterInput[89 ,3]

  diverted$E086 = waterInput[86 ,3] + diverted$E088*nodesInput[251, 3]

  diverted$E085 = waterInput[85 ,3] +
    diverted$E089*nodesInput[197, 3] +
    diverted$E086*nodesInput[252, 3] +
    diverted$E087*nodesInput[250, 3]

  diverted$E082 = waterInput[82 ,3] +
    diverted$E083*nodesInput[280, 3] +
    diverted$E084*nodesInput[278, 3] +
    diverted$E085*nodesInput[111, 3]

  diverted$E081 = waterInput[81 ,3] + diverted$E082*nodesInput[113, 3]

  diverted$E079 = waterInput[79 ,3] +
    diverted$E080*nodesInput[284, 3] +
    diverted$E081*nodesInput[235, 3]

  # -----------------------------------------------------------------------

  diverted$E093  = waterInput[93 ,3]

  diverted$E092 = waterInput[92 ,3] + diverted$E093*nodesInput[72, 3]

  diverted$E091 = waterInput[91 ,3]+ diverted$E092*nodesInput[279, 3]

  diverted$E090 = waterInput[90 ,3] + diverted$E091*nodesInput[119, 3]

  # -----------------------------------------------------------------------
  diverted$E096 = waterInput[96 ,3]

  diverted$E097 = waterInput[97 ,3]

  diverted$E098 = waterInput[98 ,3]

  diverted$E102 = waterInput[102,3]

  diverted$E100 = waterInput[100,3]

  diverted$E095 = waterInput[95 ,3] +
    diverted$E096*nodesInput[256, 3] +
    diverted$E097*nodesInput[255, 3] +
    diverted$E098*nodesInput[254, 3]

  diverted$E101 = waterInput[101,3] + diverted$E102*nodesInput[73, 3]

  diverted$E099 = waterInput[99 ,3] +
    diverted$E100*nodesInput[253, 3] +
    diverted$E101*nodesInput[201, 3]

  diverted$E094 = waterInput[94 ,3] +
    diverted$E095*nodesInput[109, 3] +
    diverted$E099*nodesInput[100, 3]

  # -----------------------------------------------------------------------

  diverted$E104 = waterInput[104,3]

  diverted$E105 = waterInput[105,3]

  diverted$E106 = waterInput[106,3]

  diverted$E107 = waterInput[107,3]

  diverted$E350 = waterInput[309, 3]

  diverted$E103 = waterInput[103,3] +
    diverted$E104*nodesInput[289, 3] +
    diverted$E105*nodesInput[286, 3] +
    diverted$E350*nodesInput[287, 3] +
    diverted$E106*nodesInput[288, 3] +
    diverted$E107*nodesInput[112, 3]

  # -----------------------------------------------------------------------

  diverted$E110 = waterInput[110,3]

  diverted$E111 = waterInput[111,3]

  diverted$E114 = waterInput[114,3]

  diverted$E115 = waterInput[115,3]

  diverted$E112 = waterInput[112,3] + diverted$E114*nodesInput[71, 3]

  diverted$E113 = waterInput[113,3] + diverted$E115*nodesInput[139, 3]

  diverted$E109 = waterInput[109,3] +
    diverted$E113*nodesInput[74, 3] +
    diverted$E112*nodesInput[114, 3] +
    diverted$E111*nodesInput[258, 3] +
    diverted$E110*nodesInput[257, 3]

  diverted$E108 = waterInput[108,3] + diverted$E109*nodesInput[132, 3]
  # -----------------------------------------------------------------------
  diverted$E117 = waterInput[117,3]

  diverted$E125 = waterInput[125, 3]

  diverted$E123 = waterInput[123, 3]

  diverted$E124 = waterInput[124, 3] + diverted$E125*nodesInput[61, 3]

  diverted$E122 = waterInput[122, 3] + diverted$E123*nodesInput[67, 3]

  diverted$E121 = waterInput[121, 3] + diverted$E124*nodesInput[68, 3]

  diverted$E120 = waterInput[120, 3] +
    diverted$E122*nodesInput[116, 3] +
    diverted$E121*nodesInput[115, 3]

  diverted$E119 = waterInput[119, 3] + diverted$E120*nodesInput[135, 3]

  diverted$E118 = waterInput[118,3] + diverted$E119*nodesInput[130, 3]

  diverted$E116 = waterInput[116,3] +
    diverted$E117*nodesInput[290, 3] +
    diverted$E118*nodesInput[110, 3]

  # -----------------------------------------------------------------------

  diverted$E127 = waterInput[127, 3]

  diverted$E126 = waterInput[126, 3] + diverted$E127*nodesInput[128, 3]

  # -----------------------------------------------------------------------

  diverted$E132 = waterInput[132, 3]

  diverted$E131 = waterInput[131, 3] + diverted$E132*nodesInput[60, 3]

  diverted$E130 = waterInput[130, 3] + diverted$E131*nodesInput[282, 3]

  diverted$E129 = waterInput[129, 3] + diverted$E130*nodesInput[136, 3]

  diverted$E128 = waterInput[128, 3]+ diverted$E129*nodesInput[101, 3]

  # -----------------------------------------------------------------------

  diverted$E146 = waterInput[146, 3]

  diverted$E147 = waterInput[147, 3]

  diverted$E148 = waterInput[148, 3]

  diverted$E149 = waterInput[149, 3]

  diverted$E144 = waterInput[144, 3]

  diverted$E141 = waterInput[141, 3]

  diverted$E134 = waterInput[134, 3]

  diverted$E135 = waterInput[135, 3]

  diverted$E145 = waterInput[145, 3] + diverted$E146*nodesInput[56, 3]

  diverted$E143 = waterInput[143, 3] + diverted$E147*nodesInput[62, 3]

  diverted$E142 = waterInput[142, 3] + diverted$E148*nodesInput[83, 3]

  diverted$E140 = waterInput[140, 3] +
    diverted$E145*nodesInput[164, 3]+
    diverted$E144*nodesInput[165, 3]+
    diverted$E143*nodesInput[166, 3]+
    diverted$E149*nodesInput[81, 3]

  diverted$E139 = waterInput[139, 3] +
    diverted$E141*nodesInput[167, 3] +
    diverted$E142*nodesInput[179, 3]

  diverted$E138 = waterInput[138, 3]  + diverted$E139*nodesInput[203, 3]

  diverted$E137 = waterInput[137, 3] + diverted$E140*nodesInput[204, 3]

  diverted$E136 = waterInput[136, 3] + diverted$E138*nodesInput[293, 3]

  diverted$E133 = waterInput[133, 3] +
    diverted$E134*nodesInput[291, 3] +
    diverted$E135*nodesInput[102, 3] +
    diverted$E137*nodesInput[103, 3] +
    diverted$E136*nodesInput[106, 3]

  # -----------------------------------------------------------------------

  diverted$E156 = waterInput[156, 3]

  diverted$E153 = waterInput[153, 3]

  diverted$E154 = waterInput[154, 3]

  diverted$E151 = waterInput[151, 3]

  diverted$E155 = waterInput[155, 3] + diverted$E156*nodesInput[88, 3]

  diverted$E152 = waterInput[152, 3] +
    diverted$E153*nodesInput[212, 3] +
    diverted$E154*nodesInput[294, 3] +
    diverted$E155*nodesInput[176, 3]

  diverted$E150 = waterInput[150, 3] +
    diverted$E151*nodesInput[292, 3] +
    diverted$E152*nodesInput[107,3]

  # -----------------------------------------------------------------------

  diverted$E165 = waterInput[165, 3]

  diverted$E166 = waterInput[166, 3]

  diverted$E167 = waterInput[167, 3]

  diverted$E163 = waterInput[163, 3] + diverted$E166*nodesInput[77, 3]

  diverted$E164 = waterInput[164, 3] + diverted$E165*nodesInput[97, 3]

  diverted$E161 = waterInput[161, 3] + diverted$E167*nodesInput[78, 3]

  diverted$E160 = waterInput[160, 3] + diverted$E161*nodesInput[171, 3]

  diverted$E162 = waterInput[162, 3] +
    diverted$E163*nodesInput[172, 3] +
    diverted$E164*nodesInput[173, 3]

  diverted$E159 = waterInput[159, 3] +
    diverted$E160*nodesInput[159, 3] +
    diverted$E162*nodesInput[75, 3]

  diverted$E158 = waterInput[158, 3]+ diverted$E159*nodesInput[94, 3]

  diverted$E157 = waterInput[157, 3]+ diverted$E158*nodesInput[202, 3]
  # -----------------------------------------------------------------------

  diverted$E174 = waterInput[174, 3]

  diverted$E175 = waterInput[175, 3]

  # added new node 310 for basin 173
  diverted$E173 = waterInput[173, 3] + diverted$E175*nodesInput[92, 3]

  diverted$E172 = waterInput[172, 3] + diverted$E174*nodesInput[89, 3]

  diverted$E171 = waterInput[171, 3] +
    diverted$E172*nodesInput[177, 3] +
    diverted$E173*nodesInput[310, 3]

  diverted$E170 = waterInput[170, 3] + diverted$E171*nodesInput[95, 3]

  diverted$E169 = waterInput[169, 3] + diverted$E170*nodesInput[69, 3]

  diverted$E168 = waterInput[168, 3] + diverted$E169*nodesInput[154, 3]
  # -----------------------------------------------------------------------

  diverted$E178 = waterInput[178, 3]

  diverted$E177 = waterInput[177, 3] + diverted$E178*nodesInput[178, 3]

  diverted$E176 = waterInput[176, 3] + diverted$E177*nodesInput[86, 3]

  # -----------------------------------------------------------------------


  diverted$E188 = waterInput[188, 3]

  diverted$E186 = waterInput[186, 3]

  diverted$E183 = waterInput[183, 3]

  diverted$E184 = waterInput[184, 3]

  diverted$E182 = waterInput[182, 3] + diverted$E183*nodesInput[263, 3]

  diverted$E185 = waterInput[185, 3] + diverted$E188*nodesInput[93, 3]

  diverted$E187 = waterInput[187, 3] + diverted$E186*nodesInput[259, 3]

  diverted$E181 = waterInput[181, 3] +
    diverted$E184*nodesInput[261, 3] +
    diverted$E185*nodesInput[149, 3] +
    diverted$E187*nodesInput[260, 3] +
    diverted$E182*nodesInput[262, 3]

  diverted$E180 = waterInput[180, 3] + diverted$E181*nodesInput[55, 3]

  diverted$E179 = waterInput[179, 3] + diverted$E180*nodesInput[134, 3]

  # -----------------------------------------------------------------------

  diverted$E191 = waterInput[191, 3]
  diverted$E193 = waterInput[193, 3]

  diverted$E192 = waterInput[192, 3] + diverted$E193*nodesInput[265, 3]

  diverted$E202 = waterInput[202, 3]

  diverted$E201 = waterInput[201, 3]+ diverted$E202*nodesInput[98, 3]

  diverted$E200 = waterInput[200, 3] + diverted$E201*nodesInput[168, 3]

  diverted$E195 = waterInput[195, 3]

  diverted$E196 = waterInput[196, 3]

  diverted$E197 = waterInput[197, 3]

  diverted$E199 = waterInput[199, 3]

  diverted$E198 = waterInput[198, 3] + diverted$E199*nodesInput[264, 3]

  diverted$E194 = waterInput[194, 3] +
    diverted$E195*nodesInput[297, 3] +
    diverted$E196*nodesInput[296, 3] +
    diverted$E197*nodesInput[146, 3] +
    diverted$E198*nodesInput[66, 3] +
    diverted$E200*nodesInput[144, 3]

  diverted$E190 = waterInput[190, 3] +
    diverted$E191*nodesInput[299, 3] +
    diverted$E192*nodesInput[298, 3]

  diverted$E189 = waterInput[189, 3] +
    diverted$E194*nodesInput[123, 3] +
    diverted$E190*nodesInput[131, 3]

  # -----------------------------------------------------------------------
  diverted$E203 = waterInput[203, 3]
  # -----------------------------------------------------------------------

  diverted$E210 = waterInput[210, 3]

  diverted$E207 = waterInput[207, 3]

  diverted$E209 = waterInput[209, 3]+  diverted$E210*nodesInput[266, 3]

  diverted$E208 = waterInput[208, 3] + diverted$E209*nodesInput[108, 3]

  diverted$E205 = waterInput[205, 3] +
    diverted$E208*nodesInput[142, 3] +
    diverted$E207*nodesInput[143, 3]

  diverted$E204 = waterInput[204, 3] + diverted$E205*nodesInput[122, 3]

  # -----------------------------------------------------------------------
  # added new node 309 for basin 211
  diverted$E213 = waterInput[213, 3]

  diverted$E214 = waterInput[214, 3]

  diverted$E212 = waterInput[212, 3] +
    diverted$E214*nodesInput[174, 3] +
    diverted$E213*nodesInput[175, 3]

  diverted$E211 = waterInput[211, 3] + diverted$E212*nodesInput[120,3]
  # -----------------------------------------------------------------------
  diverted$E215 = waterInput[215, 3]
  # -----------------------------------------------------------------------

  diverted$E227 = waterInput[227, 3]

  diverted$E228 = waterInput[228, 3]

  diverted$E229 = waterInput[229, 3]

  diverted$E218 = waterInput[218, 3]

  diverted$E219 = waterInput[219, 3]

  diverted$E223 = waterInput[223, 3]

  diverted$E226 = waterInput[226, 3] +
    diverted$E223*nodesInput[268, 3] +
    diverted$E229*nodesInput[58, 3]

  diverted$E225 = waterInput[225, 3] + diverted$E227*nodesInput[57, 3]

  # assigned two diversions to basin 225
  diverted$E224 = waterInput[224, 3]+
    diverted$E225*nodesInput[148, 3]*nodesInput[267, 3] +
    diverted$E226*nodesInput[138, 3]

  diverted$E221 = waterInput[221, 3] + diverted$E224*nodesInput[158, 3]

  diverted$E222 = waterInput[222, 3] + diverted$E228*nodesInput[59, 3]

  diverted$E220 = waterInput[220, 3] + diverted$E221*nodesInput[140, 3]

  diverted$E217 = waterInput[217, 3] +
    diverted$E218*nodesInput[300, 3] +
    diverted$E219*nodesInput[141, 3] +
    diverted$E220*nodesInput[141, 3] +
    diverted$E222*nodesInput[147, 3]

  diverted$E216 = waterInput[216, 3] + diverted$E217*nodesInput[121, 3]
  # -----------------------------------------------------------------------

  diverted$E237 = waterInput[237, 3]

  diverted$E238 = waterInput[238, 3]

  diverted$E232 = waterInput[232, 3]

  diverted$E233 = waterInput[233, 3]

  diverted$E235 = waterInput[235, 3]

  diverted$E236 = waterInput[236, 3] + diverted$E238*nodesInput[63, 3]

  diverted$E234 = waterInput[234, 3] +
    diverted$E235*nodesInput[150, 3] +
    diverted$E236*nodesInput[151, 3] +
    diverted$E237*nodesInput[269, 3]

  diverted$E231 = waterInput[231, 3] +
    diverted$E234*nodesInput[117, 3] +
    diverted$E232*nodesInput[170, 3] +
    diverted$E233*nodesInput[161, 3]

  diverted$E230 = waterInput[230, 3] + diverted$E231*nodesInput[96, 3]

  # -----------------------------------------------------------------------

  diverted$E269 = waterInput[269, 3]

  diverted$E270 = waterInput[270, 3]

  diverted$E264 = waterInput[264, 3]

  diverted$E265 = waterInput[265, 3]

  diverted$E266 = waterInput[266, 3]

  diverted$E267 = waterInput[267, 3]

  diverted$E249 = waterInput[249, 3]

  diverted$E250 = waterInput[250, 3]

  diverted$E251 = waterInput[251, 3]

  diverted$E243 = waterInput[243, 3]

  diverted$E244 = waterInput[244, 3]

  diverted$E241 = waterInput[241, 3]

  diverted$E254 = waterInput[254, 3]

  diverted$E255 = waterInput[255, 3]

  diverted$E261 = waterInput[261, 3]

  diverted$E259 = waterInput[259, 3]

  diverted$E258 = waterInput[258, 3] + diverted$E259*nodesInput[153, 3]

  diverted$E257 = waterInput[257, 3] + diverted$E267*nodesInput[64, 3]

  diverted$E263 = waterInput[263, 3] + diverted$E270*nodesInput[275, 3]

  diverted$E256 = waterInput[256, 3] + diverted$E257*nodesInput[160, 3]

  diverted$E262 = waterInput[262, 3] + diverted$E265*nodesInput[272, 3]

  diverted$E268 = waterInput[268, 3] + diverted$E269*nodesInput[70, 3]

  diverted$E253 = waterInput[253, 3] +
    diverted$E256*nodesInput[156, 3] +
    diverted$E254*nodesInput[270, 3]

  diverted$E246 = waterInput[246, 3] +
    diverted$E258*nodesInput[209, 3] +
    diverted$E255*nodesInput[155, 3]

  diverted$E252 = waterInput[252, 3] + diverted$E253*nodesInput[125, 3]

  diverted$E260 = waterInput[260, 3] +
    diverted$E266*nodesInput[274, 3] +
    diverted$E264*nodesInput[273, 3] +
    diverted$E268*nodesInput[152, 3]

  diverted$E245 = waterInput[245, 3] +
    diverted$E261*nodesInput[271, 3] +
    diverted$E262*nodesInput[276, 3] +
    diverted$E263*nodesInput[210, 3]

  diverted$E248 = waterInput[248, 3] +
    diverted$E250*nodesInput[301, 3] +
    diverted$E252*nodesInput[91, 3]

  diverted$E247 = waterInput[247, 3] +
    diverted$E248*nodesInput[90, 3] +
    diverted$E249*nodesInput[295, 3] +
    diverted$E251*nodesInput[118, 3]

  diverted$E242 = waterInput[242, 3] +
    diverted$E241*nodesInput[303, 3] +
    diverted$E245*nodesInput[126, 3] +
    diverted$E246*nodesInput[105, 3] +
    diverted$E260*nodesInput[211,3]

  diverted$E240 = waterInput[240, 3] +
    diverted$E242*nodesInput[87, 3] +
    diverted$E243*nodesInput[124, 3] +
    diverted$E244*nodesInput[302, 3]

  diverted$E239 = waterInput[239, 3] +
    diverted$E240*nodesInput[46, 3] +
    diverted$E247*nodesInput[46, 3]

  # -----------------------------------------------------------------------
  # diverted$E271 = 0
  # -----------------------------------------------------------------------

  diverted$E273 = waterInput[273, 3]

  diverted$E272 = waterInput[272, 3] + diverted$E273*nodesInput[82, 3]

  # -----------------------------------------------------------------------

  diverted$E276 = waterInput[276, 3]

  diverted$E277 = waterInput[277, 3]

  diverted$E275 = waterInput[275, 3] +
    diverted$E277*nodesInput[80, 3] +
    diverted$E276*nodesInput[127, 3]

  diverted$E274 = waterInput[274, 3] +diverted$E275*nodesInput[79, 3]

  # -----------------------------------------------------------------------
  # converted nodeID = 0 to nodeID = 311

  diverted$E279 = waterInput[279, 3]

  diverted$E280 = waterInput[280, 3]

  diverted$E278 = waterInput[278, 3] +
    diverted$E280*nodesInput[104, 3] +
    diverted$E279*nodesInput[133, 3]

  # -----------------------------------------------------------------------
  # diverted$E281 = 0
  # -----------------------------------------------------------------------

  diverted$E283 = waterInput[283, 3]

  diverted$E284 = waterInput[284, 3]

  diverted$E285 = waterInput[285, 3]

  diverted$E286 = waterInput[286, 3]

  diverted$E282 = waterInput[282, 3] +
    diverted$E283*nodesInput[304, 3] +
    diverted$E284*nodesInput[305, 3] +
    diverted$E285*nodesInput[306, 3] +
    diverted$E286*nodesInput[169, 3]

  # -----------------------------------------------------------------------

  diverted$E292 = waterInput[292, 3]

  diverted$E293 = waterInput[293, 3]

  diverted$E291 = waterInput[291, 3] + diverted$E293*nodesInput[219, 3]

  diverted$E290 = waterInput[290, 3]+ diverted$E292*nodesInput[189, 3]

  diverted$E289 = waterInput[289, 3] +
    diverted$E290*nodesInput[163, 3] +
    diverted$E291*nodesInput[163, 3]

  diverted$E288 = waterInput[288, 3] + diverted$E289*nodesInput[162, 3]

  diverted$E287 = waterInput[287, 3] + diverted$E288*nodesInput[157,3]

  # -----------------------------------------------------------------------

  diverted$E295 = waterInput[295, 3]

  diverted$E294 = waterInput[294, 3] + diverted$E295*nodesInput[308, 3]

  # -----------------------------------------------------------------------

  diverted$E298 = waterInput[298, 3]

  diverted$E297 = waterInput[297, 3] + diverted$E298*nodesInput[213, 3]

  diverted$E296 = waterInput[296, 3] + diverted$E297*nodesInput[84, 3]

  # -----------------------------------------------------------------------

  diverted$E300 = waterInput[300, 3]

  diverted$E307 = waterInput[307, 3]

  diverted$E308 = waterInput[308, 3]

  diverted$E304 = waterInput[304, 3]

  diverted$E305 = waterInput[305, 3]

  diverted$E306 = waterInput[306, 3] +
    diverted$E307*nodesInput[277, 3] +
    diverted$E308*nodesInput[65, 3]

  diverted$E303 = waterInput[303, 3] + diverted$E304*nodesInput[145, 3]

  diverted$E302 = waterInput[302, 3] + diverted$E303*nodesInput[129, 3]

  diverted$E301 = waterInput[301, 3] + diverted$E302*nodesInput[99, 2]

  diverted$E299 = waterInput[299, 3] +
    diverted$E300*nodesInput[307,3] +
    diverted$E301*nodesInput[85, 3] +
    diverted$E305*nodesInput[76,3]

  # -------------------------------------------------------------------
  # Save output in dataframe
  output.df = as.data.frame(diverted)
  # scenario = data.frame(sapply(ls(pattern="^diverted$E[0-9]+$"),get))
  spsk.df = as.data.frame(spsk)
  return(output.df)
}

