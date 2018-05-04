#' @export
# Function to calculate amount of water going into ditches

# convert all nodeInputs[..., c(8:11)] to nodeInputs[..., 2]
diversion.fun = function(nodesInput, waterInput){
  inDitch = list()
  spsk = list()

# --------------------------------------------------------------------

  inDitch$n228 = waterInput[3, 3]*nodesInput[228, 2]

  inDitch$E004 = waterInput[4, 3]*nodesInput[196, 2]

  inDitch$E002 = waterInput[2, 3] +
    inDitch$E003*nodesInput[228, 3] +
    inDitch$E004*nodesInput[196, 3]

  # node 49 = sink
  spsk$E001s = (inDitch$E002*nodesInput[49, 3] + nodesInput[49, 13])

  spsk$E001 = ifelse(spsk$E001s<0, 0, spsk$E001s)

  inDitch$E001 = waterInput[1,3] + spsk$E001

# --------------------------------------------------------------------

  inDitch$E008 = waterInput[8, 3]

  inDitch$E009 = waterInput[9, 3]

  inDitch$E010 = waterInput[10, 3]

  inDitch$E007 = waterInput[7, 3] + inDitch$E009*nodesInput[192, 3] + inDitch$E008*nodesInput[193, 3] + inDitch$E010*nodesInput[194, 3]

  inDitch$E006 = waterInput[6, 3] + inDitch$E007*nodesInput[229, 3]
  # node 50 = spring
  spsk$E005s =  (nodesInput[50,13] + inDitch$E006*nodesInput[50,3])

  spsk$E005 = ifelse(spsk$E005s<0, 0, spsk$E005s)

  inDitch$E005 = waterInput[5, 3] + spsk$E005

  # --------------------------------------------------------------------
  inDitch$E014 = waterInput[14, 3]

  inDitch$E015 = waterInput[15, 3]

  inDitch$E016 = waterInput[16, 3] + inDitch$E015*nodesInput[187, 3]

  inDitch$E013 = waterInput[13, 3] + inDitch$E016*nodesInput[188, 3]

  inDitch$E012 = waterInput[12, 3] +
    inDitch$E013*nodesInput[195, 3] +
    inDitch$E014*nodesInput[186, 3]

  # node 51 = spring

  spsk$E011s =  (nodesInput[51,13] + inDitch$E012*nodesInput[51,3])

  spsk$E011 = ifelse(spsk$E011s<0, 0, spsk$E011s)

  inDitch$E011 = waterInput[11, 3] + spsk$E011

  # --------------------------------------------------------------------

  inDitch$E018 = waterInput[18, 3]

  inDitch$E017 = waterInput[17, 3] +  inDitch$E018*nodesInput[230, 3]

  # ---------------------------------------------------------------------

  inDitch$E020 = waterInput[20, 3]

  inDitch$E021 = waterInput[21, 3]

  inDitch$E022 = waterInput[22, 3]

  inDitch$E023 = waterInput[23, 3]

  inDitch$E019 = waterInput[19, 3] +
    inDitch$E023*nodesInput[184, 3]+
    inDitch$E022*nodesInput[185, 3]+
    inDitch$E021*nodesInput[231, 3]+
    inDitch$E020*nodesInput[232, 3]

  # -----------------------------------------------------------------------
  inDitch$E025 = waterInput[25, 3]

  inDitch$E026 = waterInput[26, 3]

  inDitch$E027 = waterInput[27, 3]

  inDitch$E028 = waterInput[28, 3]

  inDitch$E024 = waterInput[24, 3] +
    inDitch$E025*nodesInput[233, 3] +
    inDitch$E026*nodesInput[234, 3] +
    inDitch$E027*nodesInput[182, 3]+
    inDitch$E028*nodesInput[183, 3]

  # -----------------------------------------------------------------------

  inDitch$E030 = waterInput[30, 3]

  inDitch$E031 = waterInput[31, 3]

  inDitch$E033 = waterInput[33, 3]

  inDitch$E034 = waterInput[34, 3]

  inDitch$E032 = waterInput[32, 3]  + inDitch$E033*nodesInput[180, 3]

  inDitch$E029 = waterInput[29, 3] + inDitch$E030*nodesInput[237, 3] + inDitch$E031*nodesInput[236, 3] + inDitch$E032*nodesInput[235, 3]+ inDitch$E034*nodesInput[191, 3]

  # -----------------------------------------------------------------------

  inDitch$E036 = waterInput[36, 3]

  inDitch$E035 = waterInput[35, 3] + inDitch$E036*nodesInput[190, 3]

  # -----------------------------------------------------------------------

  inDitch$E038 = waterInput[38, 3]

  inDitch$E037 = waterInput[37, 3] + inDitch$E038*nodesInput[223, 3]

  # -----------------------------------------------------------------------

  inDitch$E040 = waterInput[40, 3]

  inDitch$E041 = waterInput[41, 3]

  inDitch$E042 = waterInput[42, 3]

  inDitch$E043 = waterInput[43, 3]

  inDitch$E044 = waterInput[44 ,3]  + inDitch$E042*nodesInput[238, 3]

  inDitch$E039 = waterInput[39, 3] +
    inDitch$E040*nodesInput[218, 3] +
    inDitch$E041*nodesInput[220, 3] +
    inDitch$E043*nodesInput[227, 3] +
    inDitch$E044*nodesInput[217, 3]

  # -----------------------------------------------------------------------

  inDitch$E047 = waterInput[47 ,3]
  inDitch$E048 = waterInput[48 ,3]

  inDitch$E049 = waterInput[49 ,3]

  inDitch$E050 = waterInput[50 ,3]

  inDitch$E051 = waterInput[51 ,3]

  inDitch$E052 = waterInput[52 ,3]

  inDitch$E053 = waterInput[53 ,3]

  inDitch$E054 = waterInput[54 ,3]

  # basin 54 has two diversions on it
  spsk$E054s = waterInput[54, 3]*nodesInput[224, 3]*nodesInput[216, 3]
  spsk$E054 = ifelse(spsk$E054s<0, 0, spsk$E054s)

  inDitch$E046 = waterInput[46 ,3] + spsk$E054 +
    inDitch$E053*nodesInput[225,3] +
    inDitch$E051*nodesInput[243, 3] +
    inDitch$E050*nodesInput[242, 3] +
    inDitch$E049*nodesInput[241, 3] +
    inDitch$E047*nodesInput[240, 3] +
    inDitch$E048*nodesInput[239, 3] +
    inDitch$E052*nodesInput[222, 3]

  #  (inDitch$E002*nodesInput[49, 3] + nodesInput[49, 13])

  # node 54 = sink
  spsk$E046s =  waterInput[46, 3]*nodesInput[54, 3] + nodesInput[54,13]
  spsk$E046 = ifelse(spsk$E046s<0 , 0, spsk$E046s)

  inDitch$E045 = waterInput[45 ,3] + spsk$E046

  # -----------------------------------------------------------------------


  inDitch$E059 = waterInput[59 ,3]

  inDitch$E060 = waterInput[60 ,3]

  inDitch$E061 = waterInput[61 ,3]

  inDitch$E062 = waterInput[62 ,3]

  inDitch$E063 = waterInput[63 ,3]

  inDitch$E064 = waterInput[64 ,3]

  inDitch$E058 = waterInput[58 ,3] +
    inDitch$E063*nodesInput[214, 3] +
    inDitch$E061*nodesInput[215, 3] +
    inDitch$E060*nodesInput[208, 3] +
    inDitch$E059*nodesInput[207, 3] +
    inDitch$E062*nodesInput[205, 3]

  inDitch$E056 = waterInput[56 ,3] + inDitch$E064*nodesInput[226, 3]


  # node 53 = sink
  spsk$E058s = inDitch$E058*nodesInput[53, 3] + nodesInput[53, 13]
  spsk$E058 = ifelse(spsk$E058s<0, 0, spsk$E028s)
  inDitch$E057 = waterInput[57 ,3] + spsk$E058

  # node 52 = sink.
  spsk$E056s = inDitch$E056*nodesInput[52,3] + nodesInput[52,13]
  spsk$E056 = ifelse(spsk$E056s<0, 0, spsk$E056s)
  inDitch$E055 = waterInput[55 ,3] + spsk$E056
  # -----------------------------------------------------------------------
  inDitch$E066 = waterInput[66 ,3]

  inDitch$E067 = waterInput[67 ,3]

  inDitch$E068 = waterInput[68 ,3]

  inDitch$E069 = waterInput[69 ,3]

  inDitch$E070 = waterInput[70 ,3]

  inDitch$E065 = waterInput[65 ,3] +
    inDitch$E066*nodesInput[204, 3] +
    inDitch$E067*nodesInput[245, 3] +
    inDitch$E068*nodesInput[247, 3] +
    inDitch$E069*nodesInput[246, 3] +
    inDitch$E070*nodesInput[221, 3]

  # -----------------------------------------------------------------------

  inDitch$E073 = waterInput[73 ,3]

  inDitch$E074 = waterInput[74 ,3]

  inDitch$E075 = waterInput[75 ,3]

  inDitch$E076 = waterInput[76 ,3]

  # basin 77 has two diversions
  inDitch$E077 = waterInput[77 ,3]
  spsk$E077s = waterInput[77 ,3]*nodesInput[248,3]*nodesInput[200, 3]

  inDitch$E078 = waterInput[78 ,3]

  inDitch$E072 = waterInput[72 ,3] + inDitch$E075*nodesInput[206, 3]

  inDitch$E071 = waterInput[71 ,3] +
    inDitch$E072*nodesInput[281, 3] +
    inDitch$E073*nodesInput[249, 3] +
    inDitch$E074*nodesInput[198, 3] +
    inDitch$E076*nodesInput[199, 3] +
    spsk$E077s
  # -----------------------------------------------------------------------

  inDitch$E080 = waterInput[80 ,3]

  inDitch$E083 = waterInput[83 ,3]

  inDitch$E084 = waterInput[84 ,3]

  inDitch$E087 = waterInput[87 ,3]

  inDitch$E088 = waterInput[88 ,3]

  inDitch$E089 = waterInput[89 ,3]

  inDitch$E086 = waterInput[86 ,3] + inDitch$E088*nodesInput[251, 3]

  inDitch$E085 = waterInput[85 ,3] +
    inDitch$E089*nodesInput[197, 3] +
    inDitch$E086*nodesInput[252, 3] +
    inDitch$E087*nodesInput[250, 3]

  inDitch$E082 = waterInput[82 ,3] +
    inDitch$E083*nodesInput[280, 3] +
    inDitch$E084*nodesInput[278, 3] +
    inDitch$E085*nodesInput[111, 3]

  inDitch$E081 = waterInput[81 ,3] + inDitch$E082*nodesInput[113, 3]

  inDitch$E079 = waterInput[79 ,3] +
    inDitch$E080*nodesInput[284, 3] +
    inDitch$E081*nodesInput[235, 3]

  # -----------------------------------------------------------------------

  inDitch$E093  = waterInput[93 ,3]

  inDitch$E092 = waterInput[92 ,3] + inDitch$E093*nodesInput[72, 3]

  inDitch$E091 = waterInput[91 ,3]+ inDitch$E092*nodesInput[279, 3]

  inDitch$E090 = waterInput[90 ,3] + inDitch$E091*nodesInput[119, 3]

  # -----------------------------------------------------------------------
  inDitch$E096 = waterInput[96 ,3]

  inDitch$E097 = waterInput[97 ,3]

  inDitch$E098 = waterInput[98 ,3]

  inDitch$E102 = waterInput[102,3]

  inDitch$E100 = waterInput[100,3]

  inDitch$E095 = waterInput[95 ,3] +
    inDitch$E096*nodesInput[256, 3] +
    inDitch$E097*nodesInput[255, 3] +
    inDitch$E098*nodesInput[254, 3]

  inDitch$E101 = waterInput[101,3] + inDitch$E102*nodesInput[73, 3]

  inDitch$E099 = waterInput[99 ,3] +
    inDitch$E100*nodesInput[253, 3] +
    inDitch$E101*nodesInput[201, 3]

  inDitch$E094 = waterInput[94 ,3] +
    inDitch$E095*nodesInput[109, 3] +
    inDitch$E099*nodesInput[100, 3]

  # -----------------------------------------------------------------------

  inDitch$E104 = waterInput[104,3]

  inDitch$E105 = waterInput[105,3]

  inDitch$E106 = waterInput[106,3]

  inDitch$E107 = waterInput[107,3]

  inDitch$E350 = waterInput[309, 3]

  inDitch$E103 = waterInput[103,3] +
    inDitch$E104*nodesInput[289, 3] +
    inDitch$E105*nodesInput[286, 3] +
    inDitch$E350*nodesInput[287, 3] +
    inDitch$E106*nodesInput[288, 3] +
    inDitch$E107*nodesInput[112, 3]

  # -----------------------------------------------------------------------

  inDitch$E110 = waterInput[110,3]

  inDitch$E111 = waterInput[111,3]

  inDitch$E114 = waterInput[114,3]

  inDitch$E115 = waterInput[115,3]

  inDitch$E112 = waterInput[112,3] + inDitch$E114*nodesInput[71, 3]

  inDitch$E113 = waterInput[113,3] + inDitch$E115*nodesInput[139, 3]

  inDitch$E109 = waterInput[109,3] +
    inDitch$E113*nodesInput[74, 3] +
    inDitch$E112*nodesInput[114, 3] +
    inDitch$E111*nodesInput[258, 3] +
    inDitch$E110*nodesInput[257, 3]

  inDitch$E108 = waterInput[108,3] + inDitch$E109*nodesInput[132, 3]
  # -----------------------------------------------------------------------
  inDitch$E117 = waterInput[117,3]

  inDitch$E125 = waterInput[125, 3]

  inDitch$E123 = waterInput[123, 3]

  inDitch$E124 = waterInput[124, 3] + inDitch$E125*nodesInput[61, 3]

  inDitch$E122 = waterInput[122, 3] + inDitch$E123*nodesInput[67, 3]

  inDitch$E121 = waterInput[121, 3] + inDitch$E124*nodesInput[68, 3]

  inDitch$E120 = waterInput[120, 3] +
    inDitch$E122*nodesInput[116, 3] +
    inDitch$E121*nodesInput[115, 3]

  inDitch$E119 = waterInput[119, 3] + inDitch$E120*nodesInput[135, 3]

  inDitch$E118 = waterInput[118,3] + inDitch$E119*nodesInput[130, 3]

  inDitch$E116 = waterInput[116,3] +
    inDitch$E117*nodesInput[290, 3] +
    inDitch$E118*nodesInput[110, 3]

  # -----------------------------------------------------------------------

  inDitch$E127 = waterInput[127, 3]

  inDitch$E126 = waterInput[126, 3] + inDitch$E127*nodesInput[128, 3]

  # -----------------------------------------------------------------------

  inDitch$E132 = waterInput[132, 3]

  inDitch$E131 = waterInput[131, 3] + inDitch$E132*nodesInput[60, 3]

  inDitch$E130 = waterInput[130, 3] + inDitch$E131*nodesInput[282, 3]

  inDitch$E129 = waterInput[129, 3] + inDitch$E130*nodesInput[136, 3]

  inDitch$E128 = waterInput[128, 3]+ inDitch$E129*nodesInput[101, 3]

  # -----------------------------------------------------------------------

  inDitch$E146 = waterInput[146, 3]

  inDitch$E147 = waterInput[147, 3]

  inDitch$E148 = waterInput[148, 3]

  inDitch$E149 = waterInput[149, 3]

  inDitch$E144 = waterInput[144, 3]

  inDitch$E141 = waterInput[141, 3]

  inDitch$E134 = waterInput[134, 3]

  inDitch$E135 = waterInput[135, 3]

  inDitch$E145 = waterInput[145, 3] + inDitch$E146*nodesInput[56, 3]

  inDitch$E143 = waterInput[143, 3] + inDitch$E147*nodesInput[62, 3]

  inDitch$E142 = waterInput[142, 3] + inDitch$E148*nodesInput[83, 3]

  inDitch$E140 = waterInput[140, 3] +
    inDitch$E145*nodesInput[164, 3]+
    inDitch$E144*nodesInput[165, 3]+
    inDitch$E143*nodesInput[166, 3]+
    inDitch$E149*nodesInput[81, 3]

  inDitch$E139 = waterInput[139, 3] +
    inDitch$E141*nodesInput[167, 3] +
    inDitch$E142*nodesInput[179, 3]

  inDitch$E138 = waterInput[138, 3]  + inDitch$E139*nodesInput[203, 3]

  inDitch$E137 = waterInput[137, 3] + inDitch$E140*nodesInput[204, 3]

  inDitch$E136 = waterInput[136, 3] + inDitch$E138*nodesInput[293, 3]

  inDitch$E133 = waterInput[133, 3] +
    inDitch$E134*nodesInput[291, 3] +
    inDitch$E135*nodesInput[102, 3] +
    inDitch$E137*nodesInput[103, 3] +
    inDitch$E136*nodesInput[106, 3]

  # -----------------------------------------------------------------------

  inDitch$E156 = waterInput[156, 3]

  inDitch$E153 = waterInput[153, 3]

  inDitch$E154 = waterInput[154, 3]

  inDitch$E151 = waterInput[151, 3]

  inDitch$E155 = waterInput[155, 3] + inDitch$E156*nodesInput[88, 3]

  inDitch$E152 = waterInput[152, 3] +
    inDitch$E153*nodesInput[212, 3] +
    inDitch$E154*nodesInput[294, 3] +
    inDitch$E155*nodesInput[176, 3]

  inDitch$E150 = waterInput[150, 3] +
    inDitch$E151*nodesInput[292, 3] +
    inDitch$E152*nodesInput[107,3]

  # -----------------------------------------------------------------------

  inDitch$E165 = waterInput[165, 3]

  inDitch$E166 = waterInput[166, 3]

  inDitch$E167 = waterInput[167, 3]

  inDitch$E163 = waterInput[163, 3] + inDitch$E166*nodesInput[77, 3]

  inDitch$E164 = waterInput[164, 3] + inDitch$E165*nodesInput[97, 3]

  inDitch$E161 = waterInput[161, 3] + inDitch$E167*nodesInput[78, 3]

  inDitch$E160 = waterInput[160, 3] + inDitch$E161*nodesInput[171, 3]

  inDitch$E162 = waterInput[162, 3] +
    inDitch$E163*nodesInput[172, 3] +
    inDitch$E164*nodesInput[173, 3]

  inDitch$E159 = waterInput[159, 3] +
    inDitch$E160*nodesInput[159, 3] +
    inDitch$E162*nodesInput[75, 3]

  inDitch$E158 = waterInput[158, 3]+ inDitch$E159*nodesInput[94, 3]

  inDitch$E157 = waterInput[157, 3]+ inDitch$E158*nodesInput[202, 3]
  # -----------------------------------------------------------------------

  inDitch$E174 = waterInput[174, 3]

  inDitch$E175 = waterInput[175, 3]

  # added new node 310 for basin 173
  inDitch$E173 = waterInput[173, 3] + inDitch$E175*nodesInput[92, 3]

  inDitch$E172 = waterInput[172, 3] + inDitch$E174*nodesInput[89, 3]

  inDitch$E171 = waterInput[171, 3] +
    inDitch$E172*nodesInput[177, 3] +
    inDitch$E173*nodesInput[310, 3]

  inDitch$E170 = waterInput[170, 3] + inDitch$E171*nodesInput[95, 3]

  inDitch$E169 = waterInput[169, 3] + inDitch$E170*nodesInput[69, 3]

  inDitch$E168 = waterInput[168, 3] + inDitch$E169*nodesInput[154, 3]
  # -----------------------------------------------------------------------

  inDitch$E178 = waterInput[178, 3]

  inDitch$E177 = waterInput[177, 3] + inDitch$E178*nodesInput[178, 3]

  inDitch$E176 = waterInput[176, 3] + inDitch$E177*nodesInput[86, 3]

  # -----------------------------------------------------------------------


  inDitch$E188 = waterInput[188, 3]

  inDitch$E186 = waterInput[186, 3]

  inDitch$E183 = waterInput[183, 3]

  inDitch$E184 = waterInput[184, 3]

  inDitch$E182 = waterInput[182, 3] + inDitch$E183*nodesInput[263, 3]

  inDitch$E185 = waterInput[185, 3] + inDitch$E188*nodesInput[93, 3]

  inDitch$E187 = waterInput[187, 3] + inDitch$E186*nodesInput[259, 3]

  inDitch$E181 = waterInput[181, 3] +
    inDitch$E184*nodesInput[261, 3] +
    inDitch$E185*nodesInput[149, 3] +
    inDitch$E187*nodesInput[260, 3] +
    inDitch$E182*nodesInput[262, 3]

  inDitch$E180 = waterInput[180, 3] + inDitch$E181*nodesInput[55, 3]

  inDitch$E179 = waterInput[179, 3] + inDitch$E180*nodesInput[134, 3]

  # -----------------------------------------------------------------------

  inDitch$E191 = waterInput[191, 3]
  inDitch$E193 = waterInput[193, 3]

  inDitch$E192 = waterInput[192, 3] + inDitch$E193*nodesInput[265, 3]

  inDitch$E202 = waterInput[202, 3]

  inDitch$E201 = waterInput[201, 3]+ inDitch$E202*nodesInput[98, 3]

  inDitch$E200 = waterInput[200, 3] + inDitch$E201*nodesInput[168, 3]

  inDitch$E195 = waterInput[195, 3]

  inDitch$E196 = waterInput[196, 3]

  inDitch$E197 = waterInput[197, 3]

  inDitch$E199 = waterInput[199, 3]

  inDitch$E198 = waterInput[198, 3] + inDitch$E199*nodesInput[264, 3]

  inDitch$E194 = waterInput[194, 3] +
    inDitch$E195*nodesInput[297, 3] +
    inDitch$E196*nodesInput[296, 3] +
    inDitch$E197*nodesInput[146, 3] +
    inDitch$E198*nodesInput[66, 3] +
    inDitch$E200*nodesInput[144, 3]

  inDitch$E190 = waterInput[190, 3] +
    inDitch$E191*nodesInput[299, 3] +
    inDitch$E192*nodesInput[298, 3]

  inDitch$E189 = waterInput[189, 3] +
    inDitch$E194*nodesInput[123, 3] +
    inDitch$E190*nodesInput[131, 3]

  # -----------------------------------------------------------------------
  inDitch$E203 = waterInput[203, 3]
  # -----------------------------------------------------------------------

  inDitch$E210 = waterInput[210, 3]

  inDitch$E207 = waterInput[207, 3]

  inDitch$E209 = waterInput[209, 3]+  inDitch$E210*nodesInput[266, 3]

  inDitch$E208 = waterInput[208, 3] + inDitch$E209*nodesInput[108, 3]

  inDitch$E205 = waterInput[205, 3] +
    inDitch$E208*nodesInput[142, 3] +
    inDitch$E207*nodesInput[143, 3]

  inDitch$E204 = waterInput[204, 3] + inDitch$E205*nodesInput[122, 3]

  # -----------------------------------------------------------------------
  # added new node 309 for basin 211
  inDitch$E213 = waterInput[213, 3]

  inDitch$E214 = waterInput[214, 3]

  inDitch$E212 = waterInput[212, 3] +
    inDitch$E214*nodesInput[174, 3] +
    inDitch$E213*nodesInput[175, 3]

  inDitch$E211 = waterInput[211, 3] + inDitch$E212*nodesInput[120,3]
  # -----------------------------------------------------------------------
  inDitch$E215 = waterInput[215, 3]
  # -----------------------------------------------------------------------

  inDitch$E227 = waterInput[227, 3]

  inDitch$E228 = waterInput[228, 3]

  inDitch$E229 = waterInput[229, 3]

  inDitch$E218 = waterInput[218, 3]

  inDitch$E219 = waterInput[219, 3]

  inDitch$E223 = waterInput[223, 3]

  inDitch$E226 = waterInput[226, 3] +
    inDitch$E223*nodesInput[268, 3] +
    inDitch$E229*nodesInput[58, 3]

  inDitch$E225 = waterInput[225, 3] + inDitch$E227*nodesInput[57, 3]

  # assigned two diversions to basin 225
  inDitch$E224 = waterInput[224, 3]+
    inDitch$E225*nodesInput[148, 3]*nodesInput[267, 3] +
    inDitch$E226*nodesInput[138, 3]

  inDitch$E221 = waterInput[221, 3] + inDitch$E224*nodesInput[158, 3]

  inDitch$E222 = waterInput[222, 3] + inDitch$E228*nodesInput[59, 3]

  inDitch$E220 = waterInput[220, 3] + inDitch$E221*nodesInput[140, 3]

  inDitch$E217 = waterInput[217, 3] +
    inDitch$E218*nodesInput[300, 3] +
    inDitch$E219*nodesInput[141, 3] +
    inDitch$E220*nodesInput[141, 3] +
    inDitch$E222*nodesInput[147, 3]

  inDitch$E216 = waterInput[216, 3] + inDitch$E217*nodesInput[121, 3]
  # -----------------------------------------------------------------------

  inDitch$E237 = waterInput[237, 3]

  inDitch$E238 = waterInput[238, 3]

  inDitch$E232 = waterInput[232, 3]

  inDitch$E233 = waterInput[233, 3]

  inDitch$E235 = waterInput[235, 3]

  inDitch$E236 = waterInput[236, 3] + inDitch$E238*nodesInput[63, 3]

  inDitch$E234 = waterInput[234, 3] +
    inDitch$E235*nodesInput[150, 3] +
    inDitch$E236*nodesInput[151, 3] +
    inDitch$E237*nodesInput[269, 3]

  inDitch$E231 = waterInput[231, 3] +
    inDitch$E234*nodesInput[117, 3] +
    inDitch$E232*nodesInput[170, 3] +
    inDitch$E233*nodesInput[161, 3]

  inDitch$E230 = waterInput[230, 3] + inDitch$E231*nodesInput[96, 3]

  # -----------------------------------------------------------------------

  inDitch$E269 = waterInput[269, 3]

  inDitch$E270 = waterInput[270, 3]

  inDitch$E264 = waterInput[264, 3]

  inDitch$E265 = waterInput[265, 3]

  inDitch$E266 = waterInput[266, 3]

  inDitch$E267 = waterInput[267, 3]

  inDitch$E249 = waterInput[249, 3]

  inDitch$E250 = waterInput[250, 3]

  inDitch$E251 = waterInput[251, 3]

  inDitch$E243 = waterInput[243, 3]

  inDitch$E244 = waterInput[244, 3]

  inDitch$E241 = waterInput[241, 3]

  inDitch$E254 = waterInput[254, 3]

  inDitch$E255 = waterInput[255, 3]

  inDitch$E261 = waterInput[261, 3]

  inDitch$E259 = waterInput[259, 3]

  inDitch$E258 = waterInput[258, 3] + inDitch$E259*nodesInput[153, 3]

  inDitch$E257 = waterInput[257, 3] + inDitch$E267*nodesInput[64, 3]

  inDitch$E263 = waterInput[263, 3] + inDitch$E270*nodesInput[275, 3]

  inDitch$E256 = waterInput[256, 3] + inDitch$E257*nodesInput[160, 3]

  inDitch$E262 = waterInput[262, 3] + inDitch$E265*nodesInput[272, 3]

  inDitch$E268 = waterInput[268, 3] + inDitch$E269*nodesInput[70, 3]

  inDitch$E253 = waterInput[253, 3] +
    inDitch$E256*nodesInput[156, 3] +
    inDitch$E254*nodesInput[270, 3]

  inDitch$E246 = waterInput[246, 3] +
    inDitch$E258*nodesInput[209, 3] +
    inDitch$E255*nodesInput[155, 3]

  inDitch$E252 = waterInput[252, 3] + inDitch$E253*nodesInput[125, 3]

  inDitch$E260 = waterInput[260, 3] +
    inDitch$E266*nodesInput[274, 3] +
    inDitch$E264*nodesInput[273, 3] +
    inDitch$E268*nodesInput[152, 3]

  inDitch$E245 = waterInput[245, 3] +
    inDitch$E261*nodesInput[271, 3] +
    inDitch$E262*nodesInput[276, 3] +
    inDitch$E263*nodesInput[210, 3]

  inDitch$E248 = waterInput[248, 3] +
    inDitch$E250*nodesInput[301, 3] +
    inDitch$E252*nodesInput[91, 3]

  inDitch$E247 = waterInput[247, 3] +
    inDitch$E248*nodesInput[90, 3] +
    inDitch$E249*nodesInput[295, 3] +
    inDitch$E251*nodesInput[118, 3]

  inDitch$E242 = waterInput[242, 3] +
    inDitch$E241*nodesInput[303, 3] +
    inDitch$E245*nodesInput[126, 3] +
    inDitch$E246*nodesInput[105, 3] +
    inDitch$E260*nodesInput[211,3]

  inDitch$E240 = waterInput[240, 3] +
    inDitch$E242*nodesInput[87, 3] +
    inDitch$E243*nodesInput[124, 3] +
    inDitch$E244*nodesInput[302, 3]

  inDitch$E239 = waterInput[239, 3] +
    inDitch$E240*nodesInput[46, 3] +
    inDitch$E247*nodesInput[46, 3]

  # -----------------------------------------------------------------------
  # inDitch$E271 = 0
  # -----------------------------------------------------------------------

  inDitch$E273 = waterInput[273, 3]

  inDitch$E272 = waterInput[272, 3] + inDitch$E273*nodesInput[82, 3]

  # -----------------------------------------------------------------------

  inDitch$E276 = waterInput[276, 3]

  inDitch$E277 = waterInput[277, 3]

  inDitch$E275 = waterInput[275, 3] +
    inDitch$E277*nodesInput[80, 3] +
    inDitch$E276*nodesInput[127, 3]

  inDitch$E274 = waterInput[274, 3] +inDitch$E275*nodesInput[79, 3]

  # -----------------------------------------------------------------------
  # converted nodeID = 0 to nodeID = 311

  inDitch$E279 = waterInput[279, 3]

  inDitch$E280 = waterInput[280, 3]

  inDitch$E278 = waterInput[278, 3] +
    inDitch$E280*nodesInput[104, 3] +
    inDitch$E279*nodesInput[133, 3]

  # -----------------------------------------------------------------------
  # inDitch$E281 = 0
  # -----------------------------------------------------------------------

  inDitch$E283 = waterInput[283, 3]

  inDitch$E284 = waterInput[284, 3]

  inDitch$E285 = waterInput[285, 3]

  inDitch$E286 = waterInput[286, 3]

  inDitch$E282 = waterInput[282, 3] +
    inDitch$E283*nodesInput[304, 3] +
    inDitch$E284*nodesInput[305, 3] +
    inDitch$E285*nodesInput[306, 3] +
    inDitch$E286*nodesInput[169, 3]

  # -----------------------------------------------------------------------

  inDitch$E292 = waterInput[292, 3]

  inDitch$E293 = waterInput[293, 3]

  inDitch$E291 = waterInput[291, 3] + inDitch$E293*nodesInput[219, 3]

  inDitch$E290 = waterInput[290, 3]+ inDitch$E292*nodesInput[189, 3]

  inDitch$E289 = waterInput[289, 3] +
    inDitch$E290*nodesInput[163, 3] +
    inDitch$E291*nodesInput[163, 3]

  inDitch$E288 = waterInput[288, 3] + inDitch$E289*nodesInput[162, 3]

  inDitch$E287 = waterInput[287, 3] + inDitch$E288*nodesInput[157,3]

  # -----------------------------------------------------------------------

  inDitch$E295 = waterInput[295, 3]

  inDitch$E294 = waterInput[294, 3] + inDitch$E295*nodesInput[308, 3]

  # -----------------------------------------------------------------------

  inDitch$E298 = waterInput[298, 3]

  inDitch$E297 = waterInput[297, 3] + inDitch$E298*nodesInput[213, 3]

  inDitch$E296 = waterInput[296, 3] + inDitch$E297*nodesInput[84, 3]

  # -----------------------------------------------------------------------

  inDitch$E300 = waterInput[300, 3]

  inDitch$E307 = waterInput[307, 3]

  inDitch$E308 = waterInput[308, 3]

  inDitch$E304 = waterInput[304, 3]

  inDitch$E305 = waterInput[305, 3]

  inDitch$E306 = waterInput[306, 3] +
    inDitch$E307*nodesInput[277, 3] +
    inDitch$E308*nodesInput[65, 3]

  inDitch$E303 = waterInput[303, 3] + inDitch$E304*nodesInput[145, 3]

  inDitch$E302 = waterInput[302, 3] + inDitch$E303*nodesInput[129, 3]

  inDitch$E301 = waterInput[301, 3] + inDitch$E302*nodesInput[99, 2]

  inDitch$E299 = waterInput[299, 3] +
    inDitch$E300*nodesInput[307,3] +
    inDitch$E301*nodesInput[85, 3] +
    inDitch$E305*nodesInput[76,3]

  # -------------------------------------------------------------------
  # Save output in dataframe
  output.df = as.data.frame(inDitch)
  # scenario = data.frame(sapply(ls(pattern="^inDitch$E[0-9]+$"),get))
  spsk.df = as.data.frame(spsk)
  return(output.df)
}

