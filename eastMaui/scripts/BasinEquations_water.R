#' @export

# waterInput = waterInput.mat


water.fun = function(nodesInput, waterInput){
  wateroutput = list()

  wateroutput$E001 = waterInput[1, 3] + waterInput[2, 3]*nodesInput[49, 3]

  wateroutput$E002 = waterInput[2, 3] + waterInput[3, 3]*nodesInput[228, 3]

  wateroutput$E003 = waterInput[3, 3]

  wateroutput$E004 = waterInput[4, 3]

  wateroutput$E005 = waterInput[5, 3] + waterInput[6, 3]*nodesInput[50, 3]

  wateroutput$E006 = waterInput[6, 3] + waterInput[7, 3]*nodesInput[297, 3]

  wateroutput$E007 = waterInput[7, 3] + waterInput[9, 3]*nodesInput[192, 3] + waterInput[8, 3]*nodesInput[193, 3] + waterInput[10, 3]*nodesInput[194, 3]

  wateroutput$E008 = waterInput[8, 3]

  wateroutput$E009 = waterInput[9, 3]

  wateroutput$E010 = waterInput[10, 3]

  wateroutput$E011 = waterInput[11, 3] + waterInput[12, 3]*nodesInput[51, 3]

  wateroutput$E012 = waterInput[12, 3] + waterInput[13, 3]*nodesInput[195, 3]

  wateroutput$E013 = waterInput[13, 3] + waterInput[16, 3]*nodesInput[188, 3]

  wateroutput$E014 = waterInput[14, 3]

  wateroutput$E015 = waterInput[15, 3]

  wateroutput$E016 = waterInput[16, 3] + waterInput[15, 3]*nodesInput[187, 3]

  wateroutput$E017 = waterInput[17, 3] +  waterInput[18, 3]*nodesInput[230, 3]

  wateroutput$E018 = waterInput[18, 3]

  wateroutput$E019 = waterInput[19, 3] + waterInput[23, 3]*nodesInput[184, 3]

  wateroutput$E020 = waterInput[20, 3]

  wateroutput$E021 = waterInput[21, 3]

  wateroutput$E022 = waterInput[22, 3]

  wateroutput$E023 = waterInput[23, 3]

  wateroutput$E024 = waterInput[24, 3] + waterInput[28, 3]*nodesInput[183, 3]

  wateroutput$E025 = waterInput[25, 3]

  wateroutput$E026 = waterInput[26, 3]

  wateroutput$E027 = waterInput[27, 3]

  wateroutput$E028 = waterInput[28, 3]

  wateroutput$E029 = waterInput[29, 3] + waterInput[34, 3]*nodesInput[191, 3]

  wateroutput$E030 = waterInput[30, 3]

  wateroutput$E031 = waterInput[31, 3]

  wateroutput$E032 = waterInput[32, 3] + waterInput[33, 3]*nodesInput[180, 3]

  wateroutput$E033 = waterInput[33, 3]

  wateroutput$E034 = waterInput[34, 3]

  wateroutput$E035 = waterInput[35, 3] + waterInput[36, 3]*nodesInput[190, 3]

  wateroutput$E036 = waterInput[36, 3]

  wateroutput$E037 = waterInput[37, 3] + waterInput[38, 3]*nodesInput[223, 3]

  wateroutput$E038 = waterInput[38, 3]

  wateroutput$E039 = waterInput[39, 3] + waterInput[44, 3]*nodesInput[217, 3]

  wateroutput$E040 = waterInput[40, 3]

  wateroutput$E041 = waterInput[41, 3]

  wateroutput$E042 = waterInput[42, 3]

  wateroutput$E043 = waterInput[43, 3]

  wateroutput$E044 = waterInput[44 ,3] + waterInput[42, 3]*nodesInput[238, 3]

  wateroutput$E045 = waterInput[45 ,3] + waterInput[46, 3]*nodesInput[54, 3]

  wateroutput$E046 = waterInput[46 ,3] + waterInput[52, 3]*nodesInput[222, 3]

  wateroutput$E047 = waterInput[47 ,3]

  wateroutput$E048 = waterInput[48 ,3]

  wateroutput$E049 = waterInput[49 ,3]

  wateroutput$E050 = waterInput[50 ,3]

  wateroutput$E051 = waterInput[51 ,3]

  wateroutput$E052 = waterInput[52 ,3]

  wateroutput$E053 = waterInput[53 ,3]

  wateroutput$E054 = waterInput[54 ,3]

  wateroutput$E055 = waterInput[55 ,3] + waterInput[57, 3]*nodesInput[47, 3]

  wateroutput$E056 = waterInput[56 ,3] + waterInput[64, 3]*nodesInput[226, 3]

  wateroutput$E057 = waterInput[57 ,3] + waterInput[58, 3]*nodesInput[53, 3]

  wateroutput$E058 = waterInput[58 ,3] + waterInput[63, 3]*nodesInput[214, 3]

  wateroutput$E059 = waterInput[59 ,3]

  wateroutput$E060 = waterInput[60 ,3]

  wateroutput$E061 = waterInput[61 ,3]

  wateroutput$E062 = waterInput[62 ,3]

  wateroutput$E063 = waterInput[63 ,3]

  wateroutput$E064 = waterInput[64 ,3]

  wateroutput$E065 = waterInput[65 ,3] + waterInput[66, 3]*nodesInput[204, 3]

  wateroutput$E066 = waterInput[66 ,3]

  wateroutput$E067 = waterInput[67 ,3]

  wateroutput$E068 = waterInput[68 ,3]

  wateroutput$E069 = waterInput[69 ,3]

  wateroutput$E070 = waterInput[70 ,3]

  wateroutput$E071 = waterInput[71 ,3] + waterInput[77, 3]*nodesInput[200, 3]

  wateroutput$E072 = waterInput[72 ,3] + waterInput[75, 3]*nodesInput[206, 3]

  wateroutput$E073 = waterInput[73 ,3]

  wateroutput$E074 = waterInput[74 ,3]

  wateroutput$E075 = waterInput[75 ,3]

  wateroutput$E076 = waterInput[76 ,3]

  wateroutput$E077 = waterInput[77 ,3]

  wateroutput$E078 = waterInput[78 ,3]

  wateroutput$E079 = waterInput[79 ,3] + waterInput[80, 3]*nodesInput[284, 3]

  wateroutput$E080 = waterInput[80 ,3]

  wateroutput$E081 = waterInput[81 ,3] + waterInput[82, 3]*nodesInput[113, 3]

  wateroutput$E082 = waterInput[82 ,3] + waterInput[83, 3]*nodesInput[280, 3]

  wateroutput$E083 = waterInput[83 ,3]

  wateroutput$E084 = waterInput[84 ,3]

  wateroutput$E085 = waterInput[85 ,3] + waterInput[89, 3]*nodesInput[197, 3]

  wateroutput$E086 = waterInput[86 ,3] + waterInput[88, 3]*nodesInput[251, 3]

  wateroutput$E087 = waterInput[87 ,3]

  wateroutput$E088 = waterInput[88 ,3]

  wateroutput$E089 = waterInput[89 ,3]

  wateroutput$E090 = waterInput[90 ,3] + waterInput[91, 3]*nodesInput[119, 3]

  wateroutput$E091 = waterInput[91 ,3] + waterInput[92, 3]*nodesInput[279, 3]

  wateroutput$E092 = waterInput[92 ,3] + waterInput[93, 3]*nodesInput[72, 3]

  wateroutput$E093  = waterInput[93 ,3]

  wateroutput$E094 = waterInput[94 ,3] + waterInput[95, 3]*nodesInput[109, 3]

  wateroutput$E095 = waterInput[95 ,3] + waterInput[96, 3]*nodesInput[256, 3]

  wateroutput$E096 = waterInput[96 ,3]

  wateroutput$E097 = waterInput[97 ,3]

  wateroutput$E098 = waterInput[98 ,3]

  wateroutput$E099 = waterInput[99 ,3] + waterInput[100, 3]*nodesInput[253, 3]

  wateroutput$E100 = waterInput[100,3]

  wateroutput$E101 = waterInput[101,3] + waterInput[102, 3]*nodesInput[73, 3]

  wateroutput$E102 = waterInput[102,3]

  wateroutput$E103 = waterInput[103,3] + waterInput[104, 3]*nodesInput[289, 3]

  wateroutput$E104 = waterInput[104,3]

  wateroutput$E105 = waterInput[105,3]

  wateroutput$E106 = waterInput[106,3]

  wateroutput$E107 = waterInput[107,3]

  wateroutput$E108 = waterInput[108,3] + waterInput[109, 3]*nodesInput[132, 3]

  wateroutput$E109 = waterInput[109,3] + waterInput[113, 3]*nodesInput[74, 3]

  wateroutput$E110 = waterInput[110,3]

  wateroutput$E111 = waterInput[111,3]

  wateroutput$E112 = waterInput[112,3] + waterInput[114, 3]*nodesInput[71, 3]

  wateroutput$E113 = waterInput[113,3] + waterInput[115, 3]*nodesInput[139, 3]

  wateroutput$E114 = waterInput[114,3]

  wateroutput$E115 = waterInput[115,3]

  wateroutput$E116 = waterInput[116,3] + waterInput[117, 3]*nodesInput[290, 3]

  wateroutput$E117 = waterInput[117,3]

  wateroutput$E118 = waterInput[118,3] + waterInput[119, 3]*nodesInput[130, 3]

  wateroutput$E119 = waterInput[119, 3] + waterInput[120, 3]*nodesInput[135, 3]

  wateroutput$E120 = waterInput[120, 3] + waterInput[122, 3]*nodesInput[116, 3]

  wateroutput$E121 = waterInput[121, 3] + waterInput[124, 3]*nodesInput[68, 3]

  wateroutput$E122 = waterInput[122, 3] + waterInput[123, 3]*nodesInput[67, 3]

  wateroutput$E123 = waterInput[123, 3]

  wateroutput$E124 = waterInput[124, 3] + waterInput[125, 3]*nodesInput[61, 3]

  wateroutput$E125 = waterInput[125, 3]

  wateroutput$E126 = waterInput[126, 3] + waterInput[127, 3]*nodesInput[128, 3]

  wateroutput$E127 = waterInput[127, 3]

  wateroutput$E128 = waterInput[128, 3] + waterInput[129, 3]*nodesInput[101, 3]

  wateroutput$E129 = waterInput[129, 3] + waterInput[130, 3]*nodesInput[136, 3]

  wateroutput$E130 = waterInput[130, 3] + waterInput[131, 3]*nodesInput[282, 3]

  wateroutput$E131 = waterInput[131, 3] + waterInput[132, 3]*nodesInput[60, 3]

  wateroutput$E132 = waterInput[132, 3]*nodesInput[60, c(8:11)]

  wateroutput$E133 = waterInput[133, 3] + waterInput[134, 3]*nodesInput[291, 3]

  wateroutput$E134 = waterInput[134, 3]

  wateroutput$E135 = waterInput[135, 3]

  wateroutput$E136 = waterInput[136, 3] + waterInput[138, 3]*nodesInput[293, 3]

  wateroutput$E137 = waterInput[137, 3] + waterInput[140, 3]*nodesInput[204, 3]

  wateroutput$E138 = waterInput[138, 3]  + waterInput[139, 3]*nodesInput[203, 3]

  wateroutput$E139 = waterInput[139, 3] + waterInput[141, 3]*nodesInput[167, 3]

  wateroutput$E140 = waterInput[140, 3] + waterInput[145, 3]*nodesInput[164, 3]

  wateroutput$E141 = waterInput[141, 3]

  wateroutput$E142 = waterInput[142, 3] + waterInput[148, 3]*nodesInput[83, 3]

  wateroutput$E143 = waterInput[143, 3] + waterInput[147, 3]*nodesInput[62, 3]

  wateroutput$E144 = waterInput[144, 3]

  wateroutput$E145 = waterInput[145, 3] + waterInput[146, 3]*nodesInput[56, 3]

  wateroutput$E146 = waterInput[146, 3]

  wateroutput$E147 = waterInput[147, 3]

  wateroutput$E148 = waterInput[148, 3]

  wateroutput$E149 = waterInput[149, 3]

  wateroutput$E150 = waterInput[150, 3] + waterInput[151, 3]*nodesInput[292, 3]

  wateroutput$E151 = waterInput[151, 3]

  wateroutput$E152 = waterInput[152, 3] + waterInput[153, 3]*nodesInput[212, 3]

  wateroutput$E153 =waterInput[153, 3]

  wateroutput$E154 = waterInput[154, 3]

  wateroutput$E155 = waterInput[155, 3] + waterInput[156, 3]*nodesInput[88, 3]

  wateroutput$E156 = waterInput[156, 3]

  wateroutput$E157 = waterInput[157, 3] + waterInput[158, 3]*nodesInput[202, 3]

  wateroutput$E158 = waterInput[158, 3] + waterInput[159, 3]*nodesInput[94, 3]

  wateroutput$E159 = waterInput[159, 3] + waterInput[160, 3]*nodesInput[159, 3]

  wateroutput$E160 = waterInput[160, 3] + waterInput[161, 3]*nodesInput[171, 3]

  wateroutput$E161 = waterInput[161, 3] + waterInput[167, 3]*nodesInput[78, 3]

  wateroutput$E162 = waterInput[162, 3] + waterInput[163, 3]*nodesInput[172, 3]

  wateroutput$E163 = waterInput[163, 3] + waterInput[166, 3]*nodesInput[77, 3]

  wateroutput$E164 =  waterInput[164, 3] + waterInput[165, 3]*nodesInput[97, 3]

  wateroutput$E165 = waterInput[165, 3]

  wateroutput$E166 = waterInput[166, 3]

  wateroutput$E167 = waterInput[167, 3]

  wateroutput$E168 = waterInput[168, 3] + waterInput[169, 3]*nodesInput[154, 3]

  wateroutput$E169 = waterInput[169, 3] + waterInput[170, 3]*nodesInput[69, 3]

  wateroutput$E170 = waterInput[170, 3] + waterInput[171, 3]*nodesInput[95, 3]

  # wateroutput$E171=  list()  NO OUTLwateroutput$ET NODwateroutput$E IN GIS FILwateroutput$ES
  # wateroutput$E171 = waterInput[171, 3]*nodesInput[95, c(8:11)]*waterInput[170, 3]*nodesInput[69, c(8:11)]*waterInput[169, 3]*nodesInput[154, c(8:11)]*waterInput[168, 3]*nodesInput[18, c(8:11)]+ waterInput[172, 3]*nodesInput[177, 3]+ waterInput[173, 3]*nodesInput[*NODwateroutput$E*, 3]

  wateroutput$E172 = waterInput[172, 3] + waterInput[174, 3]*nodesInput[89, 3]

  # wateroutput$E173 =  list() No Outlet node in GIS file
  # wateroutput$E173 = waterInput[173, 3]*nodesInput[0, c(8:11)]*waterInput[171, 3]*nodesInput[95, c(8:11)]*waterInput[170, 3]*nodesInput[69, c(8:11)]*waterInput[169, 3]*nodesInput[154, c(8:11)]*waterInput[168, 3]*nodesInput[18, c(8:11)]+ waterInput[175, 3]*nodesInput[92, 3]

  wateroutput$E174 = waterInput[174, 3]

  wateroutput$E175 = waterInput[175, 3]
  # Removed node 173 from wateroutput$E175 - no stream outlet node in GIS file : * waterInput[173, 3]*nodesInput[0, c(8:11)]

  wateroutput$E176 = waterInput[176, 3] + waterInput[177, 3]*nodesInput[86, 3]

  wateroutput$E177 = waterInput[177, 3] + waterInput[178, 3]*nodesInput[178, 3]

  wateroutput$E178 = waterInput[178, 3]

  wateroutput$E179 = waterInput[179, 3] + waterInput[180, 3]*nodesInput[134, 3]

  wateroutput$E180 = waterInput[180, 3] + waterInput[181, 3]*nodesInput[55, 3]

  wateroutput$E181 = waterInput[181, 3] + waterInput[184, 3]*nodesInput[261, 3]

  wateroutput$E182 = waterInput[182, 3]

  wateroutput$E183 = waterInput[183, 3] + waterInput[182, 3]*nodesInput[262, 3]

  wateroutput$E184 = waterInput[184, 3]

  wateroutput$E185 = waterInput[185, 3] + waterInput[188, 3]*nodesInput[93, 3]

  wateroutput$E186 = waterInput[186, 3]

  wateroutput$E187 = waterInput[187, 3] + waterInput[186, 3]*nodesInput[256, 3]

  wateroutput$E188 = waterInput[188, 3]

  wateroutput$E189 = waterInput[189, 3] + waterInput[194, 3]*nodesInput[123, 3]

  wateroutput$E190 = waterInput[190, 3] + waterInput[191, 3]*nodesInput[299, 3]

  wateroutput$E191 = waterInput[191, 3]

  wateroutput$E192 = waterInput[192, 3] + waterInput[193, 3]*nodesInput[265, 3]

  wateroutput$E193 = waterInput[193, 3]

  wateroutput$E194 = waterInput[194, 3] + waterInput[196, 3]*nodesInput[296, 3]

  wateroutput$E195 = waterInput[195, 3]

  wateroutput$E196 = waterInput[196, 3]

  wateroutput$E197 = waterInput[197, 3]

  wateroutput$E198 = waterInput[198, 3] + waterInput[199, 3]*nodesInput[264, 3]

  wateroutput$E199 = waterInput[199, 3]

  wateroutput$E200 = waterInput[200, 3] + waterInput[201, 3]*nodesInput[168, 3]

  wateroutput$E201 = waterInput[201, 3] + waterInput[202, 3]*nodesInput[98, 3]

  wateroutput$E202 = waterInput[202, 3]

  wateroutput$E203 = waterInput[203, 3]

  wateroutput$E204 = waterInput[204, 3] + waterInput[205, 3]*nodesInput[122, 3]

  wateroutput$E205 = waterInput[205, 3] + waterInput[208, 3]*nodesInput[142, 3]

  # wateroutput$E206 =  list()
  # wateroutput$E206 = MISSING FROM GIS FILwateroutput$ES

  wateroutput$E207 = waterInput[207, 3]

  wateroutput$E208 = waterInput[208, 3] + waterInput[209, 3]*nodesInput[108, 3]

  wateroutput$E209 = waterInput[209, 3] + waterInput[210, 3]*nodesInput[266, 3]

  wateroutput$E210 = waterInput[210, 3]

  # wateroutput$E211 =  list() BASIN 211 MISSING OUTLwateroutput$ET NODwateroutput$E
  # wateroutput$E211 = waterInput[211, 3]*nodesInput[0, c(8:11)] + waterInput[212, 3]*nodesInput[120, 3]
  # 212-214 ALL Rwateroutput$ELY ON OUTLwateroutput$ET OF BASIN 211
  # wateroutput$E212 =  list()
  # wateroutput$E212 = waterInput[212, 3]*nodesInput[120, c(8:11)]*waterInput[211, 3]*nodesInput[0, c(8:11)]+ waterInput[213, 3]*nodesInput[175, 3]+ waterInput[214, 3]*nodesInput[174, 3]
  #
  # wateroutput$E213 =  list()
  # wateroutput$E213 = waterInput[213, 3]*nodesInput[175, c(8:11)]*waterInput[212, 3]*nodesInput[120, c(8:11)]*waterInput[211, 3]*nodesInput[0, c(8:11)]
  #
  # wateroutput$E214 =  list()
  # wateroutput$E214 = waterInput[214, 3]*nodesInput[174, c(8:11)]*waterInput[212, 3]*nodesInput[120, c(8:11)]*waterInput[211, 3]*nodesInput[0, c(8:11)]

  wateroutput$E215 = waterInput[215, 3]

  wateroutput$E216 = waterInput[216, 3] + waterInput[217, 3]*nodesInput[121, 3]

  wateroutput$E217 = waterInput[217, 3] + waterInput[218, 3]*nodesInput[300, 3]

  wateroutput$E218 = waterInput[218, 3]

  wateroutput$E219 = waterInput[219, 3]

  wateroutput$E220 = waterInput[220, 3] + waterInput[221, 3]*nodesInput[140, 3]

  wateroutput$E221 = waterInput[221, 3] + waterInput[224, 3]*nodesInput[158, 3]

  wateroutput$E222 = waterInput[222, 3] + waterInput[228, 3]*nodesInput[59, 3]

  wateroutput$E223 = waterInput[223, 3]

  wateroutput$E224 = waterInput[224, 3] + waterInput[225, 3]*nodesInput[148, 3]

  wateroutput$E225 = waterInput[225, 3] + waterInput[227, 3]*nodesInput[57, 3]

  wateroutput$E226 = waterInput[226, 3] + waterInput[223, 3]*nodesInput[268, 3]

  wateroutput$E227 = waterInput[227, 3]

  wateroutput$E228 = waterInput[228, 3]

  wateroutput$E229 = waterInput[229, 3]

  wateroutput$E230 = waterInput[230, 3] + waterInput[231, 3]*nodesInput[96, 3]

  wateroutput$E231 = waterInput[231, 3] + waterInput[234, 3]*nodesInput[117, 3]

  wateroutput$E232 = waterInput[232, 3]

  wateroutput$E233 = waterInput[233, 3]

  wateroutput$E234 = waterInput[234, 3] + waterInput[235, 3]*nodesInput[150, 3]

  wateroutput$E235 = waterInput[235, 3]

  wateroutput$E236 = waterInput[236, 3] + waterInput[238, 3]*nodesInput[63, 3]

  wateroutput$E237 = waterInput[237, 3]

  wateroutput$E238 = waterInput[238, 3]

  wateroutput$E239 = waterInput[239, 3] + waterInput[240, 3]*nodesInput[46, 3]

  wateroutput$E240 = waterInput[240, 3] + waterInput[242, 3]*nodesInput[87, 3]

  wateroutput$E241 = waterInput[241, 3]

  wateroutput$E242 = waterInput[242, 3] + waterInput[241, 3]*nodesInput[303, 3]

  wateroutput$E243 = waterInput[243, 3]

  wateroutput$E244 = waterInput[244, 3] + waterInput[261, 3]*nodesInput[271, 3]

  wateroutput$E245 = waterInput[245, 3]

  wateroutput$E246 = waterInput[246, 3] + waterInput[258, 3]*nodesInput[209, 3]

  wateroutput$E247 = waterInput[247, 3] + waterInput[249, 3]*nodesInput[295, 3]

  wateroutput$E248 = waterInput[248, 3] + waterInput[250, 3]*nodesInput[301, 3]

  wateroutput$E249 = waterInput[249, 3]

  wateroutput$E250 = waterInput[250, 3]

  wateroutput$E251 = waterInput[251, 3]

  wateroutput$E252 = waterInput[252, 3] + waterInput[253, 3]*nodesInput[125, 3]

  wateroutput$E253 = waterInput[253, 3] + waterInput[256, 3]*nodesInput[156, 3]

  wateroutput$E254 = waterInput[254, 3]

  wateroutput$E255 = waterInput[255, 3]

  wateroutput$E256 = waterInput[256, 3] + waterInput[257, 3]*nodesInput[160, 3]

  wateroutput$E257 = waterInput[257, 3] + waterInput[267, 3]*nodesInput[64, 3]

  wateroutput$E258 = waterInput[258, 3] + waterInput[259, 3]*nodesInput[153, 3]

  wateroutput$E259 = waterInput[259, 3]

  wateroutput$E260 = waterInput[260, 3] + waterInput[266, 3]*nodesInput[274, 3]

  wateroutput$E261 = waterInput[261, 3]

  wateroutput$E262 = waterInput[262, 3] + waterInput[265, 3]*nodesInput[272, 3]

  wateroutput$E263 = waterInput[263, 3] + waterInput[270, 3]*nodesInput[275, 3]

  wateroutput$E264 = waterInput[264, 3]

  wateroutput$E265 = waterInput[265, 3]

  wateroutput$E266 = waterInput[266, 3]

  wateroutput$E267 = waterInput[267, 3]

  wateroutput$E268 = waterInput[268, 3] + waterInput[269, 3]*nodesInput[70, 3]

  wateroutput$E269 = waterInput[269, 3]

  wateroutput$E270 = waterInput[270, 3]

  # wateroutput$E271

  wateroutput$E272 = waterInput[272, 3] + waterInput[273, 3]*nodesInput[82, 3]

  wateroutput$E273 = waterInput[273, 3]

  wateroutput$E274 = waterInput[274, 3] + waterInput[275, 3]*nodesInput[79, 3]

  wateroutput$E275 = waterInput[275, 3] + waterInput[277, 3]*nodesInput[80, 3]

  wateroutput$E276 = waterInput[276, 3]

  wateroutput$E277 = waterInput[277, 3]

  wateroutput$E278 = waterInput[278, 3] + waterInput[280, 3]*nodesInput[104, 3]

  wateroutput$E279 =  waterInput[279, 3]

  wateroutput$E280 = waterInput[280, 3]

  # wateroutput$E281 =  list()
  # wateroutput$E281 = waterInput[281, 3]*nodesInput[104, c(8:11)]

  wateroutput$E282 = waterInput[282, 3] + waterInput[286, 3]*nodesInput[169, 3]

  wateroutput$E283 = waterInput[283, 3]

  wateroutput$E284 = waterInput[284, 3]

  wateroutput$E285 = waterInput[285, 3]

  wateroutput$E286 = waterInput[286, 3]

  wateroutput$E287 = waterInput[287, 3]

  wateroutput$E288 = waterInput[288, 3] + waterInput[289, 3]*nodesInput[162, 3]

  wateroutput$E289 = waterInput[289, 3] + waterInput[290, 3]*nodesInput[163, 3]

  wateroutput$E290 = waterInput[290, 3] + waterInput[292, 3]*nodesInput[189, 3]

  wateroutput$E291 = waterInput[291, 3] + waterInput[293, 3]*nodesInput[219, 3]

  wateroutput$E292 = waterInput[292, 3]

  wateroutput$E293 = waterInput[293, 3]

  wateroutput$E294 = waterInput[294, 3] + waterInput[295, 3]*nodesInput[308, 3]

  wateroutput$E295 = waterInput[295, 3]

  wateroutput$E296 = waterInput[296, 3] + waterInput[297, 3]*nodesInput[84, 3]

  wateroutput$E297 = waterInput[297, 3] + waterInput[298, 3]*nodesInput[219, 3]

  wateroutput$E298 = waterInput[298, 3]

  wateroutput$E299 = waterInput[299, 3] + waterInput[301, 3]*nodesInput[85, 3]

  # wateroutput$E300 =  list() Basin not included in GIS data
  # wateroutput$E300 = waterInput[300, 3]*nodesInput[307, c(8:11)]*waterInput[299, 3]*nodesInput[2, c(8:11)]

  wateroutput$E301 = waterInput[301, 3] + waterInput[302, 3]*nodesInput[99, 3]

  wateroutput$E302 = waterInput[302, 3] + waterInput[303, 3]*nodesInput[129, 3]

  wateroutput$E303 = waterInput[303, 3] + waterInput[304, 3]*nodesInput[145, 3]

  wateroutput$E304 = waterInput[304, 3]

  wateroutput$E305 = waterInput[305, 3]

  wateroutput$E306 = waterInput[306, 3] + waterInput[307, 3]*nodesInput[277, 3]

  wateroutput$E307 = waterInput[305, 3]

  wateroutput$E308 = waterInput[306, 3]*nodesInput[65, c(8:11)]*waterInput[297, 3]*nodesInput[84, c(8:11)]*waterInput[296, 3]*nodesInput[3, c(8:11)]

  # -------------------------------------------------------------------
  # Save output in dataframe
  output.df = as.data.frame(wateroutput)
  # scenario = data.frame(sapply(ls(pattern="^wateroutput$E[0-9]+$"),get))

  return(output.df)
}

