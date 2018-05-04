#' @export
# Function to calculate amount of water shunted to diversion

# convert all nodeInputs[..., c(8:11)] to nodeInputs[..., 2]
diversion.fun = function(nodesInput, waterInput){
  diverted = list()

  diverted$E001 = waterInput[1, 3]*nodesInput[39, 2] + waterInput[2, 3]*nodesInput[49, 3]

  diverted$E002 = waterInput[2, 3]*nodesInput[49, 2] + waterInput[3, 3]*nodesInput[228, 3] + waterInput[4, 3]*nodesInput[196, 3]

  diverted$E003 = waterInput[3, 3]*nodesInput[228, 2]

  diverted$E004 = waterInput[4, 3]*nodesInput[196, 2]

  diverted$E005 = waterInput[5, 3]*nodesInput[42, 2] + waterInput[6, 3]*nodesInput[50, 3]

  # nodesInput[50, ...] = spring, so cfs is added, not siphoned off
  diverted$E006 = waterInput[6, 3]+nodesInput[50, 2] + waterInput[7, 3]*nodesInput[229, 3]

  diverted$E007 = waterInput[7, 3]*nodesInput[229, 2] + waterInput[9, 3]*nodesInput[192, 3] + waterInput[8, 3]*nodesInput[193, 3] + waterInput[10, 3]*nodesInput[194, 3]

  diverted$E008 = waterInput[8, 3]*nodesInput[193, 2]

  diverted$E009 = waterInput[9, 3]*nodesInput[192, 2]

  diverted$E010 = waterInput[10, 3]*nodesInput[194, 2]

  diverted$E011 = waterInput[11, 3]*nodesInput[44, 2] + waterInput[12, 3]*nodesInput[51, 3]

  # nodesInput[51, ...] = spring, so cfs is added, not siphoned off
  diverted$E012 = waterInput[12, 3]+nodesInput[51, 2] + waterInput[13, 3]*nodesInput[195, 3] + waterInput[14, 3]*nodesInput[186, 3]

  diverted$E013 = waterInput[13, 3]*nodesInput[195, 2] + waterInput[16, 3]*nodesInput[188, 3]

  diverted$E014 = waterInput[14, 3]*nodesInput[186, 2]

  diverted$E015 = waterInput[15, 3]*nodesInput[187, 2]

  diverted$E016 = waterInput[16, 3]*nodesInput[188, 2] + waterInput[15, 3]*nodesInput[187, 3]

  diverted$E017 = waterInput[17, 3]*nodesInput[43, 2] +  waterInput[18, 3]*nodesInput[230, 3]

  diverted$E018 = waterInput[18, 3]*nodesInput[230, 2]

  diverted$E019 = waterInput[19, 3]*nodesInput[41, 3] + waterInput[23, 3]*nodesInput[184, 3]

  diverted$E020 = waterInput[20, 3]*nodesInput[232, 2]

  diverted$E021 = waterInput[21, 3]*nodesInput[231, 2]

  diverted$E022 = waterInput[22, 3]*nodesInput[185, 2]

  diverted$E023 = waterInput[23, 3]*nodesInput[184, 2]

  diverted$E024 = waterInput[24, 3]*nodesInput[40, 2] + waterInput[28, 3]*nodesInput[183, 3]

  diverted$E025 = waterInput[25, 3]*nodesInput[233, 2]

  diverted$E026 = waterInput[26, 3]*nodesInput[234, 2]

  diverted$E027 = waterInput[27, 3]*nodesInput[182, 2]

  diverted$E028 = waterInput[28, 3]*nodesInput[183, 2]

  diverted$E029 = waterInput[29, 3]*nodesInput[38, 2] + waterInput[34, 3]*nodesInput[191, 3]

  diverted$E030 = waterInput[30, 3]*nodesInput[237, 2]

  diverted$E031 = waterInput[31, 3]*nodesInput[236, 2]

  diverted$E032 = waterInput[32, 3]*nodesInput[235, 2]  + waterInput[33, 3]*nodesInput[180, 3]

  diverted$E033 = waterInput[33, 3]*nodesInput[180, 2]

  diverted$E034 = waterInput[34, 3]*nodesInput[191, 2]

  diverted$E035 = waterInput[35, 3]*nodesInput[37, 2] + waterInput[36, 3]*nodesInput[190, 3]

  diverted$E036 = waterInput[36, 3]*nodesInput[190, 2]

  diverted$E037 = waterInput[37, 3]*nodesInput[36, 2] + waterInput[38, 3]*nodesInput[223, 3]

  diverted$E038 = waterInput[38, 3]*nodesInput[223, 2]

  diverted$E039 = waterInput[39, 3]*nodesInput[35, 2] + waterInput[44, 3]*nodesInput[217, 3]

  diverted$E040 = waterInput[40, 3]*nodesInput[218, 2]

  diverted$E041 = waterInput[41, 3]*nodesInput[220, 2]

  diverted$E042 = waterInput[42, 3]*nodesInput[238, 2]

  diverted$E043 = waterInput[43, 3]*nodesInput[227, 2]

  diverted$E044 = waterInput[44 ,3]*nodesInput[217, 2]  + waterInput[42, 3]*nodesInput[238, 3]

  diverted$E045 = waterInput[45 ,3]*nodesInput[34, 2] + waterInput[46, 3]*nodesInput[54, 3]

  diverted$E046 = waterInput[46 ,3]*nodesInput[54, 2] + waterInput[54, 3]*nodesInput[224, 3] + waterInput[53, 3]*nodesInput[225,3] + waterInput[51, 3]*nodesInput[243, 3] + waterInput[50, 3]*nodesInput[242, 3] + waterInput[49, 3]*nodesInput[241, 3] + waterInput[47, 3]*nodesInput[240, 3] + waterInput[48, 3]*nodesInput[239, 3] + waterInput[52, 3]*nodesInput[222, 3]

  diverted$E047 = waterInput[47 ,3]*nodesInput[240, 2]

  diverted$E048 = waterInput[48 ,3]*nodesInput[239, 2]

  diverted$E049 = waterInput[49 ,3]*nodesInput[241, 2]

  diverted$E050 = waterInput[50 ,3]*nodesInput[242, 2]

  diverted$E051 = waterInput[51 ,3]*nodesInput[243, 2]

  diverted$E052 = waterInput[52 ,3]*nodesInput[222, 2]

  diverted$E053 = waterInput[53 ,3]*nodesInput[225, 2]

  diverted$E054 = waterInput[54 ,3]*nodesInput[224, 2]

  diverted$E055 = waterInput[55 ,3]*nodesInput[32, 2] + waterInput[57, 3]*nodesInput[47, 3]

  diverted$E056 = waterInput[56 ,3]*nodesInput[52, 2] + waterInput[64, 3]*nodesInput[226, 3]

  diverted$E057 = waterInput[57 ,3]*nodesInput[47, 2] + waterInput[58, 3]*nodesInput[53, 3]

  diverted$E058 = waterInput[58 ,3]*nodesInput[53, 2] + waterInput[63, 3]*nodesInput[214, 3] + waterInput[61, 3]*nodesInput[215, 3] + waterInput[60, 3]*nodesInput[208, 3] + waterInput[59, 3]*nodesInput[207, 3] + waterInput[62, 3]*nodesInput[205, 3]

  diverted$E059 = waterInput[59 ,3]*nodesInput[207, 2]

  diverted$E060 = waterInput[60 ,3]*nodesInput[208, 2]

  diverted$E061 = waterInput[61 ,3]*nodesInput[215, 2]

  diverted$E062 = waterInput[62 ,3]*nodesInput[205, 2]

  diverted$E063 = waterInput[63 ,3]*nodesInput[214, 2]

  diverted$E064 = waterInput[64 ,3]*nodesInput[226, 2]

  diverted$E065 = waterInput[65 ,3]*nodesInput[31, 2] + waterInput[66, 3]*nodesInput[204, 3]

  diverted$E066 = waterInput[66 ,3]*nodesInput[244, 2]

  diverted$E067 = waterInput[67 ,3]*nodesInput[245, 2]

  diverted$E068 = waterInput[68 ,3]*nodesInput[247, 2]

  diverted$E069 = waterInput[69 ,3]*nodesInput[246, 2]

  diverted$E070 = waterInput[70 ,3]*nodesInput[221, 2]

  diverted$E071 = waterInput[71 ,3]*nodesInput[30, 2] + waterInput[77, 3]*nodesInput[200, 3]

  diverted$E072 = waterInput[72 ,3]*nodesInput[281, 2] + waterInput[75, 3]*nodesInput[206, 3]

  diverted$E073 = waterInput[73 ,3]*nodesInput[249, 2]

  diverted$E074 = waterInput[74 ,3]*nodesInput[198, 2]

  diverted$E075 = waterInput[75 ,3]*nodesInput[206, 2]

  diverted$E076 = waterInput[76 ,3]*nodesInput[199, 2]

  diverted$E077 = waterInput[77 ,3]*nodesInput[200, 2]

  diverted$E078 = waterInput[78 ,3]*nodesInput[181, 2]

  diverted$E079 = waterInput[79 ,3]*nodesInput[29, 2] + waterInput[80, 3]*nodesInput[284, 3]

  diverted$E080 = waterInput[80 ,3]*nodesInput[284, 2]

  diverted$E081 = waterInput[81 ,3]*nodesInput[285, 2] + waterInput[82, 3]*nodesInput[113, 3]

  diverted$E082 = waterInput[82 ,3]*nodesInput[113, 2] + waterInput[83, 3]*nodesInput[280, 3] + waterInput[84, 3]*nodesInput[278, 3] + waterInput[85, 3]*nodesInput[111, 3]

  diverted$E083 = waterInput[83 ,3]*nodesInput[280, 2]

  diverted$E084 = waterInput[84 ,3]*nodesInput[278, 2]

  diverted$E085 = waterInput[85 ,3]*nodesInput[111, 2] + waterInput[89, 3]*nodesInput[197, 3] + waterInput[86, 3]*nodesInput[252, 3] + waterInput[87, 3]*nodesInput[250, 3]

  diverted$E086 = waterInput[86 ,3]*nodesInput[252, 2] + waterInput[88, 3]*nodesInput[251, 3]

  diverted$E087 = waterInput[87 ,3]*nodesInput[250, 2]

  diverted$E088 = waterInput[88 ,3]*nodesInput[251, 2]

  diverted$E089 = waterInput[89 ,3]*nodesInput[197, 2]

  diverted$E090 = waterInput[90 ,3]*nodesInput[27, 2] + waterInput[91, 3]*nodesInput[119, 3]

  diverted$E091 = waterInput[91 ,3]*nodesInput[119, 2] + waterInput[92, 3]*nodesInput[279, 3]

  diverted$E092 = waterInput[92 ,3]*nodesInput[279, 2] + waterInput[93, 3]*nodesInput[72, 3]

  diverted$E093  = waterInput[93 ,3]*nodesInput[72, 2]

  diverted$E094 = waterInput[94 ,3]*nodesInput[28, 2] + waterInput[95, 3]*nodesInput[109, 3]

  diverted$E095 = waterInput[95 ,3]*nodesInput[109, 2] + waterInput[96, 3]*nodesInput[256, 3] + waterInput[97, 3]*nodesInput[255, 3] + waterInput[98, 3]*nodesInput[254, 3]

  diverted$E096 = waterInput[96 ,3]*nodesInput[256, 2]

  diverted$E097 = waterInput[97 ,3]*nodesInput[255, 2]

  diverted$E098 = waterInput[98 ,3]*nodesInput[254, 2]

  diverted$E099 = waterInput[99 ,3]*nodesInput[100, 2] + waterInput[100, 3]*nodesInput[253, 3] + waterInput[101, 3]*nodesInput[201, 3]

  diverted$E100 = waterInput[100,3]*nodesInput[253, 2]

  diverted$E101 = waterInput[101,3]*nodesInput[201, 2] + waterInput[102, 3]*nodesInput[73, 3]

  diverted$E102 = waterInput[102,3]*nodesInput[73, 2]

  diverted$E103 = waterInput[103,3]*nodesInput[26, 2] + waterInput[104, 3]*nodesInput[289, 3]

  diverted$E104 = waterInput[104,3]*nodesInput[289, 2]

  diverted$E105 = waterInput[105,3]*nodesInput[286, 2]

  diverted$E106 = waterInput[106,3]*nodesInput[288, 2]

  diverted$E107 = waterInput[107,3]*nodesInput[112, 2]

  diverted$E108 = waterInput[108,3]*nodesInput[25, 2] + waterInput[109, 3]*nodesInput[132, 3]

  diverted$E109 = waterInput[109,3]*nodesInput[132, 2] + waterInput[113, 3]*nodesInput[74, 3] + waterInput[112, 3]*nodesInput[114, 3] + waterInput[111, 3]*nodesInput[258, 3] + waterInput[110,3]*nodesInput[257, 3]

  diverted$E110 = waterInput[110,3]*nodesInput[257, 2]

  diverted$E111 = waterInput[111,3]*nodesInput[258, 2]

  diverted$E112 = waterInput[112,3]*nodesInput[114, 2] + waterInput[114, 3]*nodesInput[71, 3]

  diverted$E113 = waterInput[113,3]*nodesInput[74, 2] + waterInput[115, 3]*nodesInput[139, 3]

  diverted$E114 = waterInput[114,3]*nodesInput[71, 2]

  diverted$E115 = waterInput[115,3]*nodesInput[139, 2]

  diverted$E116 = waterInput[116,3]*nodesInput[24, 2] + waterInput[117, 3]*nodesInput[290, 3]

  diverted$E117 = waterInput[117,3]*nodesInput[290, 2]

  diverted$E118 = waterInput[118,3]*nodesInput[110, 2] + waterInput[119, 3]*nodesInput[130, 3]

  diverted$E119 = waterInput[119, 3]*nodesInput[130, 2] + waterInput[120, 3]*nodesInput[135, 3]

  diverted$E120 = waterInput[120, 3]*nodesInput[135, 2] + waterInput[122, 3]*nodesInput[116, 3] + waterInput[121, 3]*nodesInput[115, 3]

  diverted$E121 = waterInput[121, 3]*nodesInput[115, 2] + waterInput[124, 3]*nodesInput[68, 3]

  diverted$E122 = waterInput[122, 3]*nodesInput[116, 2] + waterInput[123, 3]*nodesInput[67, 3]

  diverted$E123 = waterInput[123, 3]*nodesInput[67, 2]

  diverted$E124 = waterInput[124, 3]*nodesInput[68, 2] + waterInput[125, 3]*nodesInput[61, 3]

  diverted$E125 = waterInput[125, 3]*nodesInput[61, 2]

  diverted$E126 = waterInput[126, 3]*nodesInput[23, 2] + waterInput[127, 3]*nodesInput[128, 3]

  diverted$E127 = waterInput[127, 3]*nodesInput[128, 2]

  diverted$E128 = waterInput[128, 3]*nodesInput[22, 2] + waterInput[129, 3]*nodesInput[101, 3]

  diverted$E129 = waterInput[129, 3]*nodesInput[101, 2] + waterInput[130, 3]*nodesInput[136, 3]

  diverted$E130 = waterInput[130, 3]*nodesInput[136, 2] + waterInput[131, 3]*nodesInput[282, 3]

  diverted$E131 = waterInput[131, 3]*nodesInput[282, 2] + waterInput[132, 3]*nodesInput[60, 3]

  diverted$E132 = waterInput[132, 3]*nodesInput[60, 2]

  diverted$E133 = waterInput[133, 3]*nodesInput[21, 2] + waterInput[134, 3]*nodesInput[291, 3]

  diverted$E134 = waterInput[134, 3]*nodesInput[291, 2]

  diverted$E135 = waterInput[135, 3]*nodesInput[102, 2]

  diverted$E136 = waterInput[136, 3]*nodesInput[106, 2] + waterInput[138, 3]*nodesInput[293, 3]

  diverted$E137 = waterInput[137, 3]*nodesInput[103, 2] + waterInput[140, 3]*nodesInput[204, 3]

  diverted$E138 = waterInput[138, 3]*nodesInput[293, 2]  + waterInput[139, 3]*nodesInput[203, 3]

  diverted$E139 = waterInput[139, 3]*nodesInput[203, 2] + waterInput[141, 3]*nodesInput[167, 3] + waterInput[142, 3]*nodesInput[179, 3]

  diverted$E140 = waterInput[140, 3]*nodesInput[204, 2] + waterInput[145, 3]*nodesInput[164, 3]+ waterInput[144, 3]*nodesInput[165, 3]+ waterInput[143, 3]*nodesInput[166, 3]+ waterInput[149, 3]*nodesInput[81, 3]

  diverted$E141 = waterInput[141, 3]*nodesInput[167, 2]

  diverted$E142 = waterInput[142, 3]*nodesInput[179, 2] + waterInput[148, 3]*nodesInput[83, 3]

  diverted$E143 = waterInput[143, 3]*nodesInput[166, 2] + waterInput[147, 3]*nodesInput[62, 3]

  diverted$E144 = waterInput[144, 3]*nodesInput[165, 2]

  diverted$E145 = waterInput[145, 3]*nodesInput[164, 2] + waterInput[146, 3]*nodesInput[56, 3]

  diverted$E146 = waterInput[146, 3]*nodesInput[56, 2]

  diverted$E147 = waterInput[147, 3]*nodesInput[62, 2]

  diverted$E148 = waterInput[148, 3]*nodesInput[83, 2]

  diverted$E149 = waterInput[149, 3]*nodesInput[81, 2]

  diverted$E150 = waterInput[150, 3]*nodesInput[20, 2] + waterInput[151, 3]*nodesInput[292, 3]

  diverted$E151 = waterInput[151, 3]*nodesInput[292, 2]

  diverted$E152 = waterInput[152, 3]*nodesInput[107, 2] + waterInput[153, 3]*nodesInput[212, 3] + waterInput[154, 3]*nodesInput[294, 3] + waterInput[155, 3]*nodesInput[176, 3]

  diverted$E153 = waterInput[153, 3]*nodesInput[212, 2]

  diverted$E154 = waterInput[154, 3]*nodesInput[294, 2]

  diverted$E155 = waterInput[155, 3]*nodesInput[176, 2] + waterInput[156, 3]*nodesInput[88, 3]

  diverted$E156 = waterInput[156, 3]*nodesInput[88, 2]

  diverted$E157 = waterInput[157, 3]*nodesInput[19, 2] + waterInput[158, 3]*nodesInput[202, 3]

  diverted$E158 = waterInput[158, 3]*nodesInput[202, 2] + waterInput[159, 3]*nodesInput[94, 3]

  diverted$E159 = waterInput[159, 3]*nodesInput[94, 2] + waterInput[160, 3]*nodesInput[159, 3] + waterInput[162, 3]*nodesInput[75, 3]

  diverted$E160 = waterInput[160, 3]*nodesInput[159, 2] + waterInput[161, 3]*nodesInput[171, 3]

  diverted$E161 = waterInput[161, 3]*nodesInput[171, 2] + waterInput[167, 3]*nodesInput[78, 3]

  diverted$E162 = waterInput[162, 3]*nodesInput[75, 2] + waterInput[163, 3]*nodesInput[172, 3] + waterInput[164, 3]*nodesInput[173, 3]

  diverted$E163 = waterInput[163, 3]*nodesInput[172, 2] + waterInput[166, 3]*nodesInput[77, 3]

  diverted$E164 = waterInput[164, 3]*nodesInput[73, 2] + waterInput[165, 3]*nodesInput[97, 3]

  diverted$E165 = waterInput[165, 3]*nodesInput[97, 2]

  diverted$E166 = waterInput[166, 3]*nodesInput[77, 2]

  diverted$E167 = waterInput[167, 3]*nodesInput[78, 2]

  diverted$E168 = waterInput[168, 3]*nodesInput[18, 2] + waterInput[169, 3]*nodesInput[154, 3]

  diverted$E169 = waterInput[169, 3]*nodesInput[154, 2] + waterInput[170, 3]*nodesInput[69, 3]

  diverted$E170 = waterInput[170, 3]*nodesInput[69, 2] + waterInput[171, 3]*nodesInput[95, 3]

  diverted$E171 = waterInput[171, 3]*nodesInput[95, 2] + waterInput[172, 3]*nodesInput[177, 3]

  diverted$E172 = waterInput[172, 3]*nodesInput[177, 2] + waterInput[174, 3]*nodesInput[89, 3]

  # added new node 310 for basin 173
  diverted$E173 = waterInput[173, 3]*nodesInput[310, 3]

  diverted$E174 = waterInput[174, 3]*nodesInput[89, 2]

  diverted$E175 = waterInput[175, 3]*nodesInput[92, 2]

  diverted$E176 = waterInput[176, 3]*nodesInput[17, 2] + waterInput[177, 3]*nodesInput[86, 3]

  diverted$E177 = waterInput[177, 3]*nodesInput[86, 2] + waterInput[178, 3]*nodesInput[178, 3]

  diverted$E178 = waterInput[178, 3]*nodesInput[178, 2]

  diverted$E179 = waterInput[179, 3]*nodesInput[16, 2] + waterInput[180, 3]*nodesInput[134, 3]

  diverted$E180 = waterInput[180, 3]*nodesInput[134, 2] + waterInput[181, 3]*nodesInput[55, 3]

  diverted$E181 = waterInput[181, 3]*nodesInput[55, 2] + waterInput[184, 3]*nodesInput[261, 3] + waterInput[185, 3]*nodesInput[149, 3]

  diverted$E182 = waterInput[182, 3]*nodesInput[262, 2] + waterInput[183, 3]*nodesInput[263, 3]

  diverted$E183 = waterInput[183, 3]*nodesInput[263, 2]

  diverted$E184 = waterInput[184, 3]*nodesInput[261, 2]

  diverted$E185 = waterInput[185, 3]*nodesInput[149, 2] + waterInput[188, 3]*nodesInput[93, 3]

  diverted$E186 = waterInput[186, 3]*nodesInput[259, 2]

  diverted$E187 = waterInput[187, 3]*nodesInput[260, 2] + waterInput[186, 3]*nodesInput[259, 3]

  diverted$E188 = waterInput[188, 3]*nodesInput[93, 2]

  diverted$E189 = waterInput[189, 3]*nodesInput[15, 2] + waterInput[194, 3]*nodesInput[123, 3]

  diverted$E190 = waterInput[190, 3]*nodesInput[131, 2] + waterInput[191, 3]*nodesInput[299, 3] + waterInput[192, 3]*nodesInput[298, 3]

  diverted$E191 = waterInput[191, 3]*nodesInput[299, 2]

  diverted$E192 = waterInput[192, 3]*nodesInput[298, 2] + waterInput[193, 3]*nodesInput[265, 3]

  diverted$E193 = waterInput[193, 3]*nodesInput[265, 2]

  diverted$E194 = waterInput[194, 3]*nodesInput[123, 2] + waterInput[195, 3]*nodesInput[297, 3] + waterInput[196, 3]*nodesInput[296, 3] + waterInput[197, 3]*nodesInput[146, 3] + waterInput[198, 3]*nodesInput[66, 3] + waterInput[200, 3]*nodesInput[144, 3]

  diverted$E195 = waterInput[195, 3]*nodesInput[297, 2]

  diverted$E196 = waterInput[196, 3]*nodesInput[296, 2]

  diverted$E197 = waterInput[197, 3]*nodesInput[146, 2]

  diverted$E198 = waterInput[198, 3]*nodesInput[66, 2] + waterInput[199, 3]*nodesInput[264, 3]

  diverted$E199 = waterInput[199, 3]*nodesInput[264, 2]

  diverted$E200 = waterInput[200, 3]*nodesInput[144, 2] + waterInput[201, 3]*nodesInput[168, 3]

  diverted$E201 = waterInput[201, 3]*nodesInput[168, 2] + waterInput[202, 3]*nodesInput[98, 3]

  diverted$E202 = waterInput[202, 3]*nodesInput[98, 2]

  diverted$E203 = waterInput[203, 3]*nodesInput[14, 2]

  diverted$E204 = waterInput[204, 3]*nodesInput[13, 2] + waterInput[205, 3]*nodesInput[122, 3]

  diverted$E205 = waterInput[205, 3]*nodesInput[122, 2] + waterInput[208, 3]*nodesInput[142, 3] + waterInput[207, 3]*nodesInput[143, 3]

  # diverted$E206 = MISSING FROM GIS FILES

  diverted$E207 = waterInput[207, 3]*nodesInput[143, 2]

  diverted$E208 = waterInput[208, 3]*nodesInput[142, 2] + waterInput[209, 3]*nodesInput[108, 3]

  diverted$E209 = waterInput[209, 3]*nodesInput[108, 2] + waterInput[210, 3]*nodesInput[266, 3]

  diverted$E210 = waterInput[210, 3]*nodesInput[266, 2]

  # added new node 309 for basin 211
  diverted$E211 = waterInput[211, 3]*nodesInput[309, 2]

  diverted$E212 = waterInput[212, 3]*nodesInput[120, 2] + waterInput[214, 3]*nodesInput[174, 3] + waterInput[213, 3]*nodesInput[175, 3]

  diverted$E213 = waterInput[213, 3]*nodesInput[175, 2]

  diverted$E214 = waterInput[214, 3]*nodesInput[174, 2]

  diverted$E215 = waterInput[215, 3]*nodesInput[12, 2]

  diverted$E216 = waterInput[216, 3]*nodesInput[11, 2] + waterInput[217, 3]*nodesInput[121, 3]

  diverted$E217 = waterInput[217, 3]*nodesInput[121, 2] + waterInput[218, 3]*nodesInput[300, 3] + waterInput[219, 3]*nodesInput[141, 3] + waterInput[220, 3]*nodesInput[141, 3] + waterInput[222, 3]*nodesInput[147, 3]

  diverted$E218 = waterInput[218, 3]*nodesInput[300, 2]

  diverted$E219 = waterInput[219, 3]*nodesInput[141, 2]

  diverted$E220 = waterInput[220, 3]*nodesInput[141, 2] + waterInput[221, 3]*nodesInput[140, 3]

  diverted$E221 = waterInput[221, 3]*nodesInput[140, 2] + waterInput[224, 3]*nodesInput[158, 3]

  diverted$E222 = waterInput[222, 3]*nodesInput[147, 2] + waterInput[228, 3]*nodesInput[59, 3]

  diverted$E223 = waterInput[223, 3]*nodesInput[268, 2]

  diverted$E224 = waterInput[224, 3]*nodesInput[158, 2] + waterInput[225, 3]*nodesInput[148, 3]*nodesInput[267, 3] + waterInput[226, 3]*nodesInput[138, 3]

  diverted$E225 = waterInput[225, 3]*nodesInput[148, 2]*nodesInput[267, 2]  + waterInput[227, 3]*nodesInput[57, 3] + waterInput[227, 3]*nodesInput[57, 3]

  diverted$E226 = waterInput[226, 3]*nodesInput[138, 2] + waterInput[223, 3]*nodesInput[268, 3] + waterInput[229, 3]*nodesInput[58, 3]

  diverted$E227 = waterInput[227, 3]*nodesInput[57, 2]

  diverted$E228 = waterInput[228, 3]*nodesInput[59, 2]

  diverted$E229 = waterInput[229, 3]*nodesInput[58, 2]

  diverted$E230 = waterInput[230, 3]*nodesInput[7, 2] + waterInput[231, 3]*nodesInput[96, 3]

  diverted$E231 = waterInput[231, 3]*nodesInput[96, 2] + waterInput[234, 3]*nodesInput[117, 3] + waterInput[232, 3]*nodesInput[170, 3] + waterInput[233, 3]*nodesInput[161, 3]

  diverted$E232 = waterInput[232, 3]*nodesInput[170, 2]

  diverted$E233 = waterInput[233, 3]*nodesInput[161, 2]

  diverted$E234 = waterInput[234, 3]*nodesInput[117, 2] + waterInput[235, 3]*nodesInput[150, 3] + waterInput[236, 3]*nodesInput[151, 3] + waterInput[237, 3]*nodesInput[269, 3]

  diverted$E235 = waterInput[235, 3]*nodesInput[150, 2]

  diverted$E236 = waterInput[236, 3]*nodesInput[151, 2] + waterInput[238, 3]*nodesInput[63, 3]

  diverted$E237 = waterInput[237, 3]*nodesInput[269, 2]

  diverted$E238 = waterInput[238, 3]*nodesInput[63, 2]

  diverted$E239 = waterInput[239, 3]*nodesInput[10, 2] + waterInput[240, 3]*nodesInput[46, 3]

  diverted$E240 = waterInput[240, 3]*nodesInput[46, 2] + waterInput[242, 3]*nodesInput[87, 3] + waterInput[243, 3]*nodesInput[124, 3] + waterInput[242, 3]*nodesInput[87, 3] + waterInput[244, 3]*nodesInput[302, 3]

  diverted$E241 = waterInput[241, 3]*nodesInput[303, 2]

  diverted$E242 = waterInput[242, 3]*nodesInput[87, 2] + waterInput[241, 3]*nodesInput[303, 3] + waterInput[245, 3]*nodesInput[126, 3] + waterInput[246, 3]*nodesInput[105, 3]

  diverted$E243 = waterInput[243, 3]*nodesInput[124, 2]

  diverted$E244 = waterInput[244, 3]*nodesInput[302, 2]

  diverted$E245 = waterInput[245, 3]*nodesInput[126, 2] + waterInput[261, 3]*nodesInput[271, 3] + waterInput[262, 3]*nodesInput[276, 3] + waterInput[263, 3]*nodesInput[210, 3]

  diverted$E246 = waterInput[246, 3]*nodesInput[105, 2] + waterInput[258, 3]*nodesInput[209, 3] + waterInput[255, 3]*nodesInput[155, 3]

  diverted$E247 = waterInput[247, 3]*nodesInput[46, 2] + waterInput[248, 3]*nodesInput[90, 3] + waterInput[249, 3]*nodesInput[295, 3] + waterInput[251, 3]*nodesInput[118, 3]

  diverted$E248 = waterInput[248, 3]*nodesInput[90, 2] + waterInput[250, 3]*nodesInput[301, 3] + waterInput[252, 3]*nodesInput[91, 3]

  diverted$E249 = waterInput[249, 3]*nodesInput[295, 2]

  diverted$E250 = waterInput[250, 3]*nodesInput[301, 2]

  diverted$E251 = waterInput[251, 3]*nodesInput[118, 2]

  diverted$E252 = waterInput[252, 3]*nodesInput[91, 2] + waterInput[253, 3]*nodesInput[125, 3]

  diverted$E253 = waterInput[253, 3]*nodesInput[125, 2] + waterInput[256, 3]*nodesInput[156, 3] + waterInput[254, 3]*nodesInput[270, 3]

  diverted$E254 = waterInput[254, 3]*nodesInput[270, 2]

  diverted$E255 = waterInput[255, 3]*nodesInput[155, 2]

  diverted$E256 = waterInput[256, 3]*nodesInput[156, 2] + waterInput[257, 3]*nodesInput[160, 3]

  diverted$E257 = waterInput[257, 3]*nodesInput[160, 2] + waterInput[267, 3]*nodesInput[64, 3]

  diverted$E258 = waterInput[258, 3]*nodesInput[209, 2] + waterInput[259, 3]*nodesInput[153, 3]

  diverted$E259 = waterInput[259, 3]*nodesInput[153, 2]

  diverted$E260 = waterInput[260, 3]*nodesInput[211, 2] + waterInput[266, 3]*nodesInput[274, 3] + waterInput[264, 3]*nodesInput[273, 3] + waterInput[268, 3]*nodesInput[152, 3]

  diverted$E261 = waterInput[261, 3]*nodesInput[271, 2]

  diverted$E262 = waterInput[262, 3]*nodesInput[276, 2] + waterInput[265, 3]*nodesInput[272, 3]

  diverted$E263 = waterInput[263, 3]*nodesInput[210, 2] + waterInput[270, 3]*nodesInput[275, 3]

  diverted$E264 = waterInput[264, 3]*nodesInput[273, 2]

  diverted$E265 = waterInput[265, 3]*nodesInput[272, 2]

  diverted$E266 = waterInput[266, 3]*nodesInput[274, 2]

  diverted$E267 = waterInput[267, 3]*nodesInput[64, 2]

  diverted$E268 = waterInput[268, 3]*nodesInput[152, 2] + waterInput[269, 3]*nodesInput[70, 3]

  diverted$E269 = waterInput[269, 3]*nodesInput[70, 2]

  diverted$E270 = waterInput[270, 3]*nodesInput[275, 2]

  # diverted$E271 = 0

  diverted$E272 = waterInput[272, 3]*nodesInput[6, 2] + waterInput[273, 3]*nodesInput[82, 3]

  diverted$E273 = waterInput[273, 3]*nodesInput[82, 2]

  diverted$E274 = waterInput[274, 3]*nodesInput[9, 2] + waterInput[275, 3]*nodesInput[79, 3]

  diverted$E275 = waterInput[275, 3]*nodesInput[271, 2] + waterInput[277, 3]*nodesInput[80, 3] + waterInput[276, 3]*nodesInput[127, 3]

  diverted$E276 = waterInput[276, 3]*nodesInput[127, 2]

  diverted$E277 = waterInput[277, 3]*nodesInput[80, 2]

  # converted nodeID = 0 to nodeID = 311
  diverted$E278 = waterInput[278, 3]*nodesInput[311, 2] + waterInput[280, 3]*nodesInput[104, 3]

  diverted$E279 = waterInput[279, 3]*nodesInput[133, 2]

  diverted$E280 = waterInput[280, 3]*nodesInput[104, 2]

  # diverted$E281 = 0

  diverted$E282 = waterInput[282, 3]*nodesInput[1, 2] + waterInput[286, 3]*nodesInput[169, 3]

  diverted$E283 = waterInput[283, 3]*nodesInput[304, 2]

  diverted$E284 = waterInput[284, 3]*nodesInput[305, 2]

  diverted$E285 = waterInput[285, 3]*nodesInput[306, 2]

  diverted$E286 = waterInput[286, 3]*nodesInput[169, 2]

  diverted$E287 = waterInput[287, 3]*nodesInput[4, 2]

  diverted$E288 = waterInput[288, 3]*nodesInput[157, 2] + waterInput[289, 3]*nodesInput[162, 3]

  diverted$E289 = waterInput[289, 3]*nodesInput[162, 2] + waterInput[290, 3]*nodesInput[163, 3] + waterInput[291, 3]*nodesInput[163, 3]

  diverted$E290 = waterInput[290, 3]*nodesInput[163, 2] + waterInput[292, 3]*nodesInput[189, 3]

  diverted$E291 = waterInput[291, 3]*nodesInput[163, 2] + waterInput[293, 3]*nodesInput[219, 3]

  diverted$E292 = waterInput[292, 3]*nodesInput[189, 2]

  diverted$E293 = waterInput[293, 3]*nodesInput[219, 2]

  diverted$E294 = waterInput[294, 3]*nodesInput[5, 2] + waterInput[295, 3]*nodesInput[308, 3]

  diverted$E295 = waterInput[295, 3]*nodesInput[308, 2]

  diverted$E296 = waterInput[296, 3]*nodesInput[3, 2] + waterInput[297, 3]*nodesInput[84, 3]

  diverted$E297 = waterInput[297, 3]*nodesInput[84, 2] + waterInput[298, 3]*nodesInput[213, 3]

  diverted$E298 = waterInput[298, 3]*nodesInput[213, 2]

  diverted$E299 = waterInput[299, 3]*nodesInput[2, 2] + waterInput[301, 3]*nodesInput[85, 3]

  diverted$E300 = waterInput[300, 3]*nodesInput[307, 2]

  diverted$E301 = waterInput[301, 3]*nodesInput[307, 2] + waterInput[301, 3]*nodesInput[85, 2]

  diverted$E302 = waterInput[302, 3]*nodesInput[99, 2] + waterInput[303, 3]*nodesInput[129, 3]

  diverted$E303 = waterInput[303, 3]*nodesInput[129, 2] + waterInput[304, 3]*nodesInput[145, 3]

  diverted$E304 = waterInput[304, 3]*nodesInput[145, 2]

  diverted$E305 = waterInput[305, 3]*nodesInput[76, 2]

  diverted$E306 = waterInput[306, 3]*nodesInput[137, 2] + waterInput[307, 3]*nodesInput[277, 3] + waterInput[308, 3]*nodesInput[65, 3]

  diverted$E307 = waterInput[307, 3]*nodesInput[277, 2]

  diverted$E308 = waterInput[308, 3]*nodesInput[65, 2]

  # -------------------------------------------------------------------
  # Save output in dataframe
  output.df = as.data.frame(diverted)
  # scenario = data.frame(sapply(ls(pattern="^diverted$E[0-9]+$"),get))

  return(output.df)
}

