# ============================ #
# TDK - Layoff Event study
# Industry Competitors data cleaning
# ============================ #

#clear memory
rm(list = ls())

# set working directory
setwd("D:\\OneDrive\\Tanulás\\BCE\\TDK\\")
output <- "D:\\OneDrive\\Tanulás\\BCE\\TDK\\output\\"
dir_df <- "D:\\OneDrive\\Tanulás\\BCE\\TDK\\Databases\\"

# Data prep ==========================================

comp_assets <- c("Alibaba", "Alphabet", "AMCX", "Autonation Inc", "Brunswick Corp", 
                 "Buzzfeed", "CBRE", "Comcast", "Compass Inc", "CRMT", "Doordash", 
                 "Ebay", "Etsy", "EXP World Holding", "Joyy", "Kingsoft", "Nextdoor Holding", "Opendoor", 
                 "Oracle", "Paltalk", "Paramount Global", "Phoenix New", "Pinterest", 
                 "Sirius XM Holding", "Snowflake", "Spotify", "Topgolf Callaway", 
                 "Uber", "Vista Outdoor", "VROOM Inc", "Warner Bros", "Weibo", "Yelp")

IT_comp_assets <- c("Alibaba", "Alphabet", "Buzzfeed", "Ebay", "Etsy", "Joyy", "Kingsoft", 
                    "Nextdoor Holding", "Oracle", "Paltalk", "Phoenix New", "Pinterest", 
                    "Sirius XM Holding", "Snowflake", "Spotify", "Weibo", "Yelp")
NIT_comp_assets <- c("AMCX", "Autonation Inc", "Brunswick Corp", "CBRE", "Comcast", 
                     "Compass Inc", "CRMT", "Doordash", "EXP World Holding", "Opendoor", 
                     "Paramount Global", "Topgolf Callaway", "Uber", "Vista Outdoor", 
                     "VROOM Inc", "Warner Bros")


comp_all <- read_excel(path = paste0(dir_df, "IndustryCompetitors.xlsx"), sheet = "Autonation Inc", range = "A1:B1275")
names(comp_all)[2] <- comp_assets[1]

for (i in 2:length(comp_assets)){
  comp_all[1 + i] <- read_excel(path = paste0(dir_df, "IndustryCOmpetitors.xlsx"), sheet = comp_assets[i], range = "B1:B1275")
  names(comp_all)[1 + i] <- comp_assets[i]
}

# Dataframe cleaning ===================================================

# Define function to replace commas with periods in string values
replace_commas <- function(x) {
  if (is.character(x)) {
    return(as.double(gsub(",", ".",x)))
  } else {
    return(x)
  }
}

comp_all[,2:ncol(comp_all)] <- apply(comp_all[,2:ncol(comp_all)], 2, replace_commas)

# Fixing damaged values
comp_all$Kingsoft[626] <- 28.925

comp_all$CRMT[56] <- 61.945
comp_all$CRMT[61] <- 67.065
comp_all$CRMT[138] <- 110.445
comp_all$CRMT[339] <- 165.315
comp_all$CRMT[1208] <- 49.825

comp_all$`VROOM Inc`[163] <-   1.645
comp_all$`VROOM Inc`[201] <- 2.895
comp_all$`VROOM Inc`[407] <- 38.315
comp_all$`VROOM Inc`[461] <- 47.675

comp_all$Ebay[465] <- 62.865
comp_all$Ebay[578] <- 55.085
comp_all$Ebay[586] <- 58.205
comp_all$Ebay[655] <- 42.035
comp_all$Ebay[674] <- 37.455
comp_all$Ebay[720] <- 36.435
comp_all$Ebay[724] <- 37.125
comp_all$Ebay[743] <- 35.595
comp_all$Ebay[749] <- 35.795
comp_all$Ebay[756] <- 35.635
comp_all$Ebay[814] <- 38.925
comp_all$Ebay[816] <- 39.555

comp_all$Etsy[542] <- 137.845
comp_all$Etsy[570] <- 111.005
comp_all$Etsy[651] <- 75.295
comp_all$Etsy[652] <- 77.085
comp_all$Etsy[664] <- 64.445
comp_all$Etsy[743] <- 45.005
comp_all$Etsy[745] <- 44.835
comp_all$Etsy[770] <- 44.425
comp_all$Etsy[788] <- 45.655
comp_all$Etsy[805] <- 57.205

comp_all$AMCX[500] <- 33.735
comp_all$AMCX[547] <- 23.275
comp_all$AMCX[579] <- 22.895
comp_all$AMCX[596] <- 23.595
comp_all$AMCX[655] <- 27.845
comp_all$AMCX[699] <- 25.345
comp_all$AMCX[725] <- 37.425
comp_all$AMCX[742] <- 40.015
comp_all$AMCX[743] <- 40.685
comp_all$AMCX[806] <- 46.925
comp_all$AMCX[813] <- 48.135

comp_all$Comcast[402] <- 55.455
comp_all$Comcast[420] <- 54.655
comp_all$Comcast[426] <- 53.875
comp_all$Comcast[451] <- 55.475
comp_all$Comcast[484] <- 48.845
comp_all$Comcast[500] <- 50.035
comp_all$Comcast[659] <- 36.535
comp_all$Comcast[663] <- 36.215
comp_all$Comcast[775] <- 44.215
comp_all$Comcast[817] <- 46.215

comp_all$Joyy[133] <- 34.595
comp_all$Joyy[155] <- 35.685

comp_all$Opendoor[29] <- 1.735
comp_all$Opendoor[45] <- 2.445
comp_all$Opendoor[86] <- 5.085
comp_all$Opendoor[338] <- 14.795
comp_all$Opendoor[411] <- 19.315
comp_all$Opendoor[419] <- 18.685
comp_all$Opendoor[453] <- 24.385
comp_all$Opendoor[597] <- 11.035
comp_all$Opendoor[606] <- 11.925
comp_all$Opendoor[621] <- 114.803
comp_all$Opendoor[631] <- 108.038

comp_all$`Sirius XM Holding`[404] <- 5.835
comp_all$`Sirius XM Holding`[412] <- 6.055
comp_all$`Sirius XM Holding`[415] <- 6.065
comp_all$`Sirius XM Holding`[419] <- 6.295
comp_all$`Sirius XM Holding`[470] <- 6.055
comp_all$`Sirius XM Holding`[471] <- 6.105
comp_all$`Sirius XM Holding`[498] <- 6.345
comp_all$`Sirius XM Holding`[499] <- 6.305
comp_all$`Sirius XM Holding`[550] <- 5.885
comp_all$`Sirius XM Holding`[554] <- 5.815
comp_all$`Sirius XM Holding`[590] <- 5.995
comp_all$`Sirius XM Holding`[592] <- 6.005
comp_all$`Sirius XM Holding`[594] <- 6.045
comp_all$`Sirius XM Holding`[606] <- 5.905
comp_all$`Sirius XM Holding`[633] <- 6.035
comp_all$`Sirius XM Holding`[635] <- 5.935
comp_all$`Sirius XM Holding`[638] <- 6.325
comp_all$`Sirius XM Holding`[639] <- 6.515
comp_all$`Sirius XM Holding`[644] <- 5.965
comp_all$`Sirius XM Holding`[649] <- 5.305
comp_all$`Sirius XM Holding`[650] <- 5.195
comp_all$`Sirius XM Holding`[655] <- 5.285
comp_all$`Sirius XM Holding`[659] <- 5.745
comp_all$`Sirius XM Holding`[667] <- 5.755
comp_all$`Sirius XM Holding`[669] <- 5.505
comp_all$`Sirius XM Holding`[731] <- 7.115
comp_all$`Sirius XM Holding`[750] <- 7.135
comp_all$`Sirius XM Holding`[773] <- 6.805
comp_all$`Sirius XM Holding`[801] <- 6.435
comp_all$`Sirius XM Holding`[805] <- 6.245
comp_all$`Sirius XM Holding`[812] <- 6.255
comp_all$`Sirius XM Holding`[813] <- 6.205
comp_all$`Sirius XM Holding`[815] <- 6.315
comp_all$`Sirius XM Holding`[1057] <- 6.195
comp_all$`Sirius XM Holding`[1098] <- 6.995
comp_all$`Sirius XM Holding`[1191] <- 6.175
comp_all$`Sirius XM Holding`[1244] <- 5.555

comp_all$`Phoenix New`[77] <- 49.039
comp_all$`Phoenix New`[146] <- 27.012
comp_all$`Phoenix New`[147] <- 2.856
comp_all$`Phoenix New`[148] <- 29.154
comp_all$`Phoenix New`[150] <- 2.745
comp_all$`Phoenix New`[152] <- 26.103
comp_all$`Phoenix New`[154] <- 26.466
comp_all$`Phoenix New`[155] <- 26.412
comp_all$`Phoenix New`[157] <- 32.004
comp_all$`Phoenix New`[158] <- 32.394
comp_all$`Phoenix New`[159] <- 3.312
comp_all$`Phoenix New`[160] <- 3.246
comp_all$`Phoenix New`[162] <- 30.126
comp_all$`Phoenix New`[163] <- 31.476
comp_all$`Phoenix New`[165] <- 3.327
comp_all$`Phoenix New`[166] <- 33.078
comp_all$`Phoenix New`[167] <- 3.507
comp_all$`Phoenix New`[168] <- 3.642
comp_all$`Phoenix New`[170] <- 3.555
comp_all$`Phoenix New`[172] <- 3.426
comp_all$`Phoenix New`[173] <- 33.846
comp_all$`Phoenix New`[174] <- 3.372
comp_all$`Phoenix New`[175] <- 35.448
comp_all$`Phoenix New`[176] <- 35.394
comp_all$`Phoenix New`[177] <- 3.408
comp_all$`Phoenix New`[180] <- 32.946
comp_all$`Phoenix New`[182] <- 33.486
comp_all$`Phoenix New`[183] <- 34.806
comp_all$`Phoenix New`[185] <- 3.477
comp_all$`Phoenix New`[186] <- 3.558
comp_all$`Phoenix New`[188] <- 35.412
comp_all$`Phoenix New`[189] <- 35.772
comp_all$`Phoenix New`[190] <- 36.006
comp_all$`Phoenix New`[192] <- 3.516
comp_all$`Phoenix New`[193] <- 32.994
comp_all$`Phoenix New`[194] <- 32.736
comp_all$`Phoenix New`[195] <- 37.482
comp_all$`Phoenix New`[196] <- 39.036
comp_all$`Phoenix New`[198] <- 40.764
comp_all$`Phoenix New`[200] <- 4.086
comp_all$`Phoenix New`[201] <- 43.008
comp_all$`Phoenix New`[202] <- 44.694
comp_all$`Phoenix New`[203] <- 4.458
comp_all$`Phoenix New`[204] <- 4.608
comp_all$`Phoenix New`[205] <- 45.774
comp_all$`Phoenix New`[206] <- 43.806
comp_all$`Phoenix New`[208] <- 45.018
comp_all$`Phoenix New`[209] <- 45.606
comp_all$`Phoenix New`[210] <- 48.594
comp_all$`Phoenix New`[211] <- 47.436
comp_all$`Phoenix New`[212] <- 48.408
comp_all$`Phoenix New`[213] <- 46.782
comp_all$`Phoenix New`[215] <- 47.412
comp_all$`Phoenix New`[217] <- 44.628
comp_all$`Phoenix New`[218] <- 46.026
comp_all$`Phoenix New`[220] <- 47.604
comp_all$`Phoenix New`[221] <- 4.914
comp_all$`Phoenix New`[224] <- 4.167
comp_all$`Phoenix New`[227] <- 42.738
comp_all$`Phoenix New`[229] <- 44.364
comp_all$`Phoenix New`[230] <- 47.226
comp_all$`Phoenix New`[231] <- 45.558
comp_all$`Phoenix New`[232] <- 47.256
comp_all$`Phoenix New`[233] <- 48.894
comp_all$`Phoenix New`[236] <- 48.606
comp_all$`Phoenix New`[238] <- 49.956
comp_all$`Phoenix New`[239] <- 48.618
comp_all$`Phoenix New`[240] <- 48.318
comp_all$`Phoenix New`[241] <- 5.058
comp_all$`Phoenix New`[242] <- 53.082
comp_all$`Phoenix New`[243] <- 5.172
comp_all$`Phoenix New`[244] <- 51.714
comp_all$`Phoenix New`[246] <- 4.884
comp_all$`Phoenix New`[247] <- 51.204
comp_all$`Phoenix New`[249] <- 51.606
comp_all$`Phoenix New`[250] <- 51.048
comp_all$`Phoenix New`[251] <- 50.196
comp_all$`Phoenix New`[252] <- 51.384
comp_all$`Phoenix New`[253] <- 52.248
comp_all$`Phoenix New`[254] <- 50.478
comp_all$`Phoenix New`[255] <- 5.394
comp_all$`Phoenix New`[257] <- 56.316
comp_all$`Phoenix New`[258] <- 58.788
comp_all$`Phoenix New`[259] <- 5.805
comp_all$`Phoenix New`[262] <- 56.394
comp_all$`Phoenix New`[269] <- 65.394
comp_all$`Phoenix New`[637] <- 93.606
comp_all$`Phoenix New`[639] <- 85.194
comp_all$`Phoenix New`[674] <- 85.962
comp_all$`Phoenix New`[676] <- 80.562
comp_all$`Phoenix New`[686] <- 7.428
comp_all$`Phoenix New`[700] <- 91.434
comp_all$`Phoenix New`[703] <- 10.692
comp_all$`Phoenix New`[709] <- 106.272
comp_all$`Phoenix New`[717] <- 11.847
comp_all$`Phoenix New`[730] <- 116.406
comp_all$`Phoenix New`[807] <- 14.676
comp_all$`Phoenix New`[912] <- 24.495
comp_all$`Phoenix New`[915] <- 261.599
comp_all$`Phoenix New`[916] <- 256.799
comp_all$`Phoenix New`[917] <- 257.999
comp_all$`Phoenix New`[918] <- 260.999
comp_all$`Phoenix New`[919] <- 262.799
comp_all$`Phoenix New`[920] <- 256.199
comp_all$`Phoenix New`[921] <- 257.999
comp_all$`Phoenix New`[922] <- 257.999
comp_all$`Phoenix New`[923] <- 266.999
comp_all$`Phoenix New`[924] <- 257.399
comp_all$`Phoenix New`[925] <- 260.399
comp_all$`Phoenix New`[926] <- 257.999
comp_all$`Phoenix New`[927] <- 261.599
comp_all$`Phoenix New`[928] <- 258.599
comp_all$`Phoenix New`[929] <- 262.799
comp_all$`Phoenix New`[930] <- 266.999
comp_all$`Phoenix New`[931] <- 273.599
comp_all$`Phoenix New`[932] <- 268.199
comp_all$`Phoenix New`[933] <- 270.599
comp_all$`Phoenix New`[934] <- 267.599
comp_all$`Phoenix New`[935] <- 261.599
comp_all$`Phoenix New`[936] <- 254.999
comp_all$`Phoenix New`[939] <- 252.599
comp_all$`Phoenix New`[945] <- 256.199
comp_all$`Phoenix New`[947] <- 263.999
comp_all$`Phoenix New`[948] <- 247.199
comp_all$`Phoenix New`[952] <- 251.999
comp_all$`Phoenix New`[953] <- 251.999
comp_all$`Phoenix New`[954] <- 251.999
comp_all$`Phoenix New`[955] <- 257.999
comp_all$`Phoenix New`[956] <- 278.999
comp_all$`Phoenix New`[957] <- 284.999
comp_all$`Phoenix New`[958] <- 288.599
comp_all$`Phoenix New`[959] <- 283.799
comp_all$`Phoenix New`[960] <- 286.799
comp_all$`Phoenix New`[961] <- 290.399
comp_all$`Phoenix New`[962] <- 280.799
comp_all$`Phoenix New`[963] <- 284.999
comp_all$`Phoenix New`[1064] <- 259.199
comp_all$`Phoenix New`[1068] <- 252.599
comp_all$`Phoenix New`[1069] <- 256.799
comp_all$`Phoenix New`[1070] <- 263.999
comp_all$`Phoenix New`[1071] <- 267.599
comp_all$`Phoenix New`[1072] <- 267.599
comp_all$`Phoenix New`[1073] <- 270.599
comp_all$`Phoenix New`[1074] <- 274.199
comp_all$`Phoenix New`[1075] <- 265.799
comp_all$`Phoenix New`[1076] <- 262.799
comp_all$`Phoenix New`[1077] <- 274.199
comp_all$`Phoenix New`[1078] <- 290.399
comp_all$`Phoenix New`[1079] <- 301.799
comp_all$`Phoenix New`[1080] <- 303.599
comp_all$`Phoenix New`[1081] <- 302.399
comp_all$`Phoenix New`[1082] <- 307.199
comp_all$`Phoenix New`[1083] <- 294.599
comp_all$`Phoenix New`[1084] <- 294.599
comp_all$`Phoenix New`[1085] <- 280.199
comp_all$`Phoenix New`[1086] <- 274.799
comp_all$`Phoenix New`[1087] <- 267.599
comp_all$`Phoenix New`[1088] <- 265.199
comp_all$`Phoenix New`[1089] <- 268.799
comp_all$`Phoenix New`[1090] <- 268.799
comp_all$`Phoenix New`[1091] <- 266.399
comp_all$`Phoenix New`[1092] <- 260.999
comp_all$`Phoenix New`[1093] <- 257.999
comp_all$`Phoenix New`[1097] <- 265.799
comp_all$`Phoenix New`[1098] <- 260.999
comp_all$`Phoenix New`[1100] <- 249.599
comp_all$`Phoenix New`[1103] <- 252.599
comp_all$`Phoenix New`[1104] <- 256.799
comp_all$`Phoenix New`[1113] <- 262.199
comp_all$`Phoenix New`[1114] <- 262.799
comp_all$`Phoenix New`[1115] <- 272.999
comp_all$`Phoenix New`[1116] <- 268.799
comp_all$`Phoenix New`[1117] <- 275.999
comp_all$`Phoenix New`[1118] <- 276.599
comp_all$`Phoenix New`[1119] <- 272.399
comp_all$`Phoenix New`[1120] <- 284.999
comp_all$`Phoenix New`[1121] <- 292.799
comp_all$`Phoenix New`[1122] <- 272.399
comp_all$`Phoenix New`[1123] <- 271.799
comp_all$`Phoenix New`[1124] <- 260.399
comp_all$`Phoenix New`[1125] <- 259.799
comp_all$`Phoenix New`[1126] <- 254.999
comp_all$`Phoenix New`[1128] <- 254.999
comp_all$`Phoenix New`[1129] <- 263.399
comp_all$`Phoenix New`[1130] <- 267.599
comp_all$`Phoenix New`[1131] <- 273.599
comp_all$`Phoenix New`[1132] <- 274.799
comp_all$`Phoenix New`[1133] <- 275.999
comp_all$`Phoenix New`[1134] <- 268.799
comp_all$`Phoenix New`[1135] <- 285.599
comp_all$`Phoenix New`[1136] <- 295.199
comp_all$`Phoenix New`[1137] <- 302.999
comp_all$`Phoenix New`[1138] <- 305.399
comp_all$`Phoenix New`[1139] <- 308.399
comp_all$`Phoenix New`[1140] <- 322.199
comp_all$`Phoenix New`[1141] <- 311.999
comp_all$`Phoenix New`[1142] <- 308.399
comp_all$`Phoenix New`[1143] <- 308.399
comp_all$`Phoenix New`[1144] <- 319.199
comp_all$`Phoenix New`[1145] <- 329.999
comp_all$`Phoenix New`[1146] <- 328.199
comp_all$`Phoenix New`[1147] <- 330.599
comp_all$`Phoenix New`[1148] <- 329.999
comp_all$`Phoenix New`[1149] <- 334.799
comp_all$`Phoenix New`[1150] <- 330.599
comp_all$`Phoenix New`[1151] <- 332.999
comp_all$`Phoenix New`[1152] <- 338.999
comp_all$`Phoenix New`[1153] <- 337.199
comp_all$`Phoenix New`[1154] <- 337.799
comp_all$`Phoenix New`[1155] <- 332.999
comp_all$`Phoenix New`[1156] <- 330.599
comp_all$`Phoenix New`[1157] <- 332.999
comp_all$`Phoenix New`[1158] <- 292.199
comp_all$`Phoenix New`[1159] <- 251.399
comp_all$`Phoenix New`[1161] <- 256.799
comp_all$`Phoenix New`[1174] <- 247.199
comp_all$`Phoenix New`[1176] <- 251.399
comp_all$`Phoenix New`[1177] <- 254.399
comp_all$`Phoenix New`[1178] <- 251.999
comp_all$`Phoenix New`[1182] <- 256.799
comp_all$`Phoenix New`[1183] <- 255.599
comp_all$`Phoenix New`[1184] <- 251.399
comp_all$`Phoenix New`[1185] <- 255.599
comp_all$`Phoenix New`[1186] <- 255.599
comp_all$`Phoenix New`[1187] <- 258.599
comp_all$`Phoenix New`[1188] <- 261.599
comp_all$`Phoenix New`[1189] <- 251.399
comp_all$`Phoenix New`[1190] <- 260.999
comp_all$`Phoenix New`[1191] <- 257.999
comp_all$`Phoenix New`[1192] <- 260.399
comp_all$`Phoenix New`[1195] <- 260.999
comp_all$`Phoenix New`[1196] <- 274.199
comp_all$`Phoenix New`[1197] <- 276.599
comp_all$`Phoenix New`[1198] <- 281.399
comp_all$`Phoenix New`[1199] <- 283.199
comp_all$`Phoenix New`[1200] <- 290.999
comp_all$`Phoenix New`[1201] <- 299.399
comp_all$`Phoenix New`[1202] <- 301.199
comp_all$`Phoenix New`[1203] <- 343.199
comp_all$`Phoenix New`[1204] <- 331.799
comp_all$`Phoenix New`[1205] <- 320.999
comp_all$`Phoenix New`[1206] <- 321.599
comp_all$`Phoenix New`[1207] <- 330.599
comp_all$`Phoenix New`[1208] <- 316.199
comp_all$`Phoenix New`[1209] <- 302.999
comp_all$`Phoenix New`[1210] <- 300.599
comp_all$`Phoenix New`[1211] <- 310.799
comp_all$`Phoenix New`[1212] <- 309.599
comp_all$`Phoenix New`[1213] <- 335.999
comp_all$`Phoenix New`[1214] <- 331.799
comp_all$`Phoenix New`[1215] <- 327.599
comp_all$`Phoenix New`[1216] <- 346.199
comp_all$`Phoenix New`[1217] <- 317.399
comp_all$`Phoenix New`[1218] <- 327.599
comp_all$`Phoenix New`[1219] <- 338.999
comp_all$`Phoenix New`[1220] <- 344.999
comp_all$`Phoenix New`[1221] <- 337.799
comp_all$`Phoenix New`[1222] <- 323.999
comp_all$`Phoenix New`[1223] <- 328.199
comp_all$`Phoenix New`[1224] <- 327.599
comp_all$`Phoenix New`[1225] <- 343.799
comp_all$`Phoenix New`[1226] <- 350.399
comp_all$`Phoenix New`[1227] <- 352.199
comp_all$`Phoenix New`[1228] <- 370.799
comp_all$`Phoenix New`[1229] <- 404.999
comp_all$`Phoenix New`[1230] <- 396.599
comp_all$`Phoenix New`[1231] <- 397.199
comp_all$`Phoenix New`[1232] <- 410.999
comp_all$`Phoenix New`[1233] <- 418.199
comp_all$`Phoenix New`[1234] <- 404.999
comp_all$`Phoenix New`[1235] <- 398.999
comp_all$`Phoenix New`[1236] <- 406.799
comp_all$`Phoenix New`[1237] <- 417.599
comp_all$`Phoenix New`[1238] <- 425.999
comp_all$`Phoenix New`[1239] <- 421.799
comp_all$`Phoenix New`[1240] <- 421.799
comp_all$`Phoenix New`[1241] <- 409.799
comp_all$`Phoenix New`[1242] <- 422.399
comp_all$`Phoenix New`[1243] <- 434.399
comp_all$`Phoenix New`[1244] <- 419.399
comp_all$`Phoenix New`[1245] <- 420.599
comp_all$`Phoenix New`[1246] <- 430.799
comp_all$`Phoenix New`[1247] <- 433.199
comp_all$`Phoenix New`[1248] <- 404.999
comp_all$`Phoenix New`[1249] <- 400.199
comp_all$`Phoenix New`[1250] <- 407.399
comp_all$`Phoenix New`[1251] <- 389.399
comp_all$`Phoenix New`[1252] <- 398.399
comp_all$`Phoenix New`[1253] <- 392.999
comp_all$`Phoenix New`[1254] <- 392.399
comp_all$`Phoenix New`[1255] <- 407.999
comp_all$`Phoenix New`[1256] <- 411.599
comp_all$`Phoenix New`[1257] <- 417.599
comp_all$`Phoenix New`[1258] <- 403.199
comp_all$`Phoenix New`[1259] <- 407.399
comp_all$`Phoenix New`[1260] <- 427.799
comp_all$`Phoenix New`[1261] <- 401.999
comp_all$`Phoenix New`[1262] <- 389.999
comp_all$`Phoenix New`[1263] <- 389.999
comp_all$`Phoenix New`[1264] <- 406.199
comp_all$`Phoenix New`[1265] <- 382.799
comp_all$`Phoenix New`[1266] <- 377.399
comp_all$`Phoenix New`[1267] <- 374.999
comp_all$`Phoenix New`[1268] <- 376.799
comp_all$`Phoenix New`[1269] <- 372.599
comp_all$`Phoenix New`[1270] <- 388.799
comp_all$`Phoenix New`[1271] <- 403.199
comp_all$`Phoenix New`[1272] <- 399.599
comp_all$`Phoenix New`[1273] <- 406.799
comp_all$`Phoenix New`[1274] <- 412.199

comp_all$`EXP World Holding`[32] <- 11.775
comp_all$`EXP World Holding`[327] <- 49.255
comp_all$`EXP World Holding`[466] <- 77.885
comp_all$`EXP World Holding`[467] <- 72.915
comp_all$`EXP World Holding`[469] <- 80.525
comp_all$`EXP World Holding`[470] <- 75.095
comp_all$`EXP World Holding`[471] <- 67.485
comp_all$`EXP World Holding`[473] <- 59.505
comp_all$`EXP World Holding`[475] <- 57.535
comp_all$`EXP World Holding`[476] <- 53.295
comp_all$`EXP World Holding`[482] <- 48.865
comp_all$`EXP World Holding`[484] <- 39.855
comp_all$`EXP World Holding`[485] <- 38.985
comp_all$`EXP World Holding`[486] <- 38.705
comp_all$`EXP World Holding`[487] <- 37.685
comp_all$`EXP World Holding`[491] <- 33.035
comp_all$`EXP World Holding`[492] <- 30.385
comp_all$`EXP World Holding`[493] <- 31.365
comp_all$`EXP World Holding`[494] <- 30.695
comp_all$`EXP World Holding`[498] <- 33.935
comp_all$`EXP World Holding`[499] <- 39.315
comp_all$`EXP World Holding`[504] <- 38.055
comp_all$`EXP World Holding`[507] <- 34.205
comp_all$`EXP World Holding`[511] <- 29.325
comp_all$`EXP World Holding`[517] <- 26.665
comp_all$`EXP World Holding`[518] <- 27.075
comp_all$`EXP World Holding`[521] <- 25.285
comp_all$`EXP World Holding`[524] <- 21.615
comp_all$`EXP World Holding`[525] <- 21.805
comp_all$`EXP World Holding`[529] <- 22.255
comp_all$`EXP World Holding`[530] <- 20.555
comp_all$`EXP World Holding`[535] <- 21.605
comp_all$`EXP World Holding`[536] <- 20.765
comp_all$`EXP World Holding`[537] <- 21.195
comp_all$`EXP World Holding`[539] <- 22.835
comp_all$`EXP World Holding`[541] <- 20.765
comp_all$`EXP World Holding`[546] <- 28.625
comp_all$`EXP World Holding`[547] <- 28.485
comp_all$`EXP World Holding`[548] <- 30.545
comp_all$`EXP World Holding`[549] <- 29.635
comp_all$`EXP World Holding`[551] <- 29.445
comp_all$`EXP World Holding`[552] <- 29.495
comp_all$`EXP World Holding`[555] <- 25.575
comp_all$`EXP World Holding`[557] <- 21.865
comp_all$`EXP World Holding`[560] <- 21.025
comp_all$`EXP World Holding`[561] <- 20.935
comp_all$`EXP World Holding`[566] <- 19.745
comp_all$`EXP World Holding`[567] <- 19.525
comp_all$`EXP World Holding`[569] <- 20.815
comp_all$`EXP World Holding`[570] <- 20.845
comp_all$`EXP World Holding`[572] <- 21.105
comp_all$`EXP World Holding`[575] <- 19.855
comp_all$`EXP World Holding`[577] <- 19.505
comp_all$`EXP World Holding`[579] <- 227.325
comp_all$`EXP World Holding`[581] <- 20.485
comp_all$`EXP World Holding`[584] <- 20.505
comp_all$`EXP World Holding`[586] <- 20.265
comp_all$`EXP World Holding`[587] <- 20.945
comp_all$`EXP World Holding`[588] <- 20.335
comp_all$`EXP World Holding`[591] <- 17.145
comp_all$`EXP World Holding`[592] <- 16.795
comp_all$`EXP World Holding`[593] <- 15.455
comp_all$`EXP World Holding`[594] <- 14.995
comp_all$`EXP World Holding`[598] <- 12.075
comp_all$`EXP World Holding`[599] <- 11.415
comp_all$`EXP World Holding`[601] <- 9.935
comp_all$`EXP World Holding`[602] <- 10.305
comp_all$`EXP World Holding`[603] <- 10.165
comp_all$`EXP World Holding`[604] <- 9.535
comp_all$`EXP World Holding`[606] <- 9.295
comp_all$`EXP World Holding`[607] <- 10.445
comp_all$`EXP World Holding`[608] <- 10.055
comp_all$`EXP World Holding`[609] <- 9.405
comp_all$`EXP World Holding`[610] <- 8.835
comp_all$`EXP World Holding`[611] <- 8.425
comp_all$`EXP World Holding`[613] <- 8.235
comp_all$`EXP World Holding`[616] <- 8.125
comp_all$`EXP World Holding`[617] <- 8.365
comp_all$`EXP World Holding`[618] <- 8.395
comp_all$`EXP World Holding`[622] <- 80.325
comp_all$`EXP World Holding`[623] <- 8.525
comp_all$`EXP World Holding`[624] <- 8.575
comp_all$`EXP World Holding`[625] <- 8.465
comp_all$`EXP World Holding`[626] <- 8.075
comp_all$`EXP World Holding`[627] <- 7.775
comp_all$`EXP World Holding`[628] <- 7.865
comp_all$`EXP World Holding`[629] <- 7.325
comp_all$`EXP World Holding`[632] <- 5.975
comp_all$`EXP World Holding`[633] <- 6.035
comp_all$`EXP World Holding`[635] <- 5.825
comp_all$`EXP World Holding`[636] <- 57.775
comp_all$`EXP World Holding`[640] <- 58.325
comp_all$`EXP World Holding`[641] <- 59.625
comp_all$`EXP World Holding`[644] <- 5.235
comp_all$`EXP World Holding`[645] <- 5.345
comp_all$`EXP World Holding`[646] <- 5.065
comp_all$`EXP World Holding`[648] <- 4.685
comp_all$`EXP World Holding`[650] <- 4.495
comp_all$`EXP World Holding`[652] <- 4.455
comp_all$`EXP World Holding`[653] <- 4.505
comp_all$`EXP World Holding`[658] <- 4.545
comp_all$`EXP World Holding`[659] <- 4.435
comp_all$`EXP World Holding`[666] <- 4.765
comp_all$`EXP World Holding`[667] <- 4.515
comp_all$`EXP World Holding`[668] <- 4.415
comp_all$`EXP World Holding`[670] <- 4.205
comp_all$`EXP World Holding`[671] <- 4.185
comp_all$`EXP World Holding`[672] <- 4.125
comp_all$`EXP World Holding`[674] <- 4.095
comp_all$`EXP World Holding`[677] <- 4.165
comp_all$`EXP World Holding`[679] <- 4.125
comp_all$`EXP World Holding`[682] <- 3.755
comp_all$`EXP World Holding`[683] <- 3.545
comp_all$`EXP World Holding`[687] <- 4.125
comp_all$`EXP World Holding`[688] <- 4.075
comp_all$`EXP World Holding`[690] <- 4.125
comp_all$`EXP World Holding`[692] <- 3.625
comp_all$`EXP World Holding`[693] <- 3.665
comp_all$`EXP World Holding`[695] <- 3.505
comp_all$`EXP World Holding`[699] <- 4.525
comp_all$`EXP World Holding`[700] <- 4.295
comp_all$`EXP World Holding`[701] <- 4.405
comp_all$`EXP World Holding`[704] <- 4.745
comp_all$`EXP World Holding`[706] <- 4.895
comp_all$`EXP World Holding`[710] <- 5.435
comp_all$`EXP World Holding`[713] <- 5.735
comp_all$`EXP World Holding`[714] <- 5.705
comp_all$`EXP World Holding`[715] <- 5.685
comp_all$`EXP World Holding`[716] <- 5.615
comp_all$`EXP World Holding`[718] <- 5.775
comp_all$`EXP World Holding`[719] <- 5.825
comp_all$`EXP World Holding`[720] <- 5.635
comp_all$`EXP World Holding`[721] <- 5.565
comp_all$`EXP World Holding`[723] <- 5.865
comp_all$`EXP World Holding`[724] <- 5.685
comp_all$`EXP World Holding`[725] <- 5.545
comp_all$`EXP World Holding`[729] <- 5.765
comp_all$`EXP World Holding`[736] <- 5.415
comp_all$`EXP World Holding`[739] <- 5.335
comp_all$`EXP World Holding`[740] <- 5.225
comp_all$`EXP World Holding`[746] <- 5.535
comp_all$`EXP World Holding`[748] <- 5.665
comp_all$`EXP World Holding`[749] <- 5.805
comp_all$`EXP World Holding`[750] <- 5.955
comp_all$`EXP World Holding`[751] <- 5.925
comp_all$`EXP World Holding`[752] <- 6.025
comp_all$`EXP World Holding`[753] <- 5.975
comp_all$`EXP World Holding`[754] <- 5.855
comp_all$`EXP World Holding`[755] <- 5.815
comp_all$`EXP World Holding`[756] <- 5.805
comp_all$`EXP World Holding`[757] <- 5.855
comp_all$`EXP World Holding`[759] <- 5.995
comp_all$`EXP World Holding`[762] <- 5.715
comp_all$`EXP World Holding`[763] <- 5.645
comp_all$`EXP World Holding`[764] <- 5.785
comp_all$`EXP World Holding`[765] <- 5.475
comp_all$`EXP World Holding`[769] <- 5.475
comp_all$`EXP World Holding`[772] <- 5.205
comp_all$`EXP World Holding`[775] <- 4.915
comp_all$`EXP World Holding`[777] <- 4.945
comp_all$`EXP World Holding`[778] <- 4.835
comp_all$`EXP World Holding`[779] <- 4.975
comp_all$`EXP World Holding`[781] <- 4.975
comp_all$`EXP World Holding`[782] <- 4.925
comp_all$`EXP World Holding`[783] <- 4.795
comp_all$`EXP World Holding`[787] <- 4.685
comp_all$`EXP World Holding`[788] <- 4.515
comp_all$`EXP World Holding`[789] <- 4.485
comp_all$`EXP World Holding`[791] <- 4.375
comp_all$`EXP World Holding`[792] <- 4.485
comp_all$`EXP World Holding`[794] <- 4.325
comp_all$`EXP World Holding`[797] <- 4.275
comp_all$`EXP World Holding`[799] <- 4.055
comp_all$`EXP World Holding`[800] <- 4.215
comp_all$`EXP World Holding`[801] <- 4.215
comp_all$`EXP World Holding`[804] <- 4.215
comp_all$`EXP World Holding`[805] <- 4.245
comp_all$`EXP World Holding`[806] <- 4.225
comp_all$`EXP World Holding`[807] <- 4.125
comp_all$`EXP World Holding`[808] <- 4.085
comp_all$`EXP World Holding`[810] <- 4.125
comp_all$`EXP World Holding`[811] <- 4.095
comp_all$`EXP World Holding`[815] <- 4.115
comp_all$`EXP World Holding`[816] <- 4.055
comp_all$`EXP World Holding`[817] <- 4.185
comp_all$`EXP World Holding`[819] <- 4.235
comp_all$`EXP World Holding`[821] <- 4.335
comp_all$`EXP World Holding`[822] <- 4.405
comp_all$`EXP World Holding`[824] <- 4.455
comp_all$`EXP World Holding`[825] <- 4.665
comp_all$`EXP World Holding`[826] <- 4.195
comp_all$`EXP World Holding`[828] <- 4.215
comp_all$`EXP World Holding`[829] <- 4.185
comp_all$`EXP World Holding`[830] <- 4.215
comp_all$`EXP World Holding`[831] <- 4.195
comp_all$`EXP World Holding`[833] <- 4.265
comp_all$`EXP World Holding`[835] <- 4.055
comp_all$`EXP World Holding`[836] <- 4.165
comp_all$`EXP World Holding`[840] <- 4.705
comp_all$`EXP World Holding`[841] <- 4.795
comp_all$`EXP World Holding`[844] <- 4.665
comp_all$`EXP World Holding`[848] <- 4.945
comp_all$`EXP World Holding`[852] <- 5.265
comp_all$`EXP World Holding`[854] <- 5.285
comp_all$`EXP World Holding`[855] <- 5.435
comp_all$`EXP World Holding`[858] <- 5.045
comp_all$`EXP World Holding`[859] <- 5.235
comp_all$`EXP World Holding`[860] <- 5.255
comp_all$`EXP World Holding`[862] <- 5.575
comp_all$`EXP World Holding`[864] <- 5.485
comp_all$`EXP World Holding`[865] <- 5.385
comp_all$`EXP World Holding`[866] <- 5.165
comp_all$`EXP World Holding`[869] <- 5.185
comp_all$`EXP World Holding`[870] <- 5.345
comp_all$`EXP World Holding`[873] <- 5.285
comp_all$`EXP World Holding`[874] <- 5.435
comp_all$`EXP World Holding`[876] <- 5.565
comp_all$`EXP World Holding`[877] <- 5.325
comp_all$`EXP World Holding`[878] <- 5.125
comp_all$`EXP World Holding`[879] <- 5.385
comp_all$`EXP World Holding`[882] <- 5.735
comp_all$`EXP World Holding`[884] <- 5.405
comp_all$`EXP World Holding`[886] <- 5.185
comp_all$`EXP World Holding`[887] <- 5.395
comp_all$`EXP World Holding`[888] <- 5.225
comp_all$`EXP World Holding`[890] <- 5.595
comp_all$`EXP World Holding`[891] <- 5.835
comp_all$`EXP World Holding`[894] <- 5.055
comp_all$`EXP World Holding`[899] <- 5.335
comp_all$`EXP World Holding`[900] <- 5.535
comp_all$`EXP World Holding`[901] <- 5.995
comp_all$`EXP World Holding`[902] <- 6.175
comp_all$`EXP World Holding`[904] <- 5.905
comp_all$`EXP World Holding`[905] <- 5.685
comp_all$`EXP World Holding`[907] <- 5.435
comp_all$`EXP World Holding`[910] <- 4.975
comp_all$`EXP World Holding`[911] <- 4.755
comp_all$`EXP World Holding`[912] <- 4.855
comp_all$`EXP World Holding`[913] <- 4.955
comp_all$`EXP World Holding`[916] <- 5.215
comp_all$`EXP World Holding`[926] <- 4.965
comp_all$`EXP World Holding`[928] <- 4.785
comp_all$`EXP World Holding`[929] <- 4.965
comp_all$`EXP World Holding`[930] <- 5.125
comp_all$`EXP World Holding`[931] <- 5.005
comp_all$`EXP World Holding`[935] <- 5.055
comp_all$`EXP World Holding`[936] <- 5.245
comp_all$`EXP World Holding`[937] <- 5.335
comp_all$`EXP World Holding`[939] <- 5.435
comp_all$`EXP World Holding`[941] <- 5.335
comp_all$`EXP World Holding`[944] <- 5.385
comp_all$`EXP World Holding`[946] <- 5.625
comp_all$`EXP World Holding`[949] <- 5.125
comp_all$`EXP World Holding`[950] <- 4.915
comp_all$`EXP World Holding`[951] <- 4.985
comp_all$`EXP World Holding`[954] <- 4.945
comp_all$`EXP World Holding`[955] <- 5.075
comp_all$`EXP World Holding`[956] <- 5.295
comp_all$`EXP World Holding`[957] <- 5.455
comp_all$`EXP World Holding`[959] <- 5.645
comp_all$`EXP World Holding`[960] <- 5.625
comp_all$`EXP World Holding`[962] <- 5.685
comp_all$`EXP World Holding`[964] <- 5.755
comp_all$`EXP World Holding`[968] <- 5.545
comp_all$`EXP World Holding`[969] <- 5.475
comp_all$`EXP World Holding`[972] <- 5.395
comp_all$`EXP World Holding`[973] <- 5.385
comp_all$`EXP World Holding`[974] <- 5.435
comp_all$`EXP World Holding`[977] <- 5.445
comp_all$`EXP World Holding`[978] <- 5.475
comp_all$`EXP World Holding`[980] <- 5.495
comp_all$`EXP World Holding`[983] <- 5.215
comp_all$`EXP World Holding`[984] <- 5.105
comp_all$`EXP World Holding`[985] <- 4.775
comp_all$`EXP World Holding`[987] <- 5.225
comp_all$`EXP World Holding`[989] <- 5.105
comp_all$`EXP World Holding`[991] <- 4.665
comp_all$`EXP World Holding`[997] <- 3.595
comp_all$`EXP World Holding`[999] <- 3.585
comp_all$`EXP World Holding`[1001] <- 3.625
comp_all$`EXP World Holding`[1004] <- 3.475
comp_all$`EXP World Holding`[1006] <- 4.025
comp_all$`EXP World Holding`[1012] <- 5.125
comp_all$`EXP World Holding`[1014] <- 5.025
comp_all$`EXP World Holding`[1015] <- 4.805
comp_all$`EXP World Holding`[1018] <- 5.475
comp_all$`EXP World Holding`[1019] <- 5.425
comp_all$`EXP World Holding`[1021] <- 5.135
comp_all$`EXP World Holding`[1025] <- 5.575
comp_all$`EXP World Holding`[1026] <- 5.195
comp_all$`EXP World Holding`[1027] <- 5.475
comp_all$`EXP World Holding`[1029] <- 5.725
comp_all$`EXP World Holding`[1030] <- 5.615
comp_all$`EXP World Holding`[1031] <- 6.085
comp_all$`EXP World Holding`[1033] <- 5.875
comp_all$`EXP World Holding`[1035] <- 6.115
comp_all$`EXP World Holding`[1036] <- 6.095
comp_all$`EXP World Holding`[1037] <- 5.915
comp_all$`EXP World Holding`[1038] <- 5.825
comp_all$`EXP World Holding`[1039] <- 5.825
comp_all$`EXP World Holding`[1043] <- 5.325
comp_all$`EXP World Holding`[1045] <- 5.955
comp_all$`EXP World Holding`[1050] <- 6.415
comp_all$`EXP World Holding`[1053] <- 6.385
comp_all$`EXP World Holding`[1056] <- 7.375
comp_all$`EXP World Holding`[1057] <- 7.655
comp_all$`EXP World Holding`[1058] <- 7.695
comp_all$`EXP World Holding`[1061] <- 8.425
comp_all$`EXP World Holding`[1062] <- 7.935
comp_all$`EXP World Holding`[1063] <- 9.195
comp_all$`EXP World Holding`[1064] <- 9.195
comp_all$`EXP World Holding`[1066] <- 9.195
comp_all$`EXP World Holding`[1067] <- 9.195
comp_all$`EXP World Holding`[1068] <- 9.555
comp_all$`EXP World Holding`[1071] <- 8.985
comp_all$`EXP World Holding`[1072] <- 8.885
comp_all$`EXP World Holding`[1073] <- 8.685
comp_all$`EXP World Holding`[1077] <- 8.075
comp_all$`EXP World Holding`[1078] <- 7.705
comp_all$`EXP World Holding`[1079] <- 7.755
comp_all$`EXP World Holding`[1080] <- 7.965
comp_all$`EXP World Holding`[1081] <- 7.805
comp_all$`EXP World Holding`[1087] <- 7.925
comp_all$`EXP World Holding`[1089] <- 7.975
comp_all$`EXP World Holding`[1090] <- 7.825
comp_all$`EXP World Holding`[1093] <- 8.145
comp_all$`EXP World Holding`[1094] <- 8.025
comp_all$`EXP World Holding`[1096] <- 8.535
comp_all$`EXP World Holding`[1098] <- 8.325
comp_all$`EXP World Holding`[1099] <- 8.065
comp_all$`EXP World Holding`[1103] <- 7.175
comp_all$`EXP World Holding`[1105] <- 7.085
comp_all$`EXP World Holding`[1106] <- 6.655
comp_all$`EXP World Holding`[1107] <- 7.375
comp_all$`EXP World Holding`[1108] <- 7.295
comp_all$`EXP World Holding`[1109] <- 7.275
comp_all$`EXP World Holding`[1112] <- 6.945
comp_all$`EXP World Holding`[1114] <- 6.075
comp_all$`EXP World Holding`[1115] <- 5.575
comp_all$`EXP World Holding`[1117] <- 55.585
comp_all$`EXP World Holding`[1118] <- 5.555
comp_all$`EXP World Holding`[1122] <- 5.485
comp_all$`EXP World Holding`[1125] <- 5.765
comp_all$`EXP World Holding`[1128] <- 6.085
comp_all$`EXP World Holding`[1129] <- 62.855
comp_all$`EXP World Holding`[1130] <- 6.305
comp_all$`EXP World Holding`[1131] <- 6.395
comp_all$`EXP World Holding`[1132] <- 6.305
comp_all$`EXP World Holding`[1135] <- 6.585
comp_all$`EXP World Holding`[1136] <- 71.875
comp_all$`EXP World Holding`[1137] <- 7.095
comp_all$`EXP World Holding`[1141] <- 7.665
comp_all$`EXP World Holding`[1142] <- 78.825
comp_all$`EXP World Holding`[1143] <- 7.875
comp_all$`EXP World Holding`[1145] <- 7.885
comp_all$`EXP World Holding`[1149] <- 7.975
comp_all$`EXP World Holding`[1150] <- 8.615
comp_all$`EXP World Holding`[1153] <- 7.695
comp_all$`EXP World Holding`[1154] <- 9.275
comp_all$`EXP World Holding`[1157] <- 8.545
comp_all$`EXP World Holding`[1161] <- 6.505
comp_all$`EXP World Holding`[1162] <- 6.375
comp_all$`EXP World Holding`[1163] <- 6.515
comp_all$`EXP World Holding`[1165] <- 6.725
comp_all$`EXP World Holding`[1169] <- 6.845
comp_all$`EXP World Holding`[1170] <- 6.975
comp_all$`EXP World Holding`[1172] <- 6.865
comp_all$`EXP World Holding`[1174] <- 6.805
comp_all$`EXP World Holding`[1177] <- 6.495
comp_all$`EXP World Holding`[1178] <- 5.875
comp_all$`EXP World Holding`[1179] <- 5.925
comp_all$`EXP World Holding`[1180] <- 6.055
comp_all$`EXP World Holding`[1187] <- 6.075
comp_all$`EXP World Holding`[1188] <- 5.635
comp_all$`EXP World Holding`[1192] <- 5.875
comp_all$`EXP World Holding`[1195] <- 5.135
comp_all$`EXP World Holding`[1199] <- 60.725
comp_all$`EXP World Holding`[1203] <- 6.325
comp_all$`EXP World Holding`[1208] <- 6.402
comp_all$`EXP World Holding`[1210] <- 6.395
comp_all$`EXP World Holding`[1211] <- 6.445
comp_all$`EXP World Holding`[1213] <- 6.375
comp_all$`EXP World Holding`[1217] <- 6.425
comp_all$`EXP World Holding`[1221] <- 6.185
comp_all$`EXP World Holding`[1222] <- 6.225
comp_all$`EXP World Holding`[1223] <- 6.075
comp_all$`EXP World Holding`[1224] <- 6.085
comp_all$`EXP World Holding`[1226] <- 4.975
comp_all$`EXP World Holding`[1227] <- 5.905
comp_all$`EXP World Holding`[1229] <- 6.725
comp_all$`EXP World Holding`[1233] <- 7.275
comp_all$`EXP World Holding`[1234] <- 6.925
comp_all$`EXP World Holding`[1237] <- 6.825
comp_all$`EXP World Holding`[1238] <- 8.475
comp_all$`EXP World Holding`[1240] <- 7.475
comp_all$`EXP World Holding`[1242] <- 5.975
comp_all$`EXP World Holding`[1247] <- 4.007
comp_all$`EXP World Holding`[1255] <- 3.636
comp_all$`EXP World Holding`[1256] <- 3.725
comp_all$`EXP World Holding`[1260] <- 3.975
comp_all$`EXP World Holding`[1262] <- 3.875
comp_all$`EXP World Holding`[1265] <- 3.945
comp_all$`EXP World Holding`[1268] <- 3.945
comp_all$`EXP World Holding`[1269] <- 3.995

comp_all$Paltalk[7] <- 1.5146
comp_all$Paltalk[9] <- 1.5193
comp_all$Paltalk[17] <- 1.4515
comp_all$Paltalk[49] <- 1.4550
comp_all$Paltalk[58] <- 1.525
comp_all$Paltalk[75] <- 1.7499
comp_all$Paltalk[98] <- 1.9001
comp_all$Paltalk[133] <- 1.945
comp_all$Paltalk[255] <- 2.985
comp_all$Paltalk[312] <- 2.9272
comp_all$Paltalk[327] <- 3.6168
comp_all$Paltalk[334] <- 3.585
comp_all$Paltalk[337] <- 3.415
comp_all$Paltalk[338] <- 3.3216
comp_all$Paltalk[340] <- 3.3999
comp_all$Paltalk[341] <- 3.4429
comp_all$Paltalk[342] <- 3.3903
comp_all$Paltalk[345] <- 3.4159
comp_all$Paltalk[346] <- 3.4257
comp_all$Paltalk[348] <- 3.4901
comp_all$Paltalk[352] <- 3.235
comp_all$Paltalk[362] <- 3.155
comp_all$Paltalk[367] <- 3.175
comp_all$Paltalk[371] <- 3.205
comp_all$Paltalk[375] <- 2.945
comp_all$Paltalk[377] <- 3.1525
comp_all$Paltalk[388] <- 2.715
comp_all$Paltalk[396] <- 2.875
comp_all$Paltalk[404] <- 2.905
comp_all$Paltalk[442] <- 2.121
comp_all$Paltalk[469] <- 1.7705
comp_all$Paltalk[503] <- 1.225
comp_all$Paltalk[505] <- 1.255
comp_all$Paltalk[508] <- 1.145
comp_all$Paltalk[545] <- 1.125
comp_all$Paltalk[566] <- 1.3441
comp_all$Paltalk[582] <- 1.1955
comp_all$Paltalk[642] <- 1.1199
comp_all$Paltalk[759] <- 3.175
comp_all$Paltalk[821] <- 3.4275
comp_all$Paltalk[903] <- 5.365
comp_all$Paltalk[1044] <- 6.314

comp_all$`Nextdoor Holding`[371] <-9.9402
comp_all$`Nextdoor Holding`[378] <- 9.9501
comp_all$`Nextdoor Holding`[428] <- 10.2399
comp_all$`Nextdoor Holding`[435] <- 10.005

# EXPORT CLEAN DATA ===================
write.csv(comp_all, paste0(dir_df,"IndustryCompetitors_clean.csv"), row.names = FALSE)
