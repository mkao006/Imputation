########################################################################
## Title: The test version of the full imputation
## Date: 2013-04-16
########################################################################

useInterpolation = FALSE
check = FALSE
## Validation can be none, old, or new
validateChange = "new"
validateAbs = "none"
useNAAreaGrp = FALSE
skipMissingGrp = TRUE

library(quantreg)
library(plyr)
library(reshape2)
library(XML)
library(FAOSYB)
library(reshape2)
library(zoo)



## Commodity group data take from the SAS script
##---------------------------------------------------------------------

tmp = c(cereals = c(15, 27, 44, 75, 56, 71, 79, 83, 89, 101, 92, 94,
                    97, 103, 108),
        pulses = c(176, 187, 191, 195, 201, 181, 197, 210, 205,
                   203, 211),
        roots_tub = c(125, 116, 122, 137, 135, 136, 149),
        treenuts = c(221, 223, 217, 225, 222, 220, 224, 216, 226, 234),
        oilcrops = c(236, 242, 260, 249, 277, 305, 310, 256, 263, 265,
                     267, 270, 275, 280, 289, 292, 296, 299, 328, 329,
                     333, 336, 339),
        veg = c(388, 402, 403, 406, 426, 407, 393, 358, 372, 397, 417,
                414, 423, 420, 366, 367, 399, 449, 401, 373, 394, 446,
                430, 378, 567, 568, 463),
        fruit = c(486, 489, 577, 569, 574, 572, 571, 603, 490, 495, 507,
                  497, 512, 560, 600, 515, 521, 523, 526, 530, 531, 534,
                  536, 544, 547, 552, 554, 558, 550, 549, 592, 587, 542,
                  541, 591, 619),
        stim_beve = c(656, 661, 667, 671, 674),
        spices = c(687, 689, 692, 693, 698, 702, 711, 720, 723),
        tobacco = c(826),
        othr_crops = c(459, 461, 677),
        rubber = c(837, 836),
        leaves = c(748, 754),
        fibres = c(767, 780, 788, 800, 809, 821, 782, 987, 773, 1185, 789),
        othr_meat = c(867, 947, 1035, 977, 1017, 1127, 1097, 1108, 1111,
                      1163, 1166),
        poultry_meat = c(1058, 1069, 1073, 1080, 1141),
        milk = c(882, 951, 982, 1020, 1130), eggs = c(1062, 1091),
        lvstk_prds = c(886, 901),
        sugar = c(162, 163, 1182),
        veg_oils = c(36, 60, 237, 244, 257, 258, 252, 261, 264, 266, 268,
          271, 276, 281, 290, 293, 297, 331, 334, 337, 340, 1242, 278, 307, 313),
        alch_bev = c(564, 51, 66, 82, 86)
  )

## Process the commodity group table and create groups which should
## not be imputed
comGrp.df = melt(tmp)
comGrp.df$Group = gsub("[0-9]", "", rownames(comGrp.df))
comGrp.df$includeGroup = ifelse(comGrp.df$Group %in% c("lvstk_prds", "sugar",
  "veg_oils", "alch_bev"), FALSE, TRUE)
row.names(comGrp.df) = NULL
comGrp.dt = data.table(comGrp.df)
rm(comGrp.df)
setnames(comGrp.dt, old = colnames(comGrp.dt),
         new = c("itemCode", "comGroup", "includeGroup"))

## Extract the production commodity group from the FAOSTAT 2 website.
## doc = htmlParse("http://faostat.fao.org/site/384/default.aspx")
## test = getNodeSet(doc, path = "//table")
## prodTable.df = readHTMLTable(test[[10]],
##   header = c("Group Name", "Item FAO Code", "Item HS+ Code", "Item Name",
##     "Definition"), stringsAsFactors = FALSE, skip.rows = 1)
## tmp = prodTable.df[, c("Group Name", "Item FAO Code", "Item Name")]



## Read and process the test data
## ---------------------------------------------------------------------
raw.dt = data.table(read.csv("nicola_ws_request_12yrs.csv", header = TRUE,
    stringsAsFactors = FALSE))
setnames(raw.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
         new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## Remove test area and excluded countries 
raw.dt = subset(raw.dt, !(FAOST_CODE %in% c(351, 296, 357, 76, 245, 246, 246, 43,
    298)))
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  c(351, 296, 357, 76, 245, 246, 246, 43, 298), "LAB_NAME"]

## Not sure what this column is used, and it is a mixture of numeric
## and strings.
raw.dt$itemGroup = NULL


## Manipulate the data before processing
mraw.df = melt(raw.dt, c("FAOST_CODE", "itemCode", "elementCode"))
mraw.df$type = ifelse(grepl("F", mraw.df$variable), "symb", "value")
mraw.df$variable = as.numeric(gsub("[^0-9]", "", mraw.df$variable))
cmraw.df = dcast(mraw.df, FAOST_CODE + itemCode + variable ~ type + elementCode)
colnames(cmraw.df) = c("FAOST_CODE", "itemCode", "Year",
        "symbArea", "symbProd", "valueArea", "valueProd")
cmraw.df$valueProd = as.numeric(cmraw.df$valueProd)
cmraw.df$valueArea = as.numeric(cmraw.df$valueArea)
cmraw.df$symbArea = gsub("[[:space:]]", "", cmraw.df$symbArea)
cmraw.df$symbProd = gsub("[[:space:]]", "", cmraw.df$symbProd)


## Create the final data frame with country and regional codes and
## names and also the commodity group.
process.df = merge(cmraw.df, comGrp.dt, all.x = TRUE)

## These items does not have commodity group from the file given by Hans.
sort(unique(process.df[is.na(process.df$comGroup), "itemCode"]))
unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode %in%
                              sort(unique(process.df[is.na(process.df$comGroup),
                                                     "itemCode"])),
                              c("itemCode", "itemName")])

if(skipMissingGrp)
  process.df = process.df[!is.na(process.df$comGroup), ]

    
## Merge with regional and subregional
process.df = merge(process.df, FAOcountryProfile[, c("FAOST_CODE", "LAB_NAME")],
  all.x = TRUE)
process.df = arrange(merge(process.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE",
  "UNSD_MACRO_REG_CODE")], all.x = TRUE), FAOST_CODE, itemCode, Year)
## This is a hack for the region until Fillipo fixes the countryprofile.
process.df = merge(process.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_MACRO_REG_CODE", "UNSD_MACRO_REG")])),
  all.x = TRUE)
process.df = merge(process.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_SUB_REG_CODE", "UNSD_SUB_REG")])),
  all.x = TRUE)
process.df$UNSD_SUB_REG_CODE = NULL
process.df$UNSD_MACRO_REG_CODE = NULL

## These countries does not have region or subregion available.
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(process.df[is.na(process.df$UNSD_MACRO_REG),
                                    "FAOST_CODE"]), "LAB_NAME"]

FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(process.df[is.na(process.df$UNSD_SUB_REG),
                                    "FAOST_CODE"]), "LAB_NAME"]

if(skipMissingGrp)
  process.df = process.df[!is.na(process.df$UNSD_MACRO_REG) &
    !is.na(process.df$UNSD_SUB_REG),]

## Save the original value
process.df$ovalueArea = process.df$valueArea
process.df$ovalueProd = process.df$valueProd

## Replace values with symbol T or duplicated F with NA
process.df[which(process.df$symbArea == "T"), "valueArea"] = NA
process.df[which(process.df$symbProd == "T"), "valueProd"] = NA
process.df[which(process.df$symbArea == "E"), "valueArea"] = NA
process.df[which(process.df$symbProd == "E"), "valueProd"] = NA
process.df[which(duplicated(process.df[, c("FAOST_CODE", "itemCode", "symbArea",
                                 "valueArea")]) & process.df$symbArea == "F"),
         "valueArea"] = NA
process.df[which(duplicated(process.df[, c("FAOST_CODE", "itemCode", "symbProd",
                                 "valueProd")]) & process.df$symbProd == "F"),
         "valueProd"] = NA


## A function to compute yield to account for zeros in area and
## production. To account fo identification problem, we compute yield
## as NA if one of area or production is zero or NA.
computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  yield = ifelse(production != 0 & area != 0, production/area, NA)
}

process.df$valueYield = with(process.df, computeYield(valueProd, valueArea))
process.df$ovalueYield = with(process.df, computeYield(ovalueProd, ovalueArea))

## Removing blocks which contains only NA's or zero. 
process.dt = data.table(process.df)
tmp = process.dt[, sum(valueArea, valueProd, na.rm = TRUE),
  by = c("FAOST_CODE", "itemCode")]
tmp[, allMiss := V1 == 0 | is.na(V1)]
tmp$V1 = NULL
rmNA.dt = merge(process.dt, tmp, by = c("FAOST_CODE", "itemCode"))

## Fix conflicting error, it is impossible for harvested area to be
## zero when production is non-zero and vice versa. The solution is to
## replace the zero for these entries with NA and try to impute them.
rmNA.dt[valueArea == 0 & valueProd != 0, valueArea := as.numeric(NA)]
rmNA.dt[valueArea != 0 & valueProd == 0, valueProd := as.numeric(NA)]


## These item are removed if we specify FALSE for useNAAreaGrp, this
## is because they don't have area data on area.
if(!useNAAreaGrp)
  rmNA.dt = rmNA.dt[(!comGroup %in% c("milk", "poultry_meat", "othr_meat"))]

## Function to check the sparsity of the data
checkSparsity = function(Data){
  image(data.matrix(is.na(Data)))
  text(rep(0.5, NCOL(Data)), seq(0, 1, length = NCOL(Data)),
       labels = paste(colnames(Data), " (", round(sapply(X = Data,
         FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))
}

## Remove all missing group and commodity group that does not require
## imputation
final.dt = rmNA.dt[c(!allMiss & includeGroup), ]
final.dt[, includeGroup := NULL]
final.dt[, allMiss := NULL]
setkeyv(final.dt, c("FAOST_CODE", "itemCode", "Year"))
if(check)
  checkSparsity(final.dt)

## symbol by commodity group
with(final.dt, table(symbArea, comGroup, useNA = "ifany")),
with(final.dt, table(symbProd, comGroup, useNA = "ifany"))
round(with(final.dt, table(symbArea, useNA = "ifany"))/NROW(final.dt), 2)
round(with(final.dt, table(symbProd, useNA = "ifany"))/NROW(final.dt), 2)


com = 15
tmp = subset(final.dt, itemCode == com & symbArea %in% c("", "*") &
  symbProd %in% c("", "*"))
plot.new()
plot.window(xlim = range(tmp$Year), ylim = range(tmp$valueYield, na.rm = TRUE))
for(i in unique(tmp$FAOST_CODE)){
  lines(tmp[FAOST_CODE == i, Year], tmp[FAOST_CODE == i, valueYield])
}
axis(1)
axis(2)


com = "cereals"
tmp = subset(final.dt, comGroup == com & symbArea %in% c("", "*") &
  symbProd %in% c("", "*"))
keys = unique(tmp[, list(FAOST_CODE, itemCode)])
plot.new()
plot.window(xlim = range(tmp$Year), ylim = range(tmp$valueYield, na.rm = TRUE))
for(i in 1:NROW(keys)){
  lines(tmp[FAOST_CODE == keys[i, FAOST_CODE] & itemCode == keys[i, itemCode],
            Year],
        tmp[FAOST_CODE == keys[i, FAOST_CODE] & itemCode == keys[i, itemCode],
            valueYield])
}
axis(1)
axis(2)




## Begin imputation
## ---------------------------------------------------------------------

## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x)(
  }
  tmp
}
  
## This is a test of how the linear interpolation would work
if(useInterpolation){
  final.dt[, valueArea := na.approx2(valueArea, na.rm = FALSE),
           by = c("FAOST_CODE", "itemCode")]
  final.dt[, valueProd := na.approx2(valueProd, na.rm = FALSE),
           by = c("FAOST_CODE", "itemCode")]
  if(check)
    checkSparsity(final.dt)
}



## Function for computing the year to year ratio
diffv = function(x){
    T = length(x)
    if(sum(!is.na(x)) >= 2){
      tmp = c(x[2:T]/x[1:(T - 1)])
    } else {
      tmp = rep(NA, length(x) - 1)
    }
    tmp
}


groupCh = function(Data, primary_key, group_key, value, check = FALSE,
  grpName){
  if(missing(grpName))
    grpName = paste(group_key, collapse = "_")
  setnames(Data, old = value, new = "value")
  Data[, grpSum := sum(value, na.rm = !all(is.na(value))), by = group_key]
  Data[, grpGr := as.numeric(c(NA, diffv(grpSum))), by = primary_key]
  if(!check){
    Data[, grpSum := NULL]
    setnames(Data, old = c("value", "grpGr"),
             new = c(value, paste(value, "_", grpName, "_gr", sep = "")))
  } else {
    setnames(Data, old = c("value", "grpSum", "grpGr"),
             new = c(value, paste(value, "_", grpName, "_sum", sep = ""),
               paste(value, "_", grpName, "_gr", sep = "")))
  }
}


## Compute the raw data growth
final.dt[, valueArea_raw_gr := as.numeric(c(NA, diffv(valueArea))),
         by = c("FAOST_CODE", "itemCode")]
final.dt[, valueProd_raw_gr := as.numeric(c(NA, diffv(valueProd))),
         by = c("FAOST_CODE", "itemCode")]
if(check)
  checkSparsity(final.dt)

## Compute the country commodity group
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "comGroup", "Year"), value = "valueArea",
    grpName = "countryCom")
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "comGroup", "Year"), value = "valueProd",
    grpName = "countryCom")

## Compute the subregion item group
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "itemCode", "Year"), value = "valueArea",
    grpName = "subregItem")
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "itemCode", "Year"), value = "valueProd",
    grpName = "subregItem")

## Compute the subregional commodity group
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "comGroup", "Year"), value = "valueArea",
    grpName = "subregCom")
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "comGroup", "Year"), value = "valueProd",
    grpName = "subregCom")

## Compute the region item group
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "itemCode", "Year"), value = "valueArea",
    grpName = "regItem")
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "itemCode", "Year"), value = "valueProd",
    grpName = "regItem")

## Compute the region commodity group
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "comGroup", "Year"), value = "valueArea",
    grpName = "regCom")
groupCh(Data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "comGroup", "Year"), value = "valueProd",
    grpName = "regCom")

if(check)
  checkSparsity(final.dt)


## examining the commodity group, these data have no growth rate, the
## biggest group is other and poultry meat then milk. These clearly has no area
## harvested.
table(final.dt[is.na(valueArea_regCom_gr), comGroup])


## Compute the change in yield for all changes in production and area
yield.dt = final.dt[, grep("valueProd_", colnames(final.dt), value = TRUE),
  with = FALSE]/final.dt[, grep("valueArea_", colnames(final.dt), value = TRUE),
    with = FALSE]
setnames(yield.dt, old = colnames(yield.dt),
         new = gsub("valueProd", "valueYield", colnames(yield.dt)))
full.dt = cbind(final.dt, yield.dt)

## Check sparsity by chunk
if(check){
  pdf(file = "sparsityCheckAfterImp.pdf", width = 10)
  tmp = quantile(c(1, NROW(full.dt)), probs = seq(0, 1, length = 20))
  for(i in 1:(length(tmp) - 1)){
    checkSparsity(full.dt[tmp[i]:tmp[i + 1], ])
  }
  graphics.off()
  system("evince sparsityCheckAfterImp.pdf&")
}


## Explore the relationship between the raw change and the imputed change
## ---------------------------------------------------------------------

## NOTE (Michael): Some of the plots can not be generated because
##                 either all area or production was missing.
if(check){
  area.dt = subset(full.dt, select = c("FAOST_CODE", "itemCode", "Year",
                              grep("valueArea_", colnames(full.dt),
                                   value = TRUE)))
  marea.df = melt(area.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))
  areaKey = unique(marea.df[, c("FAOST_CODE", "itemCode")])
  marea.df$variable = factor(gsub("valueArea_", "", marea.df$variable),
    levels = c("raw_gr", "countryCom_gr", "subregItem_gr", "subregCom_gr",
      "regItem_gr", "regCom_gr"))
  pdf(file = "areaImpCheck.pdf", width = 10)
  set.seed(587)
  for(i in sample(1:NROW(areaKey), 1000)){
    try(print(ggplot(data = marea.df[marea.df$FAOST_CODE ==
                       areaKey[i, "FAOST_CODE"]
                       & marea.df$itemCode == areaKey[i, "itemCode"], ],
                     aes(x = Year, y = value)) +
              geom_line(aes(col = variable)) +
              labs(title = paste(unique(with(FAOmetaTable,
                     itemTable[itemTable$itemCode == areaKey[i, "itemCode"],
                               "itemName"])), "(", areaKey[i, "itemCode"], ") - ",
                     FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                             areaKey[i, "FAOST_CODE"]),
                                       "LAB_NAME"], sep = ""), x = NULL, y = NULL)
              ))
  }
  graphics.off()
  system("evince areaImpCheck.pdf&")
  
  
  prod.dt = subset(full.dt, select = c("FAOST_CODE", "itemCode", "Year",
                              grep("valueProd_", colnames(full.dt),
                                   value = TRUE)))
  mprod.df = melt(prod.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))
  prodKey = unique(mprod.df[, c("FAOST_CODE", "itemCode")])
  mprod.df$variable = factor(gsub("valueProd_", "", mprod.df$variable),
    levels = c("raw_gr", "countryCom_gr", "subregItem_gr", "subregCom_gr",
      "regItem_gr", "regCom_gr"))
  pdf(file = "prodImpCheck.pdf", width = 10)
  set.seed(587)
  for(i in sample(1:NROW(prodKey), 50)){
    try(print(ggplot(data = mprod.df[mprod.df$FAOST_CODE ==
                       prodKey[i, "FAOST_CODE"] &
                       mprod.df$itemCode == prodKey[i, "itemCode"], ],
                     aes(x = Year, y = value)) +
              geom_line(aes(col = variable)) +
              labs(title = paste(unique(with(FAOmetaTable,
                     itemTable[itemTable$itemCode == prodKey[i, "itemCode"],
                               "itemName"])), "(", prodKey[i, "itemCode"], ") - ",
                     FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                             prodKey[i, "FAOST_CODE"]),
                                       "LAB_NAME"], sep = ""), x = NULL, y = NULL)
              ))
  }
  graphics.off()
  system("evince prodImpCheck.pdf&")
  
  
  yield.dt = subset(full.dt, select = c("FAOST_CODE", "itemCode", "Year",
                               grep("valueYield_", colnames(full.dt),
                                    value = TRUE)))
  myield.df = melt(yield.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))
  yieldKey = unique(myield.df[, c("FAOST_CODE", "itemCode")])
  myield.df$variable = factor(gsub("valueYield_", "", myield.df$variable),
    levels = c("raw_gr", "countryCom_gr", "subregItem_gr", "subregCom_gr",
      "regItem_gr", "regCom_gr"))
  pdf(file = "yieldImpCheck.pdf", width = 10)
  set.seed(587)
  for(i in sample(1:NROW(yieldKey), 50)){
    try(print(ggplot(data = myield.df[myield.df$FAOST_CODE ==
                       yieldKey[i, "FAOST_CODE"] &
                       myield.df$itemCode == yieldKey[i, "itemCode"], ],
                     aes(x = Year, y = value)) +
              geom_line(aes(col = variable)) +
              labs(title = paste(unique(with(FAOmetaTable,
                     itemTable[itemTable$itemCode == yieldKey[i, "itemCode"],
                               "itemName"])), "(", yieldKey[i, "itemCode"], ") - ",
                     FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                             yieldKey[i, "FAOST_CODE"]),
                                       "LAB_NAME"], sep = ""), x = NULL, y = NULL)
              ))
  }
  graphics.off()
  system("evince yieldImpCheck.pdf&")
}
## ---------------------------------------------------------------------





if(validateChange == "new"){
  ## full.dt[, validAbsLowerBound := predict(rq(valueYield_raw_gr ~ Year, tau = 0.05),
  ##             data.frame(Year)), by = "itemCode"]
  ## full.dt[, validAbsUpperBound := predict(rq(valueYield_raw_gr ~ Year, tau = 0.95),
  ##             data.frame(Year)), by = "itemCode"]  
  
  ## ## compute the validation bound of change in yield
  ## full.dt[, validChLowerBound := quantile(valueYield_raw_gr, probs = 0.25,
  ##             na.rm = TRUE) - (quantile(valueYield_raw_gr, probs = 0.5,
  ##               na.rm = TRUE) - quantile(valueYield_raw_gr, probs = 0.25,
  ##                 na.rm = TRUE)), by = "itemCode"]
  ## full.dt[, validChUpperBound := quantile(valueYield_raw_gr, probs = 0.75,
  ##             na.rm = TRUE) + (quantile(valueYield_raw_gr, probs = 0.75,
  ##               na.rm = TRUE) - quantile(valueYield_raw_gr, probs = 0.5,
  ##                 na.rm = TRUE)), by = "itemCode"]
  
  ## compute the validation bound of change in yield
  full.dt[, validChLowerBound := quantile(valueYield_raw_gr, probs = 0.05,
              na.rm = TRUE), by = "itemCode"]
  full.dt[, validChUpperBound := quantile(valueYield_raw_gr, probs = 0.95,
              na.rm = TRUE), by = "itemCode"]
  
  ## Validate the yield by change distribution
  full.dt[valueYield_countryCom_gr < validChLowerBound |
          valueYield_countryCom_gr > validChUpperBound,
          valueYield_countryCom_gr := NA]
  full.dt[valueYield_subregItem_gr < validChLowerBound |
          valueYield_subregItem_gr > validChUpperBound,
          valueYield_subregItem_gr := NA]
  full.dt[valueYield_subregCom_gr < validChLowerBound |
          valueYield_subregCom_gr > validChUpperBound,
          valueYield_subregCom_gr  := NA]
  full.dt[valueYield_regItem_gr < validChLowerBound |
          valueYield_regItem_gr > validChUpperBound,
          valueYield_regItem_gr := NA]
  full.dt[valueYield_regCom_gr  < validChLowerBound |
          valueYield_regCom_gr > validChUpperBound,
          valueYield_regCom_gr := NA]
  
} else if(validateChange == "old"){
  
  ## Validate the yield by change distribution
  full.dt[valueYield_countryCom_gr < 0.6 |
          valueYield_countryCom_gr > 1.4,
          valueYield_countryCom_gr := NA]
  full.dt[valueYield_subregItem_gr < 0.6 |
          valueYield_subregItem_gr > 1.4,
          valueYield_subregItem_gr := NA]
  full.dt[valueYield_subregCom_gr < 0.6 |
          valueYield_subregCom_gr > 1.4,
          valueYield_subregCom_gr  := NA]
  full.dt[valueYield_regItem_gr < 0.6 |
          valueYield_regItem_gr > 1.4,
          valueYield_regItem_gr := NA]
  full.dt[valueYield_regCom_gr  < 0.6 |
          valueYield_regCom_gr > 1.4,
          valueYield_regCom_gr := NA]
}



## if(validateAbs == "new"){
##     ## Compute the validation bound of yield
##   full.dt[, validAbsLowerBound := predict(rq(valueYield ~ Year, tau = 0.05),
##               data.frame(Year)), by = "itemCode"]
##   full.dt[, validAbsUpperBound := predict(rq(valueYield ~ Year, tau = 0.95),
##               data.frame(Year)), by = "itemCode"]  
  
##   ## full.dt[, validAbsLowerBound := quantile(valueYield, probs = 0.25,
##   ##             na.rm = TRUE) - (quantile(valueYield, probs = 0.5,
##   ##               na.rm = TRUE) - quantile(valueYield, probs = 0.25,
##   ##                 na.rm = TRUE)), by = c("itemCode", "Year")]
##   ## full.dt[, validAbsUpperBound := quantile(valueYield, probs = 0.75,
##   ##             na.rm = TRUE) + (quantile(valueYield, probs = 0.75,
##   ##               na.rm = TRUE) - quantile(valueYield, probs = 0.5,
##   ##                 na.rm = TRUE)), by = c("itemCode", "Year")]
    
##   ## Validate the yield by distribution
##   full.dt[valueYield_countryCom < validAbsLowerBound |
##           valueYield_countryCom > validAbsUpperBound,
##           valueYield_countryCom_gr := NA]
##   full.dt[valueYield_subregItem < validAbsLowerBound |
##           valueYield_subregItem > validAbsUpperBound,
##           valueYield_subregItem_gr := NA]
##   full.dt[valueYield_subregCom < validAbsLowerBound |
##           valueYield_subregCom > validAbsUpperBound,
##           valueYield_subregCom_gr  := NA]
##   full.dt[valueYield_regItem < validAbsLowerBound |
##           valueYield_regItem > validAbsUpperBound,
##           valueYield_regItem_gr := NA]
##   full.dt[valueYield_regCom  < validAbsLowerBound |
##           valueYield_regCom > validAbsUpperBound,
##           valueYield_regCom_gr := NA]
## }


if(check){
  pdf(file = "quantCheck.pdf")
  for(i in unique(full.dt$itemCode)){
    check.dt = full.dt[itemCode == i, ]
    ## q01 = rq(valueYield ~ Year, tau = 0.01, data = check.dt)
    q05 = rq(valueYield_raw_gr~ Year, tau = 0.05, data = check.dt)  
    ## q25 = rq(valueYield ~ Year, tau = 0.25, data = check.dt)
    ## q50 = rq(valueYield ~ Year, tau = 0.50, data = check.dt)
    ## q75 = rq(valueYield ~ Year, tau = 0.75, data = check.dt)
    q95 = rq(valueYield_raw_gr~ Year, tau = 0.95, data = check.dt)
    ## q99 = rq(valueYield ~ Year, tau = 0.99, data = check.dt)
    check.dt[, qregLowerBound := predict(q05, data.frame(Year))]
    check.dt[, qregUpperBound := predict(q95, data.frame(Year))]
    check.dt[, qLowerBound := quantile(valueYield_raw_gr, 0.05, na.rm = TRUE),
             by = c("itemCode", "Year")]
    check.dt[, qUpperBound := quantile(valueYield_raw_gr, 0.95, na.rm = TRUE),
             by = c("itemCode", "Year")]      
    ## Check the absolute validation
    try(print(ggplot(check.dt, aes(x = Year, y = valueYield_raw_gr)) +
              geom_point(aes(col = ifelse(symbArea %in% c("", "*") &
                               symbProd %in% c("", "*"),
                               "official", "non-official")), alpha = 0.3) +
              geom_line(aes(x = Year, y = qregLowerBound),
                        col = "steelblue", linetype = "dashed") +
              geom_line(aes(x = Year, y = qregUpperBound),
                        col = "steelblue", linetype = "dashed") +
              geom_line(aes(x = Year, y = qLowerBound),
                        col = "green", linetype = "dashed") +
              geom_line(aes(x = Year, y = qUpperBound),
                        col = "green", linetype = "dashed") +      
              geom_line(aes(x = Year, y = 1.4), col = "red", linetype = "dashed") +
              geom_line(aes(x = Year, y = 0.6), col = "red", linetype = "dashed") +
              labs(col = "", x = NULL,
       title = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
                     == i, "itemName"]), "(",
           unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                                   "itemCode"]), ")",  sep = ""))
              ))
  }
  graphics.off()
  system("evince quantCheck.pdf&")
}

## full.dt$isOfficial = with(full.dt, ifelse(symbArea %in% c("", "*") &
##   symbProd %in% c("", "*"), "official", "non-official"))

## pdf(file = "yieldOfficialCheck.pdf")
## for(i in unique(final.dt$itemCode)){
##   try(print(ggplot(full.dt[itemCode == i, ], aes(x = valueYield)) +
##             geom_histogram(binwidth = 0.05) +
##             facet_wrap(~isOfficial)))
## }
## graphics.off()
## system("evince yieldOfficialCheck.pdf&")



## dev.new()
## image(data.matrix(is.na(full.dt)))
## text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
##      labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
##        FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))



## Function to select between different method of imputation
selectImp = function(data, yieldCol, areaCol, prodCol){
  for(i in 1:NROW(data)){
    ind = which(!is.na(data[i, yieldCol, with = FALSE]))[1]
    if(!is.na(ind)){
      data[i, final_area_gr:= as.numeric(data[i, areaCol[ind], with = FALSE])]
      data[i, final_area_impname := areaCol[ind]]
      data[i, final_prod_gr:= as.numeric(data[i, prodCol[ind], with = FALSE])]
      data[i, final_prod_impname := prodCol[ind]]      
    } else {
      data[i, final_area_gr:= as.numeric(NA)]
      data[i, final_area_impname := as.character(NA)]
      data[i, final_prod_gr:= as.numeric(NA)]      
      data[i, final_prod_impname := as.character(NA)]
    }
  }
}


selectImp(data = full.dt, yieldCol = c("valueYield_countryCom_gr",
                            "valueYield_subregItem_gr",
                            "valueYield_subregCom_gr", "valueYield_regItem_gr",
                            "valueYield_regCom_gr"),
          areaCol = c("valueArea_countryCom_gr", "valueArea_subregItem_gr",
            "valueArea_subregCom_gr", "valueArea_regItem_gr",
            "valueArea_regCom_gr"),
          prodCol = c("valueProd_countryCom_gr", "valueProd_subregItem_gr",
            "valueProd_subregCom_gr", "valueProd_regItem_gr",
            "valueProd_regCom_gr"))


## Imputation by methodology
table(full.dt[is.na(valueArea), final_area_impname], useNA = "ifany")
table(full.dt[is.na(valueProd), final_area_impname], useNA = "ifany")

if(check)
  checkSparsity(full.dt)


## Carry the final imputation
imp = function(value, gr){
  if(all(is.na(value) | value == 0)){
    tmp = value
  } else {
    n = length(value)
    firstObs = which(!is.na(value) & value != 0)[1]
    tmp = double(n)
    tmp[0:(firstObs - 1)] = NA
    for(i in firstObs:n){
      if(is.na(value[i])){
        tmp[i] = tmp[i - 1] * gr[i]
      } else {
        tmp[i] = value[i] * gr[i]
      }
    }
  }
  tmp
}

full.dt[, final_area_imp := c(NA, imp(valueArea[-length(valueArea)],
            final_area_gr[-1])), by = c("FAOST_CODE", "itemCode")]

full.dt[, final_prod_imp := c(NA, imp(valueProd[-length(valueProd)],
            final_prod_gr[-1])), by = c("FAOST_CODE", "itemCode")]



## Check the imputation
## ---------------------------------------------------------------------

if(check)
  checkSparsity(full.dt)

full.dt[, imputedArea := valueArea]
full.dt[, imputedProd := valueProd]

full.dt[is.na(imputedArea), imputedArea := final_area_imp]
full.dt[is.na(imputedProd), imputedProd := final_prod_imp]
full.dt[, impliedYield := computeYield(imputedProd, imputedArea)]

## Proportion of missing value imputed
NROW(full.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(full.dt[is.na(valueArea), ])

NROW(full.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(full.dt[is.na(valueProd), ])


check.dt = full.dt[, list(FAOST_CODE, itemCode, Year, valueArea, valueProd,
  valueYield, imputedArea, imputedProd, impliedYield)]
keys = unique(check.dt[(is.na(valueArea) & !is.na(imputedArea)) |
  (is.na(valueProd) & !is.na(imputedProd)), list(FAOST_CODE, itemCode)])
mcheck.df = melt(check.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))


pdf(file = "checkImputation.pdf")
for(i in 1:NROW(keys)){
  try(print(ggplot(mcheck.df[mcheck.df$FAOST_CODE == keys[i, FAOST_CODE] &
                             mcheck.df$itemCode == keys[i, itemCode], ],
                   aes(x = Year, y = log(value))) +
            geom_line(aes(col = variable)) +
            geom_point(aes(col = variable), alpha = 0.3, size = 3) +
            scale_color_manual(values = c(rgb(0, 0, 0, alpha = c(0.3, 0.4, 0.5)),
                                 rgb(218, 165, 32, alpha = c(0.3, 0.4, 0.5),
                                     maxColorValue = 255))) +
            labs(x = NULL,
    title = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
      == keys[i, itemCode], "itemName"]), "(",
      unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == keys[i,
                                      itemCode], "itemCode"]), "), ",
      FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == keys[i, FAOST_CODE]),
                     "LAB_NAME"], sep = ""))))
}
graphics.off()
system("evince checkImputation.pdf&")











base = full.dt[, list(FAOST_CODE, itemCode, Year, valueArea, valueProd,
  imputedArea, imputedProd, impliedYield)]
setnames(base, old = c("imputedArea", "imputedProd", "impliedYield"),
         new = c("imputedNewArea", "imputedNewProd", "impliedNewYield"))
## load("noValidation")
## setnames(tmp, old = colnames(tmp), new = c("FAOST_CODE", "itemCode", "Year",
##                                      "imputedNoArea", "imputedNoProd",
##                                      "impliedNoYield"))
## test = merge(base, tmp, by = c("FAOST_CODE", "itemCode", "Year"))
load("oldValidation")
setnames(tmp, old = colnames(tmp), new = c("FAOST_CODE", "itemCode", "Year",
                                     "imputedOldArea", "imputedOldProd",
                                     "impliedOldYield"))
test = merge(test, base, by = c("FAOST_CODE", "itemCode", "Year"))
test[, needImpute := is.na(sum(valueArea, valueProd)),
     by = c("FAOST_CODE", "itemCode")]
test2 = test[test$needImpute, ]
test2[, needImpute := NULL]
test2[!is.na(valueArea), grep("^imp.+Area", colnames(test2), value = TRUE) := NA]
test2[!is.na(valueProd), grep("^imp.+Prod", colnames(test2), value = TRUE) := NA]
test2[, valueYield := computeYield(valueProd, valueArea), ]
test2[!is.na(valueYield),
      grep("^imp.+Yield", colnames(test2), value = TRUE) := NA]
mtest = melt(test2, id.var = c("FAOST_CODE", "itemCode", "Year"))
mtest$type = ifelse(grepl("Area", mtest$variable), "Area",
  ifelse(grepl("Prod", mtest$variable), "Prod", "Yield"))


keys = data.table(unique(mtest[, c("FAOST_CODE", "itemCode")]))
pdf(file = "imputationCheck.pdf")
set.seed(587)
for(i in sample(1:NROW(keys), 300)){
  plotTmp = subset(mtest, FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode])
  try(print(ggplot(plotTmp[plotTmp$variable %in%
                           c("valueArea", "valueProd", "valueYield"), ],
                           aes(x = Year, y = value)) +
            geom_line() + geom_point(alpha = 0.3) + 
            geom_point(data = plotTmp[!(plotTmp$variable %in%
                           c("valueArea", "valueProd", "valueYield")), ],
                       aes(col = variable), alpha = 0.5) + 
      facet_wrap(~type, ncol = 1, scales = "free_y") +
            scale_color_manual(values =  rep(c("steelblue", "green", "red"),
                                 each = 3)) + 
            labs(x = NULL,
    title = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode
      == keys[i, itemCode], "itemName"]), "(",
      unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == keys[i,
                                      itemCode], "itemCode"]), "), ",
      FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == keys[i, FAOST_CODE]),
                     "LAB_NAME"], sep = ""))
            ))
}
graphics.off()  
system("evince imputationCheck.pdf&")


if(check)
  checkSparsity(full.dt)

## check.dt = full.dt[, list(FAOST_CODE, itemCode, Year, valueArea, valueProd,
##   valueYield, ovalueArea, ovalueProd, ovalueYield, imputedArea, imputedProd,
##   impliedYield)]


## with(full.dt[is.na(valueArea), ], plot(ovalueArea, imputedArea))
## abline(a = 0, b = 1, col = "red")
## with(full.dt[is.na(valueProd), ], plot(ovalueProd, imputedProd))
## abline(a = 0, b = 1, col = "red")
