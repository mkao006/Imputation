########################################################################
## Title: This script performs the necessary data manipulation for
##        imputation.
## Date: 2013-04-26
########################################################################

useNAAreaGrp = FALSE
skipMissingGrp = TRUE
check = FALSE

library(quantreg)
library(plyr)
library(reshape2)
library(XML)
library(FAOSYB)
library(reshape2)
library(zoo)
library(nlme)


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
                  c(351, 296, 357, 76, 245, 246, 246, 43, 298), "FAO_TABLE_NAME"]

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
process.df = merge(process.df, FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")],
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
                                    "FAOST_CODE"]), "FAO_TABLE_NAME"]

FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(process.df[is.na(process.df$UNSD_SUB_REG),
                                    "FAOST_CODE"]), "FAO_TABLE_NAME"]

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



## Fix conflicting error, it is impossible for harvested area to be
## zero when production is non-zero and vice versa. The solution is to
## replace the zero for these entries with NA and try to impute them.
process.df[which(process.df$valueArea == 0 & process.df$valueProd != 0),
           "valueArea"] = NA
process.df[which(process.df$valueArea != 0 & process.df$valueProd == 0),
           "valueProd"] = NA

## A function to compute yield to account for zeros in area and
## production. To account fo identification problem, we compute yield
## as NA if one of area or production is zero or NA.
computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  ifelse(production != 0 & area != 0, production/area, NA)
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
process.dt = rmNA.dt[c(!allMiss & includeGroup), ]
process.dt[, includeGroup := NULL]
process.dt[, allMiss := NULL]
setkeyv(process.dt, c("FAOST_CODE", "itemCode", "Year"))

## Create the final data frame for imputation
final.dt = process.dt[, list(FAOST_CODE, UNSD_MACRO_REG,
  UNSD_SUB_REG, itemCode, Year, symbArea, symbProd, ovalueArea,
  ovalueProd, valueArea, valueProd, valueYield)]


official.dt = final.dt[symbArea %in% c("", "*") & symbProd %in% c("", "*"), ]
