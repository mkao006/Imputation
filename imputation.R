########################################################################
## Title: The stable version of the imputation
## Date: 2013-04-16
########################################################################

useInterpolation = TRUE

library(plyr)
library(reshape2)
library(XML)
library(FAOSYB)
library(reshape2)
library(zoo)



## Commodity group data take from the SAS script
##---------------------------------------------------------------------

## TODO (Michael): Check which commodity group does 461 belong to
##                 (currently in fruits and other_crops), otherwise it
##                 creates duplicates. We will put it in fruits for now.
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
doc = htmlParse("http://faostat.fao.org/site/384/default.aspx")
test = getNodeSet(doc, path = "//table")
prodTable.df = readHTMLTable(test[[10]],
  header = c("Group Name", "Item FAO Code", "Item HS+ Code", "Item Name",
    "Definition"), stringsAsFactors = FALSE, skip.rows = 1)
tmp = prodTable.df[, c("Group Name", "Item FAO Code", "Item Name")]



## Read and process the test data
## ---------------------------------------------------------------------
raw.dt = data.table(read.csv("nicola_ws_request_12yrs.csv", header = TRUE,
    stringsAsFactors = FALSE))
setnames(raw.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
         new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## Remove test area and excluded countries 
raw.dt = subset(raw.dt, !(FAOST_CODE %in% c(351, 296, 357, 76, 245, 246, 246, 43,
    298)))
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
final.df = merge(cmraw.df, comGrp.dt, all.x = TRUE)
final.df = merge(final.df, FAOcountryProfile[, c("FAOST_CODE", "LAB_NAME")],
  all.x = TRUE)
final.df = arrange(merge(final.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG",
  "UNSD_MACRO_REG")], all.x = TRUE), FAOST_CODE, itemCode, Year)

## Save the original value
final.df$ovalueArea = final.df$valueArea
final.df$ovalueProd = final.df$valueProd

## Replace values with symbol T or duplicated F with NA
final.df[which(final.df$symbArea == "T"), "valueArea"] = NA
final.df[which(final.df$symbProd == "T"), "valueProd"] = NA
final.df[which(duplicated(final.df[, c("FAOST_CODE", "itemCode", "symbArea",
                                 "valueArea")]) & final.df$symbArea == "F"),
         "valueArea"] = NA
final.df[which(duplicated(final.df[, c("FAOST_CODE", "itemCode", "symbProd",
                                 "valueProd")]) & final.df$symbProd == "F"),
         "valueProd"] = NA
final.df$valueYield = with(final.df, valueProd/valueArea)
final.df$ovalueYield = with(final.df, ovalueProd/ovalueArea)

  


## TODO (Michael): Set the keys for the table
final.dt = data.table(final.df[which(final.df$includeGroup), ])


na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x)
  }
  tmp
}
  
## This is a test of how the linear interpolation would work
if(useInterpolation){
  final.dt[, valueArea := na.approx2(valueArea, na.rm = FALSE),
           by = c("FAOST_CODE", "itemCode")]
  final.dt[, valueProd := na.approx2(valueProd, na.rm = FALSE),
           by = c("FAOST_CODE", "itemCode")]
}

image(data.matrix(is.na(final.dt)))
text(rep(0.5, NCOL(final.dt)), seq(0, 1, length = NCOL(final.dt)),
     labels = paste(colnames(final.dt), " (", round(sapply(X = final.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))




## Function for computing the year to year ratio
diffv = function(x){
    T = length(x)
    ## Use linear interpolation for weight (this might not be suitable)
    ## x[x == 0] = NA
    if(sum(!is.na(x)) >= 2)
        ## TODO (Michael): test whether approx or na.approx is faster.
        ## x = na.approx(x)
    tmp = c(x[2:T]/x[1:(T - 1)])
    tmp
}


foo = function(data, primary_key, group_key, value, check = FALSE,
  grpName){
  if(missing(grpName))
    grpName = paste(group_key, collapse = "_")
  setnames(data, old = value, new = "value")
  data[, grpSum := sum(value, na.rm = TRUE), by = group_key]
  data[, grpGr := c(0, diffv(grpSum)), by = primary_key]
  if(!check){
    data[, grpSum := NULL]
    setnames(data, old = c("value", "grpGr"),
             new = c(value, paste(value, "_", grpName, "_gr", sep = "")))
  } else {
    setnames(data, old = c("value", "grpSum", "grpGr"),
             new = c(value, paste(value, "_", grpName, "_sum", sep = ""),
               paste(value, "_", grpName, "_gr", sep = "")))
  }
}


## Compute the raw data growth
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "itemCode", "Year"), value = "valueArea",
    grpName = "raw")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "itemCode", "Year"), value = "valueProd",
    grpName = "raw")
image(data.matrix(is.na(final.dt)))
text(rep(0.5, NCOL(final.dt)), seq(0, 1, length = NCOL(final.dt)),
     labels = paste(colnames(final.dt), " (", round(sapply(X = final.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))


## Compute the country commodity group
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "comGroup", "Year"), value = "valueArea",
    grpName = "countryCom")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("FAOST_CODE", "comGroup", "Year"), value = "valueProd",
    grpName = "countryCom")

## Compute the subregion item group
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "itemCode", "Year"), value = "valueArea",
    grpName = "subregItem")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "itemCode", "Year"), value = "valueProd",
    grpName = "subregItem")

## Compute the subregional commodity group
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "comGroup", "Year"), value = "valueArea",
    grpName = "subregCom")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_SUB_REG", "comGroup", "Year"), value = "valueProd",
    grpName = "subregCom")

## Compute the region item group
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "itemCode", "Year"), value = "valueArea",
    grpName = "regItem")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "itemCode", "Year"), value = "valueProd",
    grpName = "regItem")

## Compute the region commodity group
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "comGroup", "Year"), value = "valueArea",
    grpName = "regCom")
foo(data = final.dt,
    primary_key = c("FAOST_CODE", "itemCode"),
    group_key = c("UNSD_MACRO_REG", "comGroup", "Year"), value = "valueProd",
    grpName = "regCom")
image(data.matrix(is.na(final.dt)))
text(rep(0.5, NCOL(final.dt)), seq(0, 1, length = NCOL(final.dt)),
     labels = paste(colnames(final.dt), " (", round(sapply(X = final.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))


## examining the commodity group, these data have no growth rate, the
## biggest group is other and poultry meat. These clearly has no area
## harvested.
table(final.dt[is.na(valueArea_regCom_gr), comGroup])


## Compute the change in yield for all changes in production and area
yield.dt = final.dt[, grep("valueProd_", colnames(final.dt), value = TRUE),
  with = FALSE]/final.dt[, grep("valueArea_", colnames(final.dt), value = TRUE),
    with = FALSE]
setnames(yield.dt, old = colnames(yield.dt),
         new = gsub("valueProd", "valueYield", colnames(yield.dt)))
full.dt = cbind(final.dt, yield.dt)
image(data.matrix(is.na(full.dt)))
text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
     labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))

## Compute the validation bound of yield
full.dt[, validLowerBound := quantile(valueYield_raw_gr, probs = 0.25,
            na.rm = TRUE) - (quantile(valueYield_raw_gr, probs = 0.5,
            na.rm = TRUE) - quantile(valueYield_raw_gr, probs = 0.25,
            na.rm = TRUE)), by = "itemCode"]
full.dt[, validUpperBound := quantile(valueYield_raw_gr, probs = 0.75,
            na.rm = TRUE) + (quantile(valueYield_raw_gr, probs = 0.75,
            na.rm = TRUE) - quantile(valueYield_raw_gr, probs = 0.5,
            na.rm = TRUE)), by = "itemCode"]

## Validate the yield
full.dt[valueYield_countryCom_gr < validLowerBound |
        valueYield_countryCom_gr > validUpperBound,
        valueYield_countryCom_gr := NA]
full.dt[valueYield_subregItem_gr < validLowerBound |
         valueYield_subregItem_gr > validUpperBound,
         valueYield_subregItem_gr := NA]
full.dt[valueYield_subregCom_gr < validLowerBound |
         valueYield_subregCom_gr > validUpperBound,
         valueYield_subregCom_gr  := NA]
full.dt[valueYield_regItem_gr < validLowerBound |
         valueYield_regItem_gr > validUpperBound,
         valueYield_regItem_gr := NA]
full.dt[valueYield_regCom_gr  < validLowerBound |
         valueYield_regCom_gr > validUpperBound,
         valueYield_regCom_gr := NA]
dev.new()
image(data.matrix(is.na(full.dt)))
text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
     labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))



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

## system.time(
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
  ## )
image(data.matrix(is.na(full.dt)))
text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
     labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))


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
      ## This accounts for missing value in the original data, but how
      ## do we account for missing value in the growth rate? Keep it
      ## missing?
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
image(data.matrix(is.na(full.dt)))
text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
     labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))

full.dt[, imputedArea := valueArea]
full.dt[, imputedProd := valueProd]

full.dt[is.na(imputedArea), imputedArea := final_area_imp]
full.dt[is.na(imputedProd), imputedProd := final_prod_imp]

image(data.matrix(is.na(full.dt)))
text(rep(0.5, NCOL(full.dt)), seq(0, 1, length = NCOL(full.dt)),
     labels = paste(colnames(full.dt), " (", round(sapply(X = full.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))

## with(full.dt[is.na(valueArea), ], plot(ovalueArea, imputedArea))
## abline(a = 0, b = 1, col = "red")
## with(full.dt[is.na(valueProd), ], plot(ovalueProd, imputedProd))
## abline(a = 0, b = 1, col = "red")
