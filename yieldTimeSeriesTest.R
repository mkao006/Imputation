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
process.dt = rmNA.dt[c(!allMiss & includeGroup), ]
process.dt[, includeGroup := NULL]
process.dt[, allMiss := NULL]
setkeyv(process.dt, c("FAOST_CODE", "itemCode", "Year"))
if(check)
  checkSparsity(process.dt)


final.dt = process.dt[, list(FAOST_CODE, UNSD_MACRO_REG,
  UNSD_SUB_REG, itemCode, Year, symbArea, symbProd, ovalueArea,
  ovalueProd, valueArea, valueProd, valueYield)]
checkSparsity(final.dt)

## ## Compute coefficient of variation
## final.dt[, cvArea := sd(valueArea, na.rm = TRUE)/mean(valueArea, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]

## final.dt[, cvYield := sd(valueYield, na.rm = TRUE)/mean(valueYield, na.rm = TRUE),
##          by = c("FAOST_CODE", "itemCode")]

## with(final.dt, plot(cvArea, cvYield, xlim = c(0, 3.5), ylim = c(0, 3.5)))


## Compute average subregional yield
final.dt[, gvalueYield := mean(valueYield, na.rm = TRUE),
         by = c("Year", "UNSD_SUB_REG")]
## Grouped yield is missing for Micronesia, we will do a hack here
final.dt[is.na(gvalueYield),
         gvalueYield := mean(ovalueProd/ovalueArea, na.rm = TRUE), by = "Year"]


lme.fit = lme(valueYield ~ Year + gvalueYield, random =~ 1|FAOST_CODE,
  data = final.dt, na.action = na.omit)



final.dt[, imputedYield := valueYield]
for(i in unique(final.dt$itemCode)){
  tmp.fit = lme(valueYield ~ Year + gvalueYield, random =~1|FAOST_CODE,
    data = final.dt[itemCode == i, ], na.action = na.omit)
  final.dt[is.na(imputedYield) & itemCode == i,
           imputedYield := predict(tmp.fit,
             final.dt[is.na(imputedYield) & itemCode == i, ]), ]
}

final.dt[is.na(valueArea) & !is.na(valueProd),
         valueArea := valueProd/imputedYield]

final.dt[!is.na(valueArea) & is.na(valueProd),
         valueProd := valueArea/imputedYield]

checkSparsity(final.dt)


## Looks like there is a lot of mismatch between area and production,
## let us impute area now.

## Impute the area with linear interpolation

## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

final.dt[, imputedArea := na.locf(na.approx2(valueArea), na.rm = FALSE),
         by = c("FAOST_CODE", "itemCode")]
checkSparsity(final.dt)

final.dt[, imputedProd := imputedArea * imputedYield]

checkSparsity(final.dt)

final.dt[!is.na(valueArea), imputedArea := as.numeric(NA)]
final.dt[!is.na(valueYield), imputedYield := as.numeric(NA)]
final.dt[!is.na(valueProd), imputedProd := as.numeric(NA)]


keys = unique(final.dt[is.na(valueProd) | is.na(valueArea),
  list(FAOST_CODE, itemCode)])

pdf(file = "checkImputation.pdf")
for(i in 1:100){
  tmp = final.dt[FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode]]

  par(mfrow = c(3, 1))
  try({ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE)  
  with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                 col = "black"))
  with(tmp, points(Year, imputedProd, col = "red", pch = 19))})

  try({ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE)  
  with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                 col = "black"))
  with(tmp, points(Year, imputedArea, col = "red", pch = 19))})


  try({ymax = max(tmp[, list(valueYield, imputedYield)], na.rm = TRUE)  
  with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                 col = "black"))
  with(tmp, points(Year, imputedYield, col = "red", pch = 19))})
  
}
graphics.off()
system("evince checkImputation.pdf&")


## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

final.dt[, valueArea := na.approx2(valueArea), by = c("FAOST_CODE", "itemCode")]
checkSparsity(final.dt)

## Assuming linear interpolation has been carried out
auto.arima.forecast = function(ts){
  if(length(na.omit(ts)) >= 5){
    notNA = which(!is.na(ts))
    ## print(ts)
    firstObs = notNA[1]
    lastObs = notNA[length(notNA)]
    n = length(ts)
    tmp.fit = try(auto.arima(ts[firstObs:lastObs]))
    if(!inherits(tmp.fit, "try-error")){
      if(lastObs != n){
        imp = forecast(tmp.fit, h = n - lastObs)
        final = c(rep(NA, firstObs - 1), ts[firstObs:lastObs], imp$mean)
      } else {
        final = c(rep(NA, firstObs - 1), ts[firstObs:lastObs])
      }
    } else {
      final = ts
    }
  }else {
    final = ts
  }
  c(final)
}

final.dt[, valueArea := auto.arima.forecast(valueArea),
         by = c("FAOST_CODE", "itemCode")]

final.dt[!is.na(valueArea) & !is.na(valueProd) & is.na(valueYield),
         valueYield := valueProd/valueArea]


final.dt[!is.na(valueArea) & is.na(valueProd) & !is.na(valueYield),
         valueProd := valueArea * valueYield]
checkSparsity(final.dt)


final.dt[, valueYield := auto.arima.forecast(valueYield),
         by = c("FAOST_CODE", "itemCode")]
checkSparsity(final.dt)

final.dt[, mvalueYield := median(valueYield, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

omitMissYield.dt = final.dt[!is.na(mvalueYield), ]

omitMissYield.dt[is.na(valueYield), valueYield := mvalueYield]

omitMissYield.dt[is.na(valueArea) & !is.na(valueProd),
                 valueArea := valueProd/valueYield]

omitMissYield.dt[is.na(valueProd) & !is.na(valueArea),
                 valueProd := valueArea * valueYield]

## All the unimputed data are of "E", "F", and "T"s.
table(omitMissYield.dt[is.na(valueProd), symbArea])
table(omitMissYield.dt[is.na(valueProd), symbProd])


## Use linear interpolation for area
omitMissYield.dt[, valueArea := na.approx2(valueArea, na.rm = FALSE),
                 by = c("FAOST_CODE", "itemCode")]

omitMissYield.dt[is.na(valueProd), valueProd := valueArea * valueYield]

## Function to check the sparsity of the data
checkSparsity = function(Data){
  image(data.matrix(is.na(Data)))
  text(rep(0.5, NCOL(Data)), seq(0, 1, length = NCOL(Data)),
       labels = paste(colnames(Data), " (", round(sapply(X = Data,
         FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))
}

checkSparsity(omitMissYield.dt)

keys = unique(omitMissYield.dt[, list(FAOST_CODE, itemCode)])

pdf(file = "checkAllImputedSeries.pdf", width = 10)
for(i in 1:NROW(keys)){
  tmp = subset(omitMissYield.dt, FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode])
  with(tmp, plot(Year, valueProd, ylim = c(0, max(valueProd,
                                         na.rm = TRUE)), type = "l",
  main = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
         keys[i, itemCode], "itemName"]), "(", keys[i, itemCode], ") - ",
         FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                 keys[i, FAOST_CODE]), "LAB_NAME"], sep = "")))
  with(tmp, lines(Year, valueArea, col = "green"))
  with(tmp, lines(Year, valueYield, col = "steelblue"))
}  
graphics.off()
system("evince checkAllImputedSeries.pdf&")






## Examine codes
## ---------------------------------------------------------------------

pdf(file = "yieldTS.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
  ## tmp[, upper := predict(rq(valueYield ~ Year, tau = 0.95), data.frame(Year))]
  ## tmp[, med := predict(rq(valueYield ~ Year, tau = 0.5), data.frame(Year))]
  ## tmp[, lower := predict(rq(valueYield ~ Year, tau = 0.05), data.frame(Year))]
  try(print(ggplot(data = tmp, aes(x = Year, y = valueYield)) +
            geom_line(aes(col = factor(FAOST_CODE))) +
            scale_color_manual(values = c(rep(rgb(0, 0, 0, alpha = 0.5),
                               length(unique(tmp$FAOST_CODE))), "blue", "red")) +
            theme(legend.position = "none") +
            ## geom_line(aes(x = Year, y = upper, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = med, col = "blue",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = lower, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            labs(y = paste0("Yield series for ",
                   FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                          "itemName"], "(", i, ")"),
                 x = NULL)
            ))
}
graphics.off()
system("evince yieldTS.pdf&")




pdf(file = "areaTS.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
  ## tmp[, upper := predict(rq(valueArea ~ Year, tau = 0.95), data.frame(Year))]
  ## tmp[, med := predict(rq(valueArea ~ Year, tau = 0.5), data.frame(Year))]
  ## tmp[, lower := predict(rq(valueArea ~ Year, tau = 0.05), data.frame(Year))]
  try(print(ggplot(data = tmp, aes(x = Year, y = valueArea)) +
            geom_line(aes(col = factor(FAOST_CODE))) +
            scale_color_manual(values = c(rep(rgb(0, 0, 0, alpha = 0.5),
                               length(unique(tmp$FAOST_CODE))), "blue", "red")) +
            theme(legend.position = "none") +
            ## geom_line(aes(x = Year, y = upper, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = med, col = "blue",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = lower, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            labs(y = paste0("Area series for ",
                   FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                          "itemName"], "(", i, ")"),
                 x = NULL)
            ))
}
graphics.off()
system("evince areaTS.pdf&")




## Investigate shocks in production
pdf(file = "examineShocks.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp.dt = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"),
    select = c("FAOST_CODE", "itemCode", "Year", "valueArea", "valueProd",
      "valueYield"))
  mtmp.df = melt(tmp.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))
  mtmp.df$variable = factor(gsub("value", "", mtmp.df$variable),
    levels = c("Prod", "Area", "Yield"))
    try(print(ggplot(data = mtmp.df, aes(x = Year, y = log(value))) +
    geom_line(aes(col = factor(FAOST_CODE))) +
      facet_wrap(~variable, ncol = 1) +
        scale_color_manual(values = rep(rgb(0, 0, 0, alpha = 0.3),
                             length(unique(mtmp.df$FAOST_CODE)))) +
                               theme(legend.position = "none") +
  labs(y = paste("Log of ",
         unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
              i, "itemName"]), " (", i, ")", sep = ""))
            ))
}
graphics.off()
system("evince examineShocks.pdf&")

final.dt[, varArea := var(valueArea, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

final.dt[, varYield := var(valueYield, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

cor2 = function(x, y){
  tmp = try(cor(x, y, use = "complete.obs"))
  if(inherits(tmp, "try-error"))
    tmp = as.numeric(NA)
  tmp
}

final.dt[, areaYieldCor := cor2(valueYield, valueArea),
         by = c("FAOST_CODE", "itemCode")]
hist(final.dt$areaYieldCor, breaks = 100)

with(final.dt, plot(log(varArea), log(varYield)))
with(final.dt, plot(log(valueArea), log(valueYield)), )



pdf(file = "yieldAreaRelationship.pdf")
for(i in unique(final.dt$itemCode)){
  tmp.dt = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
    print(ggplot(data = tmp.dt,
                 aes(x = log(valueArea), y = log(valueYield))) +
          geom_point(alpha = 0.1))
}    
graphics.off()
system("evince yieldAreaRelationship.pdf&")

pdf(file = "checkAllLogImputedSeries.pdf", width = 10)
for(i in 1:NROW(keys)){
  tmp = subset(omitMissYield.dt, FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode])
  with(tmp, plot(Year, log(valueProd), ylim = c(0, max(log(valueProd),
                                         na.rm = TRUE)), type = "l",
  main = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
         keys[i, itemCode], "itemName"]), "(", keys[i, itemCode], ") - ",
         FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                 keys[i, FAOST_CODE]), "LAB_NAME"], sep = "")))
  with(tmp, text(Year, log(valueProd), labels = symbProd))
  with(tmp, lines(Year, log(valueArea), col = "green"))
  with(tmp, text(Year, log(valueArea), labels = symbArea))
  with(tmp, lines(Year, log(valueYield), col = "steelblue"))
}  
graphics.off()
system("evince checkAllLogImputedSeries.pdf&")



omitMissYield.dt$checkYield = with(omitMissYield.dt, valueProd/valueArea)
with(omitMissYield.dt, plot(valueYield, checkYield))





## Examine forecast under different methods






## Test of the linear mixed model
wy.dt = final.dt[itemCode == 116, list(FAOST_CODE, itemCode, Year, valueYield)]
mwy.dt = merge(wy.dt, FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE")],
  all.x = TRUE, by = "FAOST_CODE")

mwy.dt[, gvalueYield := mean(valueYield, na.rm = TRUE),
       by = c("UNSD_SUB_REG_CODE", "Year")]
mwy.dt[, UNSD_SUB_REG_CODE := factor(UNSD_SUB_REG_CODE)]

lme.fit = lme(valueYield ~ Year + gvalueYield, random =~ 1|FAOST_CODE,
  data = mwy.dt, na.action = na.omit)

mwy.dt[is.na(valueYield) & !is.na(gvalueYield), valueYield2 :=
       predict(lme.fit, mwy.dt[is.na(valueYield)  & !is.na(gvalueYield), ])]

mwy.dt[!is.na(valueYield) & !is.na(gvalueYield), valueYield2 :=
       fitted(lme.fit)]

pdf(file = "yieldImputeCheck.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp.dt = final.dt[itemCode == i, ]
  tmp.dt[, nValue := sum(!is.na(valueYield))/length(valueYield),
         by = c("UNSD_MACRO_REG")]
  tmp.dt[, subregionalYield := mean(valueYield, na.rm = TRUE),
         by = c("Year", "UNSD_MACRO_REG")]
  tmp.dt[nValue <= 0.5, subregionalYield := as.numeric(NA)]
  tmp.dt[is.na(subregionalYield),
         subregionalYield := mean(ovalueProd/ovalueArea, na.rm = TRUE),
         by = "Year"]
  try({tmp.fit = lme(valueYield ~ Year + subregionalYield, random =~1|FAOST_CODE,
    na.action = na.omit, data = tmp.dt)
  tmp.dt[is.na(valueYield),
         imputedYield := predict(tmp.fit, tmp.dt[is.na(valueYield), ])]

  mtmp.df = melt(tmp.dt[, list(FAOST_CODE, UNSD_MACRO_REG, itemCode, Year,
    valueYield, subregionalYield, imputedYield)],
    id.var = c("FAOST_CODE", "UNSD_MACRO_REG", "itemCode", "Year"))

  itemName = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                 "itemName"])
  print(ggplot(data = mtmp.df, aes(x = Year, y = value)) +
            geom_line(data = mtmp.df[mtmp.df$variable == "valueYield", ],
                      aes(col = factor(FAOST_CODE))) +
            geom_point(data = mtmp.df[mtmp.df$variable == "imputedYield", ],
                       aes(col = factor(FAOST_CODE))) +                
            geom_line(data = mtmp.df[mtmp.df$variable == "subregionalYield", ],
                      lwd = 1.5) +
            theme(legend.position = "none") +
            facet_wrap(~UNSD_MACRO_REG, ncol = 3) +
            labs(x = NULL,
                 y = paste0(itemName, " (", i, ")"))
            )})
}
graphics.off()
system("evince yieldImputeCheck.pdf&")
