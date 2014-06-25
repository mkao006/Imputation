library(reshape2)
library(earth)
library(forecast)
library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeYield.R")
source("../support_functions/naiveImputation.R")
source("../support_functions/containInfo.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)

## Read and merge the data
cereal.dt = data.table()
for(i in seq_along(dataFile)){
    tmp = read.csv(paste0(dataPath, "/", dataFile[i]))
    if(NCOL(tmp) == 7)
        cereal.dt = rbind(cereal.dt, data.table(tmp))
}


start.time = Sys.time()
## Experiementing the extrapolation
predict.dt =
    data.table(expand.grid(areaCode = unique(cereal.dt$areaCode),
                           itemCode = unique(cereal.dt$itemCode),
                           year = c(2012:2013)))
## predict.dt[, itemCode := unique(cereal.dt$itemCode)]
predict.dt[, areaHarvestedValue := as.numeric(NA)]
predict.dt[, productionValue := as.numeric(NA)]
predict.dt[, areaHarvestedSymb := as.character("M")]
predict.dt[, productionSymb := as.character("M")]
cereal.dt = rbind(cereal.dt, predict.dt, use.names = TRUE)


## append regional and country information
regionTable.dt =
    data.table(FAOregionProfile[!is.na(FAOregionProfile$FAOST_CODE),
                                c("FAOST_CODE", "UNSD_SUB_REG",
                                  "UNSD_MACRO_REG")])
setnames(regionTable.dt,
         old = c("FAOST_CODE", "UNSD_SUB_REG", "UNSD_MACRO_REG"),
         new = c("areaCode", "unsdSubReg", "unsdMacroReg"))
countryNameTable.dt =
    data.table(FAOcountryProfile[!is.na(FAOcountryProfile$FAOST_CODE),
                                 c("FAOST_CODE", "FAO_TABLE_NAME")])
setnames(countryNameTable.dt,
         old = c("FAOST_CODE", "FAO_TABLE_NAME"),
         new = c("areaCode", "areaName"))

## append item information
itemName.dt =
    data.table(unique(FAOmetaTable$itemTable[, c("itemCode",
                                                 "itemName")]))
itemName.dt[, itemCode := as.numeric(itemCode)]

## final data frame for processing
cerealRaw.dt = merge(merge(cereal.dt, regionTable.dt,
    by = "areaCode"), countryNameTable.dt, by = "areaCode")
cerealRaw.dt = merge(cerealRaw.dt, itemName.dt, by = "itemCode",
    all.x = TRUE)
cerealRaw.dt[, yieldValue :=
             computeYield(productionValue, areaHarvestedValue)]

## Remove country which contains no information
cerealRaw.dt[,info :=
             containInfo(.SD, "productionSymb", "productionValue"),
             by = c("areaName", "itemCode")]
cerealRaw.dt = cerealRaw.dt[info == TRUE, ]


## Correct conflicting values.
cerealRaw.dt[areaHarvestedValue == 0 & productionValue != 0,
             areaHarvestedValue := as.numeric(NA)]
cerealRaw.dt[areaHarvestedValue != 0 & productionValue == 0,
             productionValue := as.numeric(NA)]


## Create variable for imputation
cerealRaw.dt[, productionFit :=
             ifelse(productionSymb == "M" &
                    productionValue == 0, NA, productionValue)]
cerealRaw.dt[, areaHarvestedFit :=
             ifelse(areaHarvestedSymb == "M" &
                    areaHarvestedValue == 0, NA, areaHarvestedValue)]
cerealRaw.dt[, yieldFit :=
             computeYield(productionFit, areaHarvestedFit)]
yieldMissIndex = which(is.na(cerealRaw.dt$yieldFit))

## Create weights
cerealRaw.dt[productionSymb %in% c(" ", "*", "", "\\"),
             productionWeight := as.numeric(1)]
cerealRaw.dt[productionSymb == "M",
             productionWeight := as.numeric(0.25)]
cerealRaw.dt[!(productionSymb %in% c(" ", "*", "", "M")),
             productionWeight := as.numeric(0.5)]
cerealRaw.dt[areaHarvestedSymb %in% c(" ", "*", "", "\\"),
             areaHarvestedWeight := as.numeric(1)]
cerealRaw.dt[areaHarvestedSymb == "M",
             areaHarvestedWeight := as.numeric(0.25)]
cerealRaw.dt[!(areaHarvestedSymb %in% c(" ", "*", "", "M")),
             areaHarvestedWeight := as.numeric(0.5)]
cerealRaw.dt[, yieldWeight := productionWeight * areaHarvestedWeight]


## First stage of imputation (yield)
## cerealRaw.dt[, yieldImputed := yieldFit]


## yieldModelFull = try(lmer(yieldFit ~
##     (1 + bs(year, degree = 1, df = 5)|areaName:itemName),
##     data = cerealRaw.dt, weights = yieldWeight))


## if(!inherits(yieldModelFull, "try-error") & !grepl("Nes|other", i)){
##     cerealRaw.dt[,
##                  yieldImputed := predict(yieldModelFull,
##                                   newdata = .SD,
##                                   allow.new.levels = TRUE)]
## } else {
##     cerealRaw.dt[, yieldImputed := ensembleImpute(yieldFit),
##                  by = c("areaName", "itemName")]
## }


## Old model
for(i in unique(cerealRaw.dt$itemCode)){
    yieldModelOld = try(lmer(yieldFit ~
        (1 + bs(year, degree = 1, df = 5)|areaName),
        data = cerealRaw.dt[itemCode == i, ], weights = yieldWeight))


    if(!inherits(yieldModelOld, "try-error") & !grepl("Nes|other", i)){
        cerealRaw.dt[itemCode == i,
                     oldImputation := predict(yieldModelOld,
                                      newdata = .SD,
                                      allow.new.levels = TRUE)]
    } else {
        cerealRaw.dt[itemCode == i, yieldImputed := ensembleImpute(yieldFit),
                     by = "areaName"]
    }
}


## New model with items
YieldModelNew = try(lmer(yieldFit ~
    (1 + bs(year, degree = 1, df = 5)|areaName:itemName),
    data = cerealRaw.dt, weights = yieldWeight))


if(!inherits(YieldModelNew, "try-error") & !grepl("Nes|other", i)){
    cerealRaw.dt[,
                 newImputation := predict(YieldModelNew,
                                  newdata = .SD,
                                  allow.new.levels = TRUE)]
} else {
    cerealRaw.dt[, proposedImputation := ensembleImpute(yieldFit),
                 by = c("areaName", "itemName")]
}

## New model with log and spline 3
YieldModelNewLog3 = try(lmer(log(yieldFit) ~
    (1 + bs(log(year), degree = 1, df = 3)|areaName:itemName),
    data = cerealRaw.dt, weights = yieldWeight))


if(!inherits(YieldModelNewLog3, "try-error") & !grepl("Nes|other", i)){
    cerealRaw.dt[,
                 newImputationLog3 := exp(predict(YieldModelNewLog3,
                                  newdata = .SD,
                                  allow.new.levels = TRUE))]
} else {
    cerealRaw.dt[, proposedImputation := ensembleImpute(yieldFit),
                 by = c("areaName", "itemName")]
}

## New model with log and spline 5
YieldModelNewLog5 = try(lmer(log(yieldFit) ~
    (1 + bs(log(year), degree = 1, df = 5)|areaName:itemName),
    data = cerealRaw.dt, weights = yieldWeight))


if(!inherits(YieldModelNewLog5, "try-error") & !grepl("Nes|other", i)){
    cerealRaw.dt[,
                 newImputationLog5 := exp(predict(YieldModelNewLog5,
                                  newdata = .SD,
                                  allow.new.levels = TRUE))]
} else {
    cerealRaw.dt[, proposedImputation := ensembleImpute(yieldFit),
                 by = c("areaName", "itemName")]
}



## New model with sqrt and spline 3
YieldModelNewSqrt3 = try(lmer(yieldFit^0.5 ~
    (1 + bs(year, degree = 1, df = 3)|areaName:itemName),
    data = cerealRaw.dt, weights = yieldWeight))


if(!inherits(YieldModelNewSqrt3, "try-error") & !grepl("Nes|other", i)){
    cerealRaw.dt[,
                 newImputationSqrt3 := (predict(YieldModelNewSqrt3,
                                  newdata = .SD,
                                  allow.new.levels = TRUE))^2]
} else {
    cerealRaw.dt[, proposedImputation := ensembleImpute(yieldFit),
                 by = c("areaName", "itemName")]
}




## New model with sqrt and spline 3
YieldModelNewSqrt5 = try(lmer(yieldFit^0.5 ~
    (1 + bs(year, degree = 1, df = 5)|areaName:itemName),
    data = cerealRaw.dt, weights = yieldWeight))


if(!inherits(YieldModelNewSqrt5, "try-error") & !grepl("Nes|other", i)){
    cerealRaw.dt[,
                 newImputationSqrt5 := (predict(YieldModelNewSqrt5,
                                  newdata = .SD,
                                  allow.new.levels = TRUE))^2]
} else {
    cerealRaw.dt[, proposedImputation := ensembleImpute(yieldFit),
                 by = c("areaName", "itemName")]
}




## NOTE (Michael): Check why some values are NA

## Remove values that are negative and impute with naive imputation.
if(any(cerealRaw.dt$yieldImputed < 0, na.rm = TRUE)){
    cat(i, file = "cerealWithNegativeYield.txt", append = TRUE)
    cerealRaw.dt[yieldImputed < 0, yieldImputed := as.numeric(NA)]
    cerealRaw.dt[, yieldImputed := naiveImputation(yieldImputed),
                 by = c("areaName", "itemName")]
}

## cerealRaw.dt[!is.na(yieldFit), yieldImputed := yieldFit]

## Second stage (impute production if area harvested is available)
cerealRaw.dt[!is.na(areaHarvestedFit) & is.na(productionFit),
             productionFit := areaHarvestedFit * yieldFit]

## Third stage (impute production if neither exist)
cerealRaw.dt[, productionImputed :=
             as.numeric(ensembleImpute(productionFit)),
             by = c("areaName", "itemName")]

## Fourth stage (balance the area harvested)
cerealRaw.dt[, areaHarvestedImputed := productionImputed/yieldImputed]


end.time = Sys.time()
end.time - start.time

pdf(file = "checkIteractionEffect.pdf", width = 30, height = 20)
for(i in unique(cerealRaw.dt$itemName)){
    yieldPlot =
        xyplot(newImputationLog3 + newImputationSqrt3 + newImputationSqrt5 + yieldValue ~
               year|areaName,
               data = cerealRaw.dt[itemName == i, ], auto.key = TRUE,
               K = cerealRaw.dt$yieldImputed,
               panel = function(...){
                   panel.xyplot(...,
                                type = c("g", "l"))
               },
               main = gsub("SUA\\.csv", "", i))
    print(yieldPlot)
}
dev.off()

pdf(file = "checkIteractionEffect2.pdf", width = 30, height = 20)
for(i in unique(cerealRaw.dt$areaName)){
    yieldPlot =
        xyplot(newImputationLog3 + newImputationSqrt3 + newImputationSqrt5 + yieldValue ~
               year|itemName,
               data = cerealRaw.dt[areaName == i, ], auto.key = TRUE,
               K = cerealRaw.dt$yieldImputed,
               panel = function(...){
                   panel.xyplot(...,
                                type = c("g", "l"))
               },
               main = gsub("SUA\\.csv", "", i))
    print(yieldPlot)
}
dev.off()





pdf(file = "checkProductionCorrelation.pdf", width = 30, height = 20)
for(i in unique(cerealRaw.dt$areaName)){
    yieldPlot =
        xyplot(log(productionValue) + log(productionImputed) ~
               year|itemName,
               data = cerealRaw.dt[areaName == i, ], auto.key = TRUE,
               K = cerealRaw.dt$yieldImputed,
               panel = function(...){
                   panel.xyplot(...,
                                type = c("g", "l"))
               },
               main = gsub("SUA\\.csv", "", i))
    print(yieldPlot)
}
dev.off()
