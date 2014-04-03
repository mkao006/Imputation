library(lattice)
library(lme4)
library(data.table)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeRatio.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)

## Read the data
wheat.dt = data.table(read.csv("../sua_data/pistachiosSUA.csv",
    stringsAsFactors = FALSE))

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
                                 c("FAOST_CODE", "ABBR_FAO_NAME")])
countryNameTable.dt[FAOST_CODE == 357, ABBR_FAO_NAME := "Taiwan and China"]
countryNameTable.dt[FAOST_CODE == 107, ABBR_FAO_NAME := "Cote d'Ivoire"]
countryNameTable.dt[FAOST_CODE == 284, ABBR_FAO_NAME := "Aland Islands"]
countryNameTable.dt[FAOST_CODE == 279, ABBR_FAO_NAME := "Curacao"]
countryNameTable.dt[FAOST_CODE == 182, ABBR_FAO_NAME := "Reunion"]
countryNameTable.dt[FAOST_CODE == 282, ABBR_FAO_NAME := "Saint Barthelemy"]
setnames(countryNameTable.dt,
         old = c("FAOST_CODE", "ABBR_FAO_NAME"),
         new = c("areaCode", "areaName"))

## final data frame
wheatRaw.dt = merge(merge(wheat.dt, regionTable.dt, by = "areaCode"),
    countryNameTable.dt, by = "areaCode")
wheatRaw.dt[, yieldValue := computeYield(productionValue, areaHarvestedValue)]

## Make corrections
wheatRaw.dt[areaHarvestedValue == 0 & productionValue != 0,
            areaHarvestedValue := as.numeric(NA)]
wheatRaw.dt[areaHarvestedValue != 0 & productionValue == 0,
            productionValue := as.numeric(NA)]


## Create variable for imputation
wheatRaw.dt[productionSymb != "M" & productionValue != 0,
            productionFit := productionValue]
wheatRaw.dt[areaHarvestedSymb != "M" & areaHarvestedValue != 0,
            areaHarvestedFit := areaHarvestedValue]
wheatRaw.dt[, yieldFit := yieldValue]

## Impute for production
wheatRaw.dt[, productionImputed := as.numeric(ensembleImpute(productionFit)),
            by = "areaName"]
wheatRaw.dt[, areaHarvestedImputed :=
            as.numeric(ensembleImpute(areaHarvestedFit)),
            by = "areaName"]
yieldModel = lmer(log(yieldFit) ~ (year|unsdSubReg) + (1|areaCode),
    data = wheatRaw.dt)
wheatRaw.dt[, yieldImputed := yieldFit]
wheatRaw.dt[is.na(yieldImputed),
            yieldImputed := exp(predict(yieldModel,
                newdata = wheatRaw.dt[is.na(yieldImputed), ],
                allow.new.levels = TRUE))]

wheatRaw.dt[, yieldImputed2 :=
            exp(predict(loess(log(yieldImputed) ~
                              year + log(productionImputed), data = .SD),
                        newdata = .SD)),
            by = "areaCode"]
wheatRaw.dt[, residualArea := productionImputed/yieldImputed2]
wheatRaw.dt[, areaHarvestedImputed2 :=
            exp(predict(loess(log(residualArea) ~
                              year + log(productionImputed), data = .SD),
                        newdata = .SD)),
            by = "areaCode"]


wheatRaw.dt[, productionImputed2 := areaHarvestedImputed2 * yieldImputed2]

xyplot(productionImputed2 + productionImputed + productionValue ~
       year|areaName, data = wheatRaw.dt, type = c("g", "l"), auto.key = TRUE)

xyplot(areaHarvestedImputed2 + areaHarvestedImputed + areaHarvestedValue ~
       year|areaName, data = wheatRaw.dt, type = c("g", "l"), auto.key = TRUE)

xyplot(yieldImputed2 + yieldImputed + yieldValue ~ year|areaName,
       data = wheatRaw.dt, type = c("g", "l"), auto.key = TRUE)









xyplot(productionValue ~ year|areaName, wheatFinal.dt,
       type = c("g", "l"))

test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    productionValue]
test[test == 0] = NA

ensembleImpute(test, plot = TRUE)






test = wheatRaw.dt[areaName == "Union of Soviet Socialist Republic",
    list(year, productionImputed, yieldImputed)]

test[, newYield :=
     exp(predict(loess(log(yieldImputed) ~ year + log(productionImputed),
                   data = test[productionImputed != 0, ]),
             newdata = test))]

