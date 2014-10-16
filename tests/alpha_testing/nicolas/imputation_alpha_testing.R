## Load the library
library(faoswsProductionImputation)
library(faoswsFlag)
library(faoswsUtil)
library(data.table)
library(lattice)
library(reshape2)
library(splines)
library(lme4)

## Get the raw data
source("process_raw_data.R")
files = as.list(dir(splitDataDirectory, full.names = TRUE))
imputedDataDirectory = paste0("imputed_data/", commodityFolder, "/")

## Set the flag table
swsOldFlagTable = rbind(faoswsFlagTable,
    data.frame(flagObservationStatus = c("*", "F", "P"),
               flagObservationWeights = c(0.9, 0.6, 0.7)))
swsOldFlagTable[swsOldFlagTable$flagObservationStatus == "E",
                "flagObservationWeights"] = 0.55

## Further process the data
testData.lst = lapply(files,
    FUN = function(x){
        tmp = data.table(read.csv(file = x, stringsAsFactors = FALSE))
        tmp[, productionValue := as.numeric(productionValue)]
        tmp[, areaHarvestedValue := as.numeric(areaHarvestedValue)]
        ## Remove 2013 "T" values as per instruction of Nicolas.
        tmp[year == 2013 & productionFlag == "T",
            productionValue := as.numeric(NA)]
        tmp[year == 2013 & areaHarvestedFlag == "T",
            areaHarvestedValue := as.numeric(NA)]
        tmp[productionFlag == " ", productionFlag := ""]
        tmp[areaHarvestedFlag == " ", areaHarvestedFlag := ""]
        })

## This is a hack to match the new system
lapply(testData.lst,
       FUN = function(x){
           x[, productionFlag2 := ""]
           x[, areaHarvestedFlag2 := ""]
           x[, yieldFlag2 := ""]
       }
       )



## Compute the yield
lapply(testData.lst,
       FUN = function(x)
       computeYield(productionValue = "productionValue",
                    productionObservationFlag = "productionFlag",
                    areaHarvestedValue = "areaHarvestedValue",
                    areaHarvestedObservationFlag =
                        "areaHarvestedFlag",
                    yieldValue = "yieldValue",
                    yieldObservationFlag = "yieldFlag",
                    yieldMethodFlag = "yieldFlag2",
                    newMethodFlag = "i",
                    flagTable = swsOldFlagTable,
                    data = x)
       )

                       
## Impute the data
imputedData.lst = lapply(testData.lst,
    FUN = function(x){
        print(unique(x$itemName))
        imputeProductionDomain(data = x,
                               productionValue = "productionValue",
                               areaHarvestedValue = "areaHarvestedValue",
                               yieldValue = "yieldValue",
                               yearValue = "year",
                               productionObservationFlag =
                                   "productionFlag",
                               areaHarvestedObservationFlag =
                                   "areaHarvestedFlag",
                               yieldObservationFlag = "yieldFlag",
                               productionMethodFlag = "productionFlag2",
                               areaHarvestedMethodFlag =
                                   "areaHarvestedFlag2",
                               yieldMethodFlag = "yieldFlag2",
                               flagTable = swsOldFlagTable,
                               imputedFlag = "E",
                               naFlag = "M",
                               imputationFlag = "I",
                               newMethodFlag = "",
                               maxdf = 5,
                               yieldFormula = yieldValue ~ -1 + (1 + bs(year, df = 2, degree = 1)|areaCode))
    }
       )


## This is a hack to match the new system
lapply(imputedData.lst,
       FUN = function(x){
           x[, productionFlag2 := NULL]
           x[, areaHarvestedFlag2 := NULL]
           x[, yieldFlag2 := NULL]
       }
       )

## Function to save the imputation
saveImputation = function(x){
    tmp = x[year >= 2009, ]
    tmp[, areaHarvestedValue := round(areaHarvestedValue)]
    tmp[, productionValue := round(productionValue)]
    tmp[, yieldValue := round(yieldValue * 10000)]
    meltTmp = melt(tmp,
        id.var = c("areaCode", "areaName", "itemCode",
            "itemName", "year"))
    meltTmp[, type := ifelse(grepl("Value",
                       as.character(variable)), "num", "symb")]
    meltTmp[, variable := gsub("Value|Flag", "", variable)]
    dcast(meltTmp, areaCode + areaName + itemCode + itemName +
          variable ~ year + type,
          value.var = "value")
}


## Save the imputation results
if(!file.exists(imputedDataDirectory))
    dir.create(imputedDataDirectory)

lapply(imputedData.lst,
       FUN = function(x){
           write.csv(saveImputation(x),
                     file =
                     paste0(imputedDataDirectory,
                            unique(x$itemName), ".csv"),
                     row.names = FALSE, na = "")
       }
       )



pdf(file = "allImputed.pdf",  width = 20, height = 12)
lapply(imputedData.lst,
       FUN = function(x){
           print(
               xyplot(log(productionValue) ~ year|areaName,
                      data = x,
                      group = x$productionFlag == "I",
                      main = unique(x$itemName))
               )
           print(
               xyplot(log(areaHarvestedValue) ~ year|areaName,
                      data = x,
                      group = x$areaHarvestedFlag == "I",
                      main = unique(x$itemName))
               )
           print(
               xyplot(yieldValue ~ year|areaName,
                      data = x,
                      group = x$yieldFlag == "I",
                      main = unique(x$itemName))
               )                      
       }
       )
graphics.off()


## ## Impute the data
## imputedDataAuto.lst = lapply(testData.lst,
##     FUN = function(x){
##         print(unique(x$itemName))
##         imputeProductionDomain(data = x,
##                                productionValue = "productionValue",
##                                areaHarvestedValue = "areaHarvestedValue",
##                                yieldValue = "yieldValue",
##                                yearValue = "year",
##                                productionObservationFlag =
##                                    "productionFlag",
##                                areaHarvestedObservationFlag =
##                                    "areaHarvestedFlag",
##                                yieldObservationFlag = "yieldFlag",
##                                productionMethodFlag = "productionFlag2",
##                                areaHarvestedMethodFlag =
##                                    "areaHarvestedFlag2",
##                                yieldMethodFlag = "yieldFlag2",
##                                flagTable = swsOldFlagTable,
##                                imputedFlag = "E",
##                                naFlag = "M",
##                                imputationFlag = "I",
##                                newMethodFlag = "",
##                                maxdf = 5)
##     }
##        )


## pdf(file = "allImputedAuto.pdf",  width = 20, height = 12)
## lapply(imputedDataAuto.lst,
##        FUN = function(x){
##            print(
##                xyplot(log(productionValue) ~ year|areaName,
##                       data = x,
##                       group = x$productionFlag == "I",
##                       main = unique(x$itemName))
##                )
##            print(
##                xyplot(log(areaHarvestedValue) ~ year|areaName,
##                       data = x,
##                       group = x$areaHarvestedFlag == "I",
##                       main = unique(x$itemName))
##                )
##            print(
##                xyplot(yieldValue ~ year|areaName,
##                       data = x,
##                       group = x$yieldFlag == "I",
##                       main = unique(x$itemName))
##                )                      
##        }
##        )
## graphics.off()


## check = imputedData.lst[[19]][areaName == "Switzerland", ]
## v = check[, productionValue]
## v[check[, productionFlag == "I"]] = NA
## ensembleImpute(v, plot = TRUE)

