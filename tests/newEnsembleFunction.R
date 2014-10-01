library(faoswsProductionImputation)
library(faoswsFlag)
library(faoswsExtra)
library(data.table)
library(earth)
library(forecast)
library(lme4)



## Step one: Get data
wheat.dt = data.table(read.csv(file = "../sua_data/wheatSUA.csv"))

## This step are not official
## Remove 298 and restrict year
wheat.dt = wheat.dt[areaCode != 298 & year >= 1995, ]
wheat.dt[productionSymb == " ", productionSymb := ""]
wheat.dt[areaHarvestedSymb == " ", areaHarvestedSymb := ""]



## Optional step: Remove prior imputation
removeImputation(data = wheat.dt,
                 value = "areaHarvestedValue",
                 flag = "areaHarvestedSymb")


removeImputation(data = wheat.dt,
                 value = "productionValue",
                 flag = "productionSymb")

## Optional step: Compute yield if not available
computeYield(productionValue = "productionValue",
             productionFlag = "productionSymb",
             areaHarvestedValue = "areaHarvestedValue",
             areaHarvestedFlag = "areaHarvestedSymb",
             yieldValue = "yieldValue",
             yieldFlag = "yieldSymb",
             data = wheat.dt)


## Optional step: Correct conflict values

## Step two: Remove country with no info.

wheatRemovedNoInfo.dt =
    removeNoInfo(data = wheat.dt, flag = "yieldSymb",
                 value = "yieldValue", byKey = "areaCode")


## Step three: Impute Yield
imputeYield(formula = yieldValue ~ -1 + (1 + year|areaCode),
            yieldFlag = "yieldSymb", imputationFlag = "I",
            byKey = "areaCode", data = wheatRemovedNoInfo.dt)

## Step four: Impute production

## (1) By balance
imputeProduction(productionValue = "productionValue",
                 productionFlag = "productionSymb",
                 areaHarvestedValue = "areaHarvestedValue",
                 areaHarvestedFlag = "areaHarvestedSymb",
                 yieldValue = "yieldValue",
                 yieldFlag = "yieldSymb",
                 data = wheatRemovedNoInfo.dt,
                 byKey = "areaCode")




## Balance Area Harvested

balanceAreaHarvested(productionValue = "productionValue",
                     productionFlag = "productionSymb",
                     areaHarvestedValue = "areaHarvestedValue",
                     areaHarvestedFlag = "areaHarvestedSymb",
                     yieldValue = "yieldValue",
                     yieldFlag = "yieldSymb",
                     data = wheatRemovedNoInfo.dt)


imputedWheat.dt =
    imputeProductionDomain(data = wheat.dt,
                           productionVar = "productionValue",
                           areaHarvestedVar = "areaHarvestedValue",
                           yieldVar = "yieldValue",
                           productionObservationFlag = "productionSymb",
                           areaHarvestedObservationFlag =
                               "areaHarvestedSymb",
                           yieldObservationFlag = "yieldSymb")
                       
