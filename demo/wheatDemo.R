library(data.table)
library(reshape2)
library(FAOSTAT)
library(lme4)
source("../codes/naiveImputation.R")
source("../codes/computeYield.R")
source("../swsToDataFrame.R")
source("../swsRmImputation.R")
source("../toLowerCamel.R")
source("../swsToImputationDataTable.R")
source("../codes/swsImputation.R")
source("../codes/meanlme4.R")
source("../splitNACountry.R")
source("../codes/impDiag.R")
source("../codes/impFit.R")

## Data manipulation
wheat.dt = imputationDataManipulation(file = "wheatSUA.csv",
    denormalizer = "Element.Code")
wheatSub.dt = wheat.dt[Year >= 1993, ]

## Imputation
wheatImputed.lst = swsImputation(wheatSub.dt, area = "areaNum",
    prod = "productionNum", yield = "yieldNum",
    country = "areaName", region = "unsdSubReg",
    year = "Year", tol = 1e-3, EMverbose = TRUE)

## Examine the imputation
impDiag(object = wheatImputed.lst, yieldObsVar = "yieldNum",
        countryVar = "areaName", savePlots = FALSE)

## Examine the fit
impFit(wheatImputed.lst, productionObsVar = "productionNum",
       areaObsVar = "areaNum", yieldObsVar = "yieldNum",
       countryVar = "areaCode", itemCode = "itemCode",
       file = "wheatImputationFit.pdf")
