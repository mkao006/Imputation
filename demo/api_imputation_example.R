########################################################################
## Title: Test script for the imputation module
## Date: 2013-11-28
########################################################################

library(data.table)
library(reshape2)
library(FAOSTAT)
library(lme4)
library(faosws)
## source("../codes/naiveImputation.R")
## source("../codes/computeYield.R")
## source("../swsToDataFrame.R")
## source("../swsRmImputation.R")
## source("../toLowerCamel.R")
## source("../swsToImputationDataTable.R")
## source("../codes/swsImputation.R")
## source("../codes/meanlme4.R")
## source("../splitNACountry.R")
## source("../codes/impDiag.R")
## source("../codes/impFit.R")
## source("../codes/shocklme4.R")
## source("../codes/predict.shocklme4.R")



## Setup mock context

swsContext.baseRestUrl <- "https://swsrm:8181/sws/rest"
swsContext.token <- "4789c952-d4b2-48b8-adca-a228c35ba560"
swsContext.executionId <- 315


## Prepare parameters.

dimArea = Dimension(name = "geographicAreaM49", keys = c("380", "250"))
dimElement = Dimension(name = "measuredElement", keys = c("5312", "5510"))
dimItem = Dimension(name = "measuredItemCPC", keys = "C0111")
addKey(dimItem) = "01112"
dimTime = Dimension(name = "timePointYears",
    keys = as.character(seq(from = 1994, to = 2011)))
myKey = DatasetKey(domain = "agriculture", dataset = "agriculture",
    dimensions = c(dimArea, dimElement, dimItem, dimTime))


## Add pivoting.
pivot = c(
	Pivoting(code= "geographicAreaM49", ascending = TRUE), 
	Pivoting(code= "measuredItemCPC", ascending = TRUE), 
	Pivoting(code= "measuredElement", ascending = TRUE), 
	Pivoting(code = "timePointYears", ascending = FALSE))


## Execute the data call.
wheatData = GetData(key = myKey, flags = TRUE, normalized = FALSE,
        metadata = FALSE, pivoting = pivot)


## Need to determine the data format for the example
## ---------------------------------------------------------------------

## Data manipulation
## wheat.dt = swsToImputationDataTable(file = "wheatSUA.csv",
##     denormalizer = "Element.Code")
## wheatSub.dt = wheat.dt[Year %in% 1994:2012, ]

## Imputation
## wheatImputed.lst = swsImputation(wheatSub.dt, area = "areaNum",
##     prod = "productionNum", yield = "yieldNum",
##     country = "areaName", region = "unsdSubReg",
##     year = "Year", tol = 1e-3, EMverbose = TRUE,
##     meanType = "shocklme4")
