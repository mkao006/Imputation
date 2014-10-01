library(forecast)
library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("../support_functions/ensembleImpute.R")
source("../support_functions/computeYield.R")
dataPath = "../sua_data"
dataFile = dir(dataPath)

## Number of time series
nSeries = 0
for(i in dataFile){
    ## Read the data
    myFile = paste0(dataPath, "/", i)
    wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))
    nSeries = nSeries + length(unique(wheat.dt$areaCode))
}


nCountries = c()
for(i in dataFile){
    ## Read the data
    myFile = paste0(dataPath, "/", i)
    wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))
    nCountries = unique(c(nCountries, wheat.dt$areaCode))
}

missingPct = list()
for(i in dataFile){
    ## Read the data
    myFile = paste0(dataPath, "/", i)
    wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))
    missingPct[["Production"]] = c(missingPct[["Production"]],
                  sum(wheat.dt$productionSymb == "M")/NROW(wheat.dt))
    missingPct[["AreaHarvested"]] = c(missingPct[["AreaHarvested"]],
                  sum(wheat.dt$productionSymb == "M")/NROW(wheat.dt))
}

