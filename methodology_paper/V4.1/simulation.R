## TODO (Michael): Do the simulation with only official and semi
##                 official data.
##
## TODO (Michael): Calculate benchmarked missing proportions.


## Load libraries
library(faoswsProductionImputation)
library(faoswsFlag)
library(FAOSTAT)
library(data.table)
library(lattice)
library(splines)

## Function to compute the yield as a ratio of production and area
## harvested.
computeRatio = function(numerator, denominator){
    as.numeric(ifelse(numerator == 0 | denominator == 0, NA,
                      numerator/denominator))
}

## A temporary flag table for the old flag system
oldFlagTable = 
    data.frame(flagObservationStatus = 
               c(" ", "*", "T", "P", "E", "F", "M"),
               flagObservationWeights = 
               seq(1, 0, length = 7), 
               stringsAsFactors = FALSE)

read.sim = function(file){
    tmp = read.csv(file = file, stringsAsFactors = FALSE)
    tmp2 = data.table(merge(tmp, 
        FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")], 
        by.x = "areaCode", by.y = "FAOST_CODE"))
    setnames(tmp2, old = "FAO_TABLE_NAME", new = "areaName")

    tmp3 = removeNoInfo(data = tmp2, productionSymb = "productionSymb",
        productionValue = "productionValue", index = "areaCode")        
    tmp3[!productionSymb %in% c(" ", "", "*"), 
         productionValue := as.numeric(NA)]
    tmp3[!areaHarvestedSymb %in% c(" ", "", "*"), 
         areaHarvestedValue := as.numeric(NA)]    
    setkeyv(tmp3, c("areaCode", "year"))
    tmp4 = tmp3[areaCode != 357, ]
    tmp4
}

simulationFunc = function(data, pct, n.sim){
    benchmark = unlist(data$productionValue)
    benchmark[benchmark == 0] = NA
    benchmarkMissIndex = which(is.na(benchmark))
    dataMissProp = length(benchmarkMissIndex)/NROW(data)
    
    sim.result = rep(NA, n.sim)
        
    for(i in 1:n.sim){
        cat("simulation: ", i, "\n")
        availableData = (1:NROW(data))[-benchmarkMissIndex]
        index = sample(availableData, length(availableData) * pct)
        tmp = copy(data)
        tmp[index, productionValue := as.numeric(NA)]
        
        tmp[, yieldValue := 
            computeRatio(productionValue, areaHarvestedValue)]
        tmp[, yieldSymb := "M"]
        imputed = imputeProductionDomain(tmp,
            productionVar = "productionValue", 
            areaHarvestedVar = "areaHarvestedValue", 
            yieldVar = "yieldValue", 
            productionObservationFlag = "productionSymb", 
            areaHarvestedObservationFlag = "areaHarvestedSymb", 
            yieldObservationFlag = "yieldSymb", 
            index = "areaName", flagTable = oldFlagTable, 
            yieldFormula = yieldValue ~ -1 + 
            (1 + year|areaName))
        sim.result[i] = sum(abs((benchmark[-benchmarkMissIndex] -
                      imputed$productionValue[-benchmarkMissIndex])/
                      benchmark[-benchmarkMissIndex]),
                      na.rm = TRUE)/NROW(data) * 100
    }
    list(result = sim.result,
         totalMiss = 1 - length(availableData)/NROW(data) + pct)
}

nSim = 1
wheat.dt = read.sim("wheatSUA.csv")
wheat20Sim = simulationFunc(wheat.dt, pct = 0.2, n.sim = nSim)
wheat40Sim = simulationFunc(wheat.dt, pct = 0.4, n.sim = nSim)
wheat60Sim = simulationFunc(wheat.dt, pct = 0.6, n.sim = nSim)
wheat80Sim = simulationFunc(wheat.dt, pct = 0.8, n.sim = nSim)
## wheatSim.df = data.frame(missPct = rep(c(20, 40, 60, 80), each = nSim),
##     simResult = c(wheat20Sim, wheat40Sim, wheat60Sim, wheat80Sim))
## plot(wheatSim.df, xlim = c(0, 100))


grape.dt = read.sim("grapesSUA.csv")
grape20Sim = simulationFunc(grape.dt, pct = 0.2, n.sim = nSim)
grape40Sim = simulationFunc(grape.dt, pct = 0.4, n.sim = nSim)
grape60Sim = simulationFunc(grape.dt, pct = 0.6, n.sim = nSim)
grape80Sim = simulationFunc(grape.dt, pct = 0.8, n.sim = nSim)
## grapeSim.df = data.frame(missPct = rep(c(40, 60, 80), each = nSim),
##     simResult = c(grape40Sim, grape60Sim, grape80Sim))
## plot(grapeSim.df, xlim = c(0, 100))



beef.dt = read.sim("beefSUA.csv")
beef20Sim = simulationFunc(beef.dt, pct = 0.2, n.sim = nSim)
beef40Sim = simulationFunc(beef.dt, pct = 0.4, n.sim = nSim)
beef60Sim = simulationFunc(beef.dt, pct = 0.6, n.sim = nSim)
beef80Sim = simulationFunc(beef.dt, pct = 0.8, n.sim = nSim)
## beefSim.df = data.frame(missPct = rep(c(40, 60, 80), each = nSim),
##     simResult = c(beef40Sim, beef60Sim, beef80Sim))
## plot(beefSim.df, xlim = c(0, 100))


mean(wheat20Sim$result)
mean(wheat40Sim$result)
mean(wheat60Sim$result)
mean(wheat80Sim$result)

mean(grape20Sim$result)
mean(grape40Sim$result)
mean(grape60Sim$result)
mean(grape80Sim$result)

mean(beef20Sim$result)
mean(beef40Sim$result)
mean(beef60Sim$result)
mean(beef80Sim$result)



