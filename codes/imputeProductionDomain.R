##' This function imputes the whole production domain.
##'
##' The function will impute production, area harvested and yield at
##' the same time.
##'
##' Transformation in the yield formula is not allowed and will not be
##' taken into account.
##'
##' @param data The data
##' @param productionVar The name of the production variable.
##' @param areaHarvestedVar The name of the area harvested variable.
##' @param yieldVar The name of the yield variable.
##' @param productionObservationFlag The observation flag of production.
##' @param areaHarvestedObservationFlag The observation flag of area
##' harvested.
##' @param yieldObservationFlag The observation flag of yield.
##' @param index The unique key identifier.
##'
##' @export
##' 

imputeProductionDomain = function(data, productionVar, areaHarvestedVar,
    yieldVar, productionObservationFlag, areaHarvestedObservationFlag,
    yieldObservationFlag, index, flagTable, yieldFormula){

    
    ## Make a copy of the data
    dataCopy = copy(data)
    setnames(dataCopy,
             old = c(productionVar, areaHarvestedVar, yieldVar,
                 productionObservationFlag, areaHarvestedObservationFlag,
                 yieldObservationFlag),
             new = c("productionVar", "areaHarvestedVar", "yieldVar",
                 "productionObservationFlag", "areaHarvestedObservationFlag",
                 "yieldObservationFlag"))
    
    ## Convert flags to weight
    dataCopy[, c("productionWeight", "areaHarvestedWeight", "yieldWeight") :=
             list(flag2weight(productionObservationFlag, flagTable = flagTable),
                  flag2weight(areaHarvestedObservationFlag, flagTable = flagTable),
                  flag2weight(yieldObservationFlag, flagTable = flagTable))]

    ## Create missing index
    productionMissIndex = dataCopy[, is.na(productionVar)]
    areaHarvestedMissIndex = dataCopy[, is.na(areaHarvestedVar)]
    yieldMissIndex = dataCopy[, is.na(yieldVar)]
    
    ## update the formula  for the updated name
    newYieldFormula = update(yieldFormula, yieldVar ~.)

    ## Imputation
    ## ---------------------------------------------------------------------
    ## Step (1): Impute yield first
    dataCopy[, yieldVar := 
             imputeYield(formula = newYieldFormula, data = .SD,
                         ## NOTE (Michael): Need to add in weights here
                         index = index, weights = NULL)]
    ## Modify yield flag
    dataCopy[yieldMissIndex & !is.na(yieldVar),
             yieldObservationFlag := "I"]

    ## Step (2): Calculate production if area harvested and yield
    ##           exists.
    dataCopy[productionMissIndex,
             productionVar := areaHarvestedVar * yieldVar]
    ## Calculate the balanced production flag
    dataCopy[productionMissIndex & !is.na(productionVar),
             productionObservationFlag :=
             aggregateObservationFlag(areaHarvestedObservationFlag,
                                      yieldObservationFlag, flagTblae = flagTable)]
    
    ## Step (3): Impute production    
    dataCopy[, productionVar :=
        imputeProduction(data = .SD,
                         productionVar = "productionVar", index = index)]
    ## Modify imputed production flag
    dataCopy[productionMissIndex & !is.na(productionVar),
                      productionObservationFlag := "I"]
    
    ## Step (4): Balance area harvested
    dataCopy[, areaHarvestedVar := productionVar/yieldVar]    
    ## Modify balanced area harvested flag    
    dataCopy[areaHarvestedMissIndex & !is.na(areaHarvestedVar),
               areaHarvestedObservationFlag :=
               aggregateObservationFlag(productionObservationFlag,
                                        yieldObservationFlag,
                                        flagTable = flagTable)]

    ## ---------------------------------------------------------------------
    ## Rename the column
    setnames(dataCopy,
             old = c("productionVar", "areaHarvestedVar", "yieldVar",
                 "productionObservationFlag", "areaHarvestedObservationFlag",
                 "yieldObservationFlag"),
             new = c(productionVar, areaHarvestedVar, yieldVar,
                 productionObservationFlag, areaHarvestedObservationFlag,
                 yieldObservationFlag))
    dataCopy[, c("productionWeight", "areaHarvestedWeight", "yieldWeight") := NULL]
    
    dataCopy
}
