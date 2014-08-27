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
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param yieldFormula The formula for yield to be passed into the
##' linear mixed model.
##' @param removePriorImputation logical, whether prior imputation
##' should be removed.
##' @param removeConflictValues logical, whether conflict area
##' harvested value and production should be removed.
##' @param imputedFlag Flag value corresponding to values from prior
##' imputation, ignored if removePriorImputation is FALSE.
##' @param imputationFlag Flag value for new imputation values.
##' @param naFlag Flag value for missing values.
##' @param byKey The unique key identifier.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##'
##' @export
##' 

imputeProductionDomain = function(data, productionVar, areaHarvestedVar,
    yieldVar, productionObservationFlag, areaHarvestedObservationFlag,
    yieldObservationFlag, flagTable = faoswsFlagTable,
    yieldFormula = yieldValue ~ -1 + (1 + year|areaCode),
    removePriorImputation = TRUE, removeConflictValues = TRUE,
    imputedFlag = "E", imputationFlag = "I", naFlag = "M",
    byKey = "areaCode", restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModel = list(defaultMean, defaultLm, defaultExp,
        defaultLogistic, defaultLoess, defaultSpline, defaultArima,
        defaultMars, defaultNaive)){

    dataCopy = copy(data)
    setnames(x = dataCopy,
             old = c(productionVar,
                     areaHarvestedVar,
                     productionObservationFlag,
                     areaHarvestedObservationFlag),
             new = c("productionValue",
                     "areaHarvestedValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag")
             )

    ## These should be documented and checked.
    ## Make sure the types are correct
    dataCopy[, productionValue := as.numeric(productionValue)]
    dataCopy[, areaHarvestedValue := as.numeric(areaHarvestedValue)]
    
    ## Optional step: Remove prior imputation
    if(removePriorImputation){
        removeImputation(data = dataCopy,
                         value = "areaHarvestedValue",
                         flag = "areaHarvestedObservationFlag",
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
        
        removeImputation(data = dataCopy,
                         value = "productionValue",
                         flag = "productionObservationFlag",
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
    }
    ## print(colnames(dataCopy))
    ## print(table(dataCopy$productionObservationFlag))
    ## print(table(dataCopy$areaHarvestedObservationFlag))    
    
    ## Optional step: Remove prior imputation
    if(removePriorImputation){
        remove0M(data = dataCopy,
                 value = "areaHarvestedValue",
                 flag = "areaHarvestedObservationFlag",
                 naFlag = naFlag)
        
        remove0M(data = dataCopy,
                 value = "productionValue",
                 flag = "productionObservationFlag",
                 naFlag = naFlag)
    }
    
    ## Optional step: Compute yield if not available
    if(!yieldVar %in% colnames(dataCopy)){
        computeYield(productionValue = "productionValue",
                     productionFlag = "productionObservationFlag",
                     areaHarvestedValue = "areaHarvestedValue",
                     areaHarvestedFlag = "areaHarvestedObservationFlag",
                     yieldValue = yieldVar,
                     yieldFlag = yieldObservationFlag,
                     flagTable = flagTable,
                     data = dataCopy)
    }
    setnames(x = dataCopy, old = c(yieldVar, yieldObservationFlag),
             new = c("yieldValue", "yieldObservationFlag"))
    ## print(table(dataCopy$yieldObservationFlag))

    ## Optional step: Remove conflict values
    if(removeConflictValues){
        removeZeroConflict(productionValue = "productionValue",
                           productionFlag = "productionObservationFlag",
                           areaHarvestedValue = "areaHarvestedValue",
                           areaHarvestedFlag =
                               "areaHarvestedObservationFlag",
                           yieldValue = "yieldValue",
                           yieldFlag = "yieldObservationFlag",
                           data = dataCopy)
    }
    
    ## Step one: Remove country with no info.
    dataCopy =
        removeNoInfo(data = dataCopy,
                     flag = "yieldObservationFlag",
                     value = "yieldValue",
                     byKey = byKey)

    ## Step two: Impute Yield
    ## print(table(dataCopy$yieldObservationFlag))
    ## print(table(is.na(dataCopy$yieldValue)))
    imputeYield(formula = yieldFormula,
                yieldObservationFlag = "yieldObservationFlag",
                imputationFlag = imputationFlag,
                byKey = byKey,
                data = dataCopy)
    ## print(table(dataCopy$yieldObservationFlag))
    ## print(table(is.na(dataCopy$yieldValue)))

    ## print(table(dataCopy$productionObservationFlag))
    ## print(table(is.na(dataCopy$productionValue)))
    ## step three: Impute production
    imputeProduction(productionValue = "productionValue",
                     productionFlag = "productionObservationFlag",
                     areaHarvestedValue = "areaHarvestedValue",
                     areaHarvestedFlag = "areaHarvestedObservationFlag",
                     yieldValue = "yieldValue",
                     yieldFlag = "yieldObservationFlag",
                     data = dataCopy,
                     ensembleModel = ensembleModel,
                     restrictWeights = restrictWeights,
                     maximumWeights = maximumWeights,
                     byKey = byKey,
                     flagTable = flagTable)
    ## print(table(dataCopy$productionObservationFlag))
    ## print(table(is.na(dataCopy$productionValue)))
    
    ## print(table(dataCopy$areaHarvestedObservationFlag))
    ## print(table(is.na(dataCopy$areaHarvestedValue)))    
    ## step four: balance area harvested
    balanceAreaHarvested(productionValue = "productionValue",
                         productionFlag = "productionObservationFlag",
                         areaHarvestedValue = "areaHarvestedValue",
                         areaHarvestedFlag = "areaHarvestedObservationFlag",
                         yieldValue = "yieldValue",
                         yieldFlag = "yieldObservationFlag",
                         data = dataCopy,
                         flagTable = flagTable)
    ## print(table(dataCopy$areaHarvestedObservationFlag))
    ## print(table(is.na(dataCopy$areaHarvestedValue)))
    
    setnames(x = dataCopy,
             old = c("productionValue",
                 "areaHarvestedValue",
                 "yieldValue",
                 "productionObservationFlag",
                 "areaHarvestedObservationFlag",
                 "yieldObservationFlag"),
             new = c(productionVar,
                 areaHarvestedVar,
                 yieldVar,
                 productionObservationFlag,
                 areaHarvestedObservationFlag,
                 yieldObservationFlag)             
             )

    dataCopy
}
