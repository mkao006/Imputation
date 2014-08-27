##' Function to impute production
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionFlag The column name corresponding to the
##' observation flag of production.
##' @param areaHarvestedValue The column name corresponding to area
##' harvested value.
##' @param areaHarvestedFlag The column name corresponding to the
##' observation flag of area harvested.
##' @param yieldValue The columne name corresponding to yield value.
##' @param yieldFlag The column name corresponding to the observation
##' flag of yield.
##' @param imputationFlag Flag value for new imputation values.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param byKey The unique key identifier.
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' 
##' @export
##' 

imputeProduction = function(productionValue, productionFlag,
    areaHarvestedValue, areaHarvestedFlag, yieldValue, yieldFlag,
    imputationFlag = "I", data, byKey, restrictWeights = TRUE,
    maximumWeights = 0.7,
    ensembleModel = list(defaultMean, defaultLm, defaultExp,
        defaultLogistic, defaultLoess, defaultSpline, defaultArima,
        defaultMars, defaultNaive), flagTable = faoswsFlagTable){


    ## By balancing first
    balanceProduction(productionValue = productionValue,
                      productionFlag = productionFlag,
                      areaHarvestedValue = areaHarvestedValue,
                      areaHarvestedFlag = areaHarvestedFlag,
                      yieldValue = yieldValue,
                      yieldFlag = yieldFlag,
                      data = data,
                      flagTable = flagTable)

    ## Then imputation by ensemble
    setnames(x = data,
             old = c(productionValue, productionFlag),
             new = c("productionValue", "productionFlag"))

    productionMissingIndex = is.na(data[, productionValue])
    data[, productionValue :=
         ensembleImpute(productionValue,
                        ensembleModel = ensembleModel,
                        plot = FALSE),
         by = byKey]
    data[productionMissingIndex & !is.na(productionValue),
         productionFlag := imputationFlag]
    
    setnames(x = data,
             old = c("productionValue", "productionFlag"),
             new = c(productionValue, productionFlag))
}
