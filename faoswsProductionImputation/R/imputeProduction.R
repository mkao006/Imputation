##' Function to impute production
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param columnNames See the same argument at ?imputeProductionDomain
##' @param imputationFlag Flag value for new imputation values.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param modelExtrapolationRange See ?computeEnsembleWeight.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param errorType See ?computeErrorRate.
##' @param errorFunction See ?computeEnsembleWeight.
##' 
##' @export
##' 

imputeProduction = function(columnNames, imputationFlag = "I",
    newMethodFlag, data,
    restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModel = allDefaultModels(),
    modelExtrapolationRange = getDefaultRange(ensembleModel),
    flagTable = faoswsFlagTable,
    errorType = "loocv", errorFunction = function(x) mean(x^2) ){

    ### Ensure inputs are as expected (and assign columnNames variables)
    stopifnot( is(data, "data.table") )
    stopifnot( is.logical(restrictWeights) )
    # Ensure all elements of ensembleModel are functions
    stopifnot( all( sapply( ensembleModel, is.function ) ) )
    stopifnot( maximumWeights <= 1 & maximumWeights >= 0 )
    stopifnot( length(ensembleModel) == length(modelExtrapolationRange) )
    stopifnot( errorType %in% c("loocv", "raw") )
    stopifnot( is( errorFunction, "function" ) )
    testColumnNames( columnNames = columnNames, data = data)
    assignColumnNames( columnNames = columnNames, data = data,
        environment = environment() )
	testFlagTable( flagTable = flagTable, data = data,
        columnNames = columnNames )

    ## By balancing first
    balanceProduction(columnNames,
                      newMethodFlag = newMethodFlag,
                      data = data,
                      flagTable = flagTable)

    ## Then imputation by ensemble
    setnames(x = data,
             old = c(productionValue, productionObservationFlag,
                 productionMethodFlag),
             new = c("productionValue", "productionObservationFlag",
                 "productionMethodFlag"))

    productionMissingIndex = is.na(data[, productionValue])
    data[, productionValue :=
         ensembleImpute(productionValue,
                        ensembleModel = ensembleModel,
                        modelExtrapolationRange = modelExtrapolationRange,
                        plot = FALSE, errorType = errorType,
                        errorFunction = errorFunction),
         by = byKey]
    data[productionMissingIndex & !is.na(productionValue),
         c("productionObservationFlag", "productionMethodFlag") :=
         list(imputationFlag, newMethodFlag)]
    
    setnames(x = data,
             old = c("productionValue", "productionObservationFlag",
                 "productionMethodFlag"),
             new = c(productionValue, productionObservationFlag,
                 productionMethodFlag))
}
