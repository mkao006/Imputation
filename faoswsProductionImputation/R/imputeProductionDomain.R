##' This function imputes the whole production domain.
##'
##' The function will impute production, area harvested and yield at
##' the same time.
##'
##' Transformation in the yield formula is not allowed and will not be
##' taken into account.
##'
##' @param data The data
##' @param columnNames A named character vector.  This argument specifies what
##' each of the relevant columns of data correspond to.  The values of this
##' vector should correspond to column names of data, and the names of this
##' vector should be productionValue, productionObservationFlag,
##' productionMethodFlag, areaHarvestedValue, areaHarvestedObservationFlag,
##' areaHarvestedMethodFlag, yieldValue, yieldObservationFlag, yieldMethodFlag,
##' yearValue, and byKey.  Ordering of this vector is not important, and it may
##' have additional elements, but it must have the elements listed above.  The
##' defaultColumnNames function may be helpful in setting up a starting point.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param removePriorImputation logical, whether prior imputation
##' should be removed.
##' @param removeConflictValues logical, whether conflict area
##' harvested value and production should be removed.
##' @param imputedFlag Flag value corresponding to values from prior
##' imputation, ignored if removePriorImputation is FALSE.
##' @param imputationFlag Flag value for new imputation values.
##' @param naFlag Flag value for missing values.
##' @param maxdf The maximum degree of freedom for the spline. 
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param yieldFormula The formula to be passed to the linear mixed
##' model for the imputation of yield, if missing default spline model
##' is fitted.
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param modelExtrapolationRange See ?computeEnsembleWeight.
##' @param errorType See ?computeErrorRate.
##' @param errorFunction See ?computeEnsembleWeight.
##'
##' @export
##' 

imputeProductionDomain = function(data, columnNames,
    flagTable = faoswsFlagTable,
    removePriorImputation = TRUE, removeConflictValues = TRUE,
    imputedFlag = "E", imputationFlag = "I", newMethodFlag = "",
    naFlag = "M", maxdf = 5, 
    restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModel = allDefaultModels(),
    modelExtrapolationRange = getDefaultRange(ensembleModel),
    yieldFormula,
    errorType = "loocv", errorFunction = function(x) mean(x^2) ){

    ### Ensure inputs are as expected (and assign columnNames variables)
    stopifnot( is(data, "data.table") )
    testColumnNames( columnNames = columnNames, data = data)
    stopifnot( is.logical( 
        c(removePriorImputation, removeConflictValues, restrictWeights) ) )
    # Ensure all elements of ensembleModel are functions
    stopifnot( all( sapply( ensembleModel, is.function ) ) )
    stopifnot( maximumWeights <= 1 & maximumWeights >= 0 )
    stopifnot( length(ensembleModel) == length(modelExtrapolationRange) )
    stopifnot( errorType %in% c("loocv", "raw") )
    stopifnot( is( errorFunction, "function" ) )
    assignColumnNames( columnNames = columnNames, data = data,
        environment = environment() )
	testFlagTable( flagTable = flagTable, data = data,
        columnNames = columnNames )
        
    cat("Initializing ... \n")
    dataCopy = copy(data)
    setkeyv(x = dataCopy, cols = c(byKey, yearValue))
    oldColumnNames = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     yearValue,
                     productionObservationFlag,
                     areaHarvestedObservationFlag,
                     yieldObservationFlag,
                     productionMethodFlag,
                     areaHarvestedMethodFlag,
                     yieldMethodFlag,
                     byKey
                     )
    newColumnNames = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "yearValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag",
                     "yieldObservationFlag",
                     "productionMethodFlag",
                     "areaHarvestedMethodFlag",
                     "yieldMethodFlag",
                     "byKey")
    # Assign names to newColumnNames so it can be passed to columnNames later
    names( newColumnNames ) = newColumnNames
    setnames(x = dataCopy,
             old = oldColumnNames,
             new = newColumnNames
             )

    ## These should be documented and checked.  Make sure the types
    ## are correct
    dataCopy[, productionValue := as.numeric(productionValue)]
    dataCopy[, areaHarvestedValue := as.numeric(areaHarvestedValue)]
    dataCopy[, yieldValue := as.numeric(yieldValue)]

    dataCopy =
        processProductionDomain(data = dataCopy,
                                columnNames = newColumnNames,
                                removePriorImputation =
                                    removePriorImputation,
                                removeConflictValues =
                                    removeConflictValues,
                                imputedFlag = imputedFlag,
                                naFlag = naFlag )    

    ## Step two: Impute Yield
    cat("Imputing Yield ...\n")
    n.missYield = length(which(is.na(dataCopy$yieldValue)))
    if(!missing(yieldFormula))
        yieldFormula =
            as.formula(gsub(yearValue, "yearValue",
                            gsub(yieldValue, "yieldValue",
                                 deparse(yieldFormula))))
    
    imputeYield(columnNames = newColumnNames,
                imputationFlag = imputationFlag,
                newMethodFlag = newMethodFlag,
                maxdf = maxdf,
                data = dataCopy,
                yieldFormula = yieldFormula,
                flagTable = flagTable)
    n.missYield2 = length(which(is.na(dataCopy$yieldValue)))
    cat("Number of values imputed: ", n.missYield - n.missYield2, "\n")
    cat("Number of values still missing: ", n.missYield2, "\n")

    ## step three: Impute production
    cat("Imputing Production ...\n")
    n.missProduction = length(which(is.na(dataCopy$productionValue)))

    imputeProduction(columnNames = newColumnNames,
                     newMethodFlag = newMethodFlag,
                     data = dataCopy,
                     ensembleModel = ensembleModel,
                     modelExtrapolationRange = modelExtrapolationRange,
                     restrictWeights = restrictWeights,
                     maximumWeights = maximumWeights,
                     flagTable = flagTable,
                     errorType = errorType,
                     errorFunction = errorFunction)

    n.missProduction2 = length(which(is.na(dataCopy$productionValue)))
    cat("Number of values imputed: ",
        n.missProduction - n.missProduction2, "\n")
    cat("Number of values still missing: ", n.missProduction2, "\n")

    ## step four: balance area harvested
    cat("Imputing Area Harvested ...\n")
    n.missAreaHarvested =
        length(which(is.na(dataCopy$areaHarvestedValue)))

    balanceAreaHarvested(newColumnNames, 
                         newMethodFlag = newMethodFlag,
                         data = dataCopy,
                         flagTable = flagTable)

    n.missAreaHarvested2 =
        length(which(is.na(dataCopy$areaHarvestedValue)))
    cat("Number of values imputed: ",
        n.missAreaHarvested - n.missAreaHarvested2, "\n")
    cat("Number of values still missing: ", n.missAreaHarvested2, "\n")
    
    ## Reset the names
    setnames(x = dataCopy,
             old = newColumnNames,
             new = newColumnNames
             )

    dataCopy
}
