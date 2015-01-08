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
##' @param newMethodFlag The character value that should be assigned to
##' *ObservationFlag when *Value is imputed (*=production, yield, or
##' areaHarvested).
##' @param naFlag Flag value for missing values.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param ensembleModelsYield A list of models to be used to build the
##' ensemble for the yield variable.
##' @param ensembleModelsProduction A list of models to be used to build the
##' ensemble for the production variable.
##' @param errorType See ?computeErrorRate.
##' @param errorFunction See ?computeEnsembleWeight.
##'
##' @export
##' 

imputeProductionDomain = function(data, processingParameters,
                                  yieldImputationParameters,
                                  productionImputationParameters){

    ### Data Quality Checks
    assignParameters(processingParameters)
    ensureData(data = data)
    assignParameters(yieldImputationParameters)
    ensureData(data = data)
    ensureFlagTable(flagTable = flagTable, data = data)
    assignParameters(productionImputationParameters)
    ensureData(data = data)
    ensureFlagTable(flagTable = flagTable, data = data)
    stopifnot(yieldImputationParameters$variable == "yield")
    stopifnot(productionImputationParameters$variable == "production")
    
    cat("Initializing ... \n")
    assignParameters(processingParameters)
    dataCopy = copy(data)
    setkeyv(x = dataCopy, cols = c(byKey, yearValue))
    processProductionDomain(data = dataCopy,
                            processingParameters = processingParameters)

    ## Step two: Impute Yield
    cat("Imputing Yield ...\n")
    n.missYield = length(which(is.na(dataCopy[[yieldValue]])))
#     if(!missing(yieldFormula))
#         yieldFormula =
#             as.formula(gsub(yearValue, "yearValue",
#                             gsub(yieldValue, "yieldValue",
#                                  deparse(yieldFormula))))
    
    imputeVariable(data = dataCopy,
                   imputationParameters = yieldImputationParameters)
    n.missYield2 = length(which(is.na(dataCopy[[yieldValue]])))
    cat("Number of values imputed: ", n.missYield - n.missYield2, "\n")
    cat("Number of values still missing: ", n.missYield2, "\n")

    ## step three: Impute production
    cat("Imputing Production ...\n")
    n.missProduction = length(which(is.na(dataCopy[[productionValue]])))

    imputeVariable(data = dataCopy,
                   imputationParameters = productionImputationParameters)

    n.missProduction2 = length(which(is.na(dataCopy[[productionValue]])))
    cat("Number of values imputed: ",
        n.missProduction - n.missProduction2, "\n")
    cat("Number of values still missing: ", n.missProduction2, "\n")

    ## step four: balance area harvested
    cat("Imputing Area Harvested ...\n")
    n.missAreaHarvested =
        length(which(is.na(dataCopy[[areaHarvestedValue]])))

    balanceAreaHarvested(data = dataCopy)

    n.missAreaHarvested2 =
        length(which(is.na(dataCopy$areaHarvestedValue)))
    cat("Number of values imputed: ",
        n.missAreaHarvested - n.missAreaHarvested2, "\n")
    cat("Number of values still missing: ", n.missAreaHarvested2, "\n")
    
    dataCopy
}
