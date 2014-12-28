##' Function to impute production or yield
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param columnNames See the same argument at ?imputeProductionDomain
##' @param imputationFlag Flag value for new imputation values.
##' @param newMethodFlag The character value that should be assigned to
##' *ObservationFlag when it is imputed (where *=variable).
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param ensembleModels A list of models to be used to build the
##' ensemble.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param errorType See ?computeErrorRate.
##' @param errorFunction See ?computeEnsembleWeight.
##' @param variable The variable to impute.  Should be either "production" or
##' "yield".
##' 
##' @export
##' 

imputeVariable = function(columnNames, imputationFlag = "I",
    newMethodFlag, data, restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModels = allDefaultModels(), flagTable = faoswsFlagTable,
    errorType = "loocv", errorFunction = function(x) mean(x^2), variable){

    ### Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is.logical(restrictWeights))
    # Ensure all elements of ensembleModel are functions
    stopifnot(all(sapply(ensembleModels, is, "ensembleModel")))
    stopifnot(maximumWeights <= 1 & maximumWeights >= .5)
    stopifnot(errorType %in% c("loocv", "raw"))
    stopifnot(is(errorFunction, "function"))
    testColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames, environment = environment())
	testFlagTable(flagTable = flagTable, data = data,
        columnNames = columnNames)
    stopifnot(variable %in% c("production", "yield"))

    ## By balancing first, if variable == "production"
    if(variable == "production"){
        balanceProduction(columnNames,
                          newMethodFlag = newMethodFlag,
                          data = data,
                          flagTable = flagTable)
    }

    ## Then imputation by ensemble
    if(variable == "production"){
        setnames(x = data,
                 old = c(productionValue, productionObservationFlag,
                     productionMethodFlag),
                 new = c("value", "observationFlag", "methodFlag"))
        columnNames["productionValue"]           = "value"
        columnNames["productionObservationFlag"] = "observationFlag"
        columnNames["productionMethodFlag"]      = "methodFlag"
    } else if(variable == "yield"){
        setnames(x = data,
                 old = c(yieldValue, yieldObservationFlag,
                         yieldMethodFlag),
                 new = c("value", "observationFlag", "methodFlag"))
        columnNames["yieldValue"]           = "value"
        columnNames["yieldObservationFlag"] = "observationFlag"
        columnNames["yieldMethodFlag"]      = "methodFlag"
    }

    missingIndex = is.na(data[, value])
    data[, value :=
         ensembleImpute(data = data, columnNames = columnNames, 
                        value = "value",
                        flag = "observationFlag",
                        ensembleModels = ensembleModels,
                        restrictWeights = restrictWeights,
                        maximumWeights = maximumWeights,
                        plot = FALSE, errorType = errorType,
                        errorFunction = errorFunction)]
    data[missingIndex & !is.na(value),
         c("observationFlag", "methodFlag") :=
         list(imputationFlag, newMethodFlag)]
    
    if(variable == "production"){
        setnames(x = data,
                 old = c("value", "observationFlag", "methodFlag"),
                 new = c(productionValue, productionObservationFlag,
                     productionMethodFlag))
    } else if(variable == "yield"){
        setnames(x = data,
                 old = c("value", "observationFlag", "methodFlag"),
                 new = c(yieldValue, yieldObservationFlag, yieldMethodFlag))
    }
}
