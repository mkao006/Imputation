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

imputeVariable = function(data, imputationParameters){

    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)

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
                 new = c("imputationValueColumn", "observationFlag",
                         "methodFlag"))
        columnNames["productionValue"]           = "imputationValueColumn"
        columnNames["productionObservationFlag"] = "observationFlag"
        columnNames["productionMethodFlag"]      = "methodFlag"
    } else if(variable == "yield"){
        setnames(x = data,
                 old = c(yieldValue, yieldObservationFlag,
                         yieldMethodFlag),
                 new = c("imputationValueColumn", "observationFlag",
                         "methodFlag"))
        columnNames["yieldValue"]           = "imputationValueColumn"
        columnNames["yieldObservationFlag"] = "observationFlag"
        columnNames["yieldMethodFlag"]      = "methodFlag"
    }

    missingIndex = is.na(data[, imputationValueColumn])
    data[, imputationValue := ensembleImpute(data = data)]
    data[missingIndex & !is.na(imputationValueColumn),
         c("observationFlag", "methodFlag") :=
         list(imputationFlag, newMethodFlag)]
    
    if(variable == "production"){
        setnames(x = data,
                 old = c("imputationValueColumn", "observationFlag", "methodFlag"),
                 new = c(productionValue, productionObservationFlag,
                     productionMethodFlag))
    } else if(variable == "yield"){
        setnames(x = data,
                 old = c("imputationValueColumn", "observationFlag", "methodFlag"),
                 new = c(yieldValue, yieldObservationFlag, yieldMethodFlag))
    }
}
