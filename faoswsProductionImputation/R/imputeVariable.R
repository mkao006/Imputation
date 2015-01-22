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
    stopifnot(!is.null(imputationParameters))
    assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)

    ## By balancing first, if variable == "production"
    if(imputationParameters$variable == "production"){
        balanceProduction(data = data)
    }

    missingIndex = is.na(data[, get(imputationValueColumn)])
    data[, c(imputationValueColumn) := ensembleImpute(data = data)]
    data[missingIndex & !is.na(get(imputationValueColumn)),
         c(imputationFlagColumn, imputationMethodColumn) :=
         list(imputationFlag, newMethodFlag)]
}
