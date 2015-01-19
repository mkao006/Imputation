##' Extend Simple Model
##'
##' This function takes any model which is designed to run on a simple time
##' series (i.e. at the commodity-country level) and applies it to each country
##' in turn, thus making it a commodity level model.
##'
##' @param data The data.table object containing the data.
##' @param model The model to be applied to each individual time series.  
##' Typically, this will be a function such as one from allDefaultModels().
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point. If
##' NULL, the parameters should have already been assigned (otherwise an error
##' will occur).
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

extendSimpleModel = function(data, model, imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned)
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    
    setnames(data, old = imputationValueColumn, new = "imputationValueColumn")
    missingIndex = is.na(data[, imputationValueColumn])
    modelFit = data[,
        # Apply the model if there is a missing value.  Else, return the data
        if(any(is.na(imputationValueColumn))){
            model(imputationValueColumn)
        } else {
            imputationValueColumn
        }, by = byKey]
    setnames(data, old = "imputationValueColumn", new = imputationValueColumn)
    return(modelFit$V1)
}
