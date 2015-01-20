##' Function to compute the error of different model fits
##'
##' @param data A data.table containing the data.
##' @param fit The fitted value from the model.
##' 
##' @return A vector of (the absolute value of the) errors corresponding to the
##' non-missing entries in data's "value" column.
##' 
##' @export
##' 

computeErrorRate = function(data, fit){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned)
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    
    ### Run the function:
    x = data[[imputationValueColumn]]
    if(all(is.na(x - fit))){
        er = rep(NA, length.out = nrow(data))
    } else {
        er = (x - fit)
    }
    abs(er)
}
