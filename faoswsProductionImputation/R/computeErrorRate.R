##' Function to compute the error of different model fits
##'
##' @param data A data.table containing the data.
##' @param value The column name of data which contains the values to be
##' imputed.
##' @param flag The column name of data which contains the flag describing the
##' status of value.
##' @param model The model fit to x, should be of class ensembleModel.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param fit The fitted value from the model.
##' @param errorType Specifies what type of error to compute.  Currently, "raw"
##' and "loocv" are implemented.  If "raw", then error is computed as the
##' difference between the model and observed data.  "loocv" performs
##' leave-one-out cross-validation to determine the predictive error of the
##' model.  "loocv" is more rigorous but much slower.
##' 
##' @return A vector of (the absolute value of the) errors corresponding to the
##' non-missing entries in data's "value" column.
##' 
##' @export
##' 

computeErrorRate = function(data, model = NULL, cvGroup, fit,
                            imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    
    ### Run the function:
    x = data[[imputationValueColumn]]
    if(all(is.na(x - fit))){
        er = rep(NA, length.out = nrow(data))
    } else {
        if(errorType == "raw")
            er = (x - fit)
        if(errorType == "loocv")
            er = computeErrorLOOCV(data = data, model = model, cvGroup = cvGroup)
    }
    abs(er)
}