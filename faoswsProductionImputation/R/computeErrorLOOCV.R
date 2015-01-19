##' LOOCV Error Rate
##' 
##' This function computes the error of different model fits via leave-one-out
##' cross-validation.  However, typically this function will be called via
##' computeErrorRate and not directly.
##'
##' @param data A data.table containing the data.
##' @param columnNames See the same argument at ?imputeProductionDomain.
##' @param value The column name of data which contains the values to be
##' imputed.
##' @param flag The column name of data which contains the flag describing the
##' status of value.
##' @param model The model fit to x, should be of class ensembleModel.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' 
##' @export

computeErrorLOOCV = function(data, model, cvGroup,
                             imputationParameters = NULL){

    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned)
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    stopifnot(is(model, "ensembleModel"))
    stopifnot(is.numeric(cvGroup))
    stopifnot(length(cvGroup)==nrow(data))
    stopifnot(length(unique(cvGroup)) > 1)

    error = rep(0, nrow(data))
    for(i in 1:length(unique(cvGroup))){
        #Copy x and remove the ith observation to fit the model
        dataTemporary = copy(data)
        setnames(dataTemporary, old = imputationValueColumn,
                 new = "imputationValueColumn")
        dataTemporary[cvGroup == i, imputationValueColumn := NA]
        setnames(dataTemporary, old = "imputationValueColumn",
                 new = imputationValueColumn)
        if(model@level == "commodity"){
            fitTemporary = model@model(data = dataTemporary)
        } else if(model@level == "countryCommodity"){
            fitTemporary = extendSimpleModel(data = dataTemporary,
                model = model@model)
        }
        filter = !is.na(cvGroup) & cvGroup == i
        error[filter] = (data[[imputationValueColumn]] - fitTemporary)[filter]
    }
    return(error)
}
