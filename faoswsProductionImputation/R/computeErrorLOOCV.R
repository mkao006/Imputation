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

computeErrorLOOCV = function(data, columnNames, value, flag, model, cvGroup){

    ### Data Quality Checks
    stopifnot(c(value, flag) %in% colnames(data))
    stopifnot(is(model, "ensembleModel"))
    stopifnot(is.numeric(cvGroup))
    stopifnot(length(cvGroup)==nrow(data))
    testColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames)

    setnames(data, old = c(value, flag), new = c("value", "flag"))
    error = rep(0, nrow(data))
    for(i in 1:length(unique(cvGroup))){
        #Copy x and remove the ith observation to fit the model
        dataTemporary = copy(data)
        dataTemporary[cvGroup == i, value := NA]
        columnNamesTemporary = columnNames
        columnNamesTemporary[columnNames == value] = "value"
        columnNamesTemporary[columnNames == flag] = "flag"        
        if(model@level == "commodity"){
            fitTemporary = model@model(data = dataTemporary, value = "value",
                flag = "flag", columnNames = columnNamesTemporary)
        } else if(model@level == "countryCommodity"){
            fitTemporary = extendSimpleModel(data = dataTemporary,
                model = model@model, value = "value", flag = "flag",
                columnNames = columnNamesTemporary)
        }
        filter = !is.na(cvGroup) & cvGroup == i
        error[filter] = (data[, value] - fitTemporary)[filter]
    }
    setnames(data, old = c("value", "flag"), new = c(value, flag) )
    return(error)
}