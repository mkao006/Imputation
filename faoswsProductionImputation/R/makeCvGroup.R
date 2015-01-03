##' Make Cross-Validation Groups
##' 
##' Creates a vector of cross-validation groups to be used for leave-one-out
##' cross-validation in later models.
##' 
##' @param data A data.table object containing the data.
##' @param value The column name of data corresponding to the value to be
##' imputed.
##' @param byKey The column name of data corresponding to the grouping
##' variable, typically "areaCode".
##' @param groupCount How many cross-validation groups should be used?
##' 
##' @return A numeric vector taking values in 1:groupCount, or NA if the
##' corresponding observation is missing.  All groups are represented within
##' each individual timeseries (defined by byKey) when possible.
##' 

makeCvGroup = function(data){
    cvGroup = rep(NA, nrow(data))
    setnames(data, old = imputationValueColumn, new = "imputationValueColumn")
    cvGroup[!is.na(data[,imputationValueColumn])] =
        data[!is.na(imputationValueColumn),
             sampleEqually(n = .N, k = groupCount),
             by = byKey]$V1
    setnames(data, old = "imputationValueColumn", new = imputationValueColumn)
    return(cvGroup)
}