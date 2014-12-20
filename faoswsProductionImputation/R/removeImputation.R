##' Function to remove prior imputation.
##'
##' @param data The data.table object containing the values.
##' @param value The value of the observation.
##' @param flag The flag of the observation.
##' @param imputedFlag The value of the flag which denotes the value
##' was imputed.
##' @param naFlag The value of the flag which denotes missing value.
##' 
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##'
##' @example data = copy(okrapd[areaName=="Sudan",])
##' data[,.(yieldValue, yieldFlag)]
##' removeImputation( data = data, value = "yieldValue", flag = "yieldFlag",
##'     imputedFlag = "T", naFlag = "M" )
##' data[,.(yieldValue, yieldFlag)]
##'
##' @export

removeImputation = function(data, value, flag, imputedFlag = "T",
    naFlag = "M"){
    
    stopifnot(is(data, "data.table"))
    stopifnot(value %in% colnames(data))
    stopifnot(flag %in% colnames(data))
    
    imputedIndex = which(data[[flag]] %in% imputedFlag)
    invisible(data[imputedIndex, `:=`(c(value, flag), list(NA, naFlag))])
}
