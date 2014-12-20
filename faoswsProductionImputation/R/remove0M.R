##' Function to remove zero values which are missing.
##'
##' @param data The data.table object containing the values.
##' @param value The value of the observation.
##' @param flag The flag of the observation.
##' @param naFlag The value of the flag which denotes missing value.
##' 
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##' 
##' @example data = okrapd[areaName == "Bahamas",]
##' data[,.(productionValue, productionFlag)]
##' remove0M(data = data, value = "productionValue", flag = "productionFlag",
##'     naFlag = "M")
##' data[,.(productionValue, productionFlag)]
##' 
##' @export

remove0M = function(data, value, flag, naFlag = "M"){
    stopifnot(is(data, "data.table"))
    stopifnot(value %in% colnames(data) )
    stopifnot(flag %in% colnames(data) )
    
    missingIndex = which(data[[flag]] == naFlag)
    missingValues = data[missingIndex,][[value]]
    if(any(!is.na(missingValues) & missingValues!=0 ))
        stop("Some missing values are not 0 or NA!")
    invisible(data[missingIndex, `:=`(c(value), list(NA))])
}
