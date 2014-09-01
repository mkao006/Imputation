##' Function to remove zero values which are missing.
##'
##' @param data The data.table object containing the values.
##' @param value The value of the observation.
##' @param flag The flag of the observation.
##' @param naFlag The value of the flag which denotes missing value.
##'
##' @export

remove0M = function(data, value, flag, naFlag = "M"){
    missingIndex = which(data[[flag]] == naFlag)
    invisible(data[missingIndex, `:=`(c(value), list(NA))])
}
