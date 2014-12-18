##' This function removes data with no information
##'
##' The information contained within a set of index is calculated
##' based on whether a non-zero value exist. If the data contains only
##' zero and missing value then it is considered to contain no
##' imformation for imputation.
##'
##' @param data The data
##' @param value The value of the production.
##' @param flag The flag/symbol of production.
##' @param byKey The unique keys which identify groups.
##' @param environment The functions in this package generally work by
##' passing the data.table by reference and modifying it in place.  In place
##' row deletion is currently not supported by data.table.  Thus, to make this
##' function consistent with the other functions in this package, environment
##' is passed to specify where the modified data.table should be placed.  The
##' default of parent.frame(1) gives the calling environment and this should be
##' satisfactory for most, if not all, use cases.
##'
##' @export
##' 

removeNoInfo = function (data, value, flag, byKey,
    environment = parent.frame(1)){
    info = data[, rep(containInfo(get(value), get(flag)),
        NROW(.SD)), by = byKey]$V1
    
    #Assign the new data.table to environment
    dataTableName = as.character( match.call()$data )
    assign( x = dataTableName, value = data[info,],
        envir = environment )
}
