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
##' @param environment The functions in this package generally work by
##' passing the data.table by reference and modifying it in place.  In place
##' row deletion is currently not supported by data.table.  Thus, to make this
##' function consistent with the other functions in this package, environment
##' is passed to specify where the modified data.table should be placed.  The
##' default of parent.frame(1) gives the calling environment and this should be
##' satisfactory for most, if not all, use cases.
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##'
##' @export
##' 

removeNoInfo = function (data, value, flag, processingParameters,
    environment = parent.frame(1)){
    
    ### Data Quality Checks
    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)
    stopifnot(c(value, flag) %in% colnames(data))
    stopifnot(is(environment, "environment"))
    
    info = data[, rep(containInfo(value = get(value), flag = get(flag),
                                  processingParameters = processingParameters),
        NROW(.SD)), by = c(processingParameters$byKey)]$V1
    
    #Assign the new data.table to environment
    dataTableName = as.character(match.call()$data)
    assign(x = dataTableName, value = data[info,],
        envir = environment)
}
