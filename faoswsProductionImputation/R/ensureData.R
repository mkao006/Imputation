##' Ensure Data
##'
##' This function is designed to ensure that the provided dataset is valid.  In
##' particular, it coerces column types: all values are coerced to numeric (
##' instead of integer, which can cause problems) and all flags are coerced to
##' character (instead of logical, which occurs if the flag is set to NA).
##' Also, it ensures data is a data.table.
##'
##' @param data A data.table containing the data.
##' @param columnNames See the same argument at ?imputeProductionDomain.
##'
##' @export
##' 

ensureData = function(data, columnNames){
    ensureColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames)
    # Convert columns to numeric:
    for(name in c(productionValue, areaHarvestedValue, yieldValue)){
        expr = substitute(x := as.numeric(data[[x]]), list(x = name))
        data[, eval(expr)]
    }
    for(name in c(productionObservationFlag, areaHarvestedObservationFlag,
                  yieldObservationFlag, productionMethodFlag,
                  areaHarvestedMethodFlag, yieldMethodFlag)){
        expr = substitute(x := as.character(data[[x]]), list(x = name))
        data[, eval(expr)]
    }
}