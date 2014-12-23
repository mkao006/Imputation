##' Extend Simple Model
##'
##' This function takes any model which is designed to run on a simple time
##' series (i.e. at the commodity-country level) and applies it to each country
##' in turn, thus making it a commodity level model.
##'
##' @param data The data.table object containing the data.
##' @param model The model to be applied to each individual time series.  
##' Typically, this will be a function such as one from allDefaultModels().
##' @param value Which variable should be imputed?  This should be one of the
##' column names of data, typically corresponding to productionValue,
##' yieldValue, or areaHarvestedValue.
##' @param flag Column name (of data) of the flag variable corresponding to the
##' value argument, i.e. "productionObservationFlag", "yieldObservationFlag",
##' or "areaHarvestedObservationFlag".
##' @param columnNames See the same argument at ?imputeProductionDomain.
##' 
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##' 
##' @export
##' 

extendSimpleModel = function(data, model, value, flag, columnNames){
    
    ### Data quality checks
    stopifnot( is(data, "data.table") )
    stopifnot( is(model, "function") )
    stopifnot( c(value, flag) %in% colnames(data) )
    testColumnNames( columnNames = columnNames, data = data)
    assignColumnNames( columnNames = columnNames, environment = environment() )
    
    setnames( data, old=value, new="value")
    missingIndex = is.na(data[,value])
    modelFit = data[,model(value), by=byKey]
    setnames( data, old="value", new=value)
    return(modelFit$V1)
}