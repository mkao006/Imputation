##' Function to compute the fits of all the component models
##'
##' @param data The data.table object containing the data.
##' @param commodityCountryModels A list of models that should be applied to
##' each individual country/commodity time series separately.   Defaults to
##' allDefaultModels().
##' @param commodityModels A list of models to be applied to all countries at
##' once.
##' @param columnNames See the same argument at ?imputeProductionDomain
##' @param value Which variable should be imputed?  This should be one of the
##' column names of data, typically corresponding to productionValue,
##' yieldValue, or areaHarvestedValue.
##' @param flag Column name (of data) of the flag variable corresponding to the
##' value argument, i.e. "productionObservationFlag", "yieldObservationFlag",
##' or "areaHarvestedObservationFlag".
##' 
##' @export
##' 

computeEnsembleFit = function(data, ensembleModels = allDefaultModels(),
    columnNames, value, flag){
    
    ### Data quality checks
    stopifnot( is(data, "data.table") )
    testColumnNames( columnNames = columnNames, data = data)
    stopifnot( c(value, flag) %in% colnames(data) )
    stopifnot( is(ensembleModels, "list") )
    stopifnot( all( sapply( ensembleModels, is ) == "ensembleModel" ) )
    assignColumnNames( columnNames = columnNames, environment = environment() )
    
    ### Fit Models
    fits = lapply(ensembleModels,
        FUN = function(model){
            if(model@level=="commodity"){
                model@model(data = data, value = value, flag = flag,
                    columnNames = columnNames )
            } else if(model@level=="countryCommodity"){
                extendSimpleModel(data = data, model = model@model,
                    value = value, flag = flag, columnNames = columnNames )                
            }
        })
    return(fits)
}
