##' Function to compute the fits of all the component models
##'
##' @param data The data.table object containing the data.
##' @param ensembleModels A list of the models fit to data.  Each element
##' should be of class ensembleModel.
##' @param columnNames See the same argument at ?imputeProductionDomain
##' @param value Which variable should be imputed?  This should be one of the
##' column names of data, typically corresponding to productionValue,
##' yieldValue, or areaHarvestedValue.
##' @param flag Column name (of data) of the flag variable corresponding to the
##' value argument, i.e. "productionObservationFlag", "yieldObservationFlag",
##' or "areaHarvestedObservationFlag".
##' 
##' @return Returns a list of vectors of the same length as ensembleModels. The
##' ith element of the list represents the fit of the ith model to data.
##' 
##' @export
##' 

computeEnsembleFit = function(data, ensembleModels = allDefaultModels(),
    columnNames, value, flag){
    
    ### Data quality checks
    stopifnot(is(data, "data.table"))
    stopifnot(c(value, flag) %in% colnames(data))
    stopifnot(is(ensembleModels, "list"))
    stopifnot(all(sapply(ensembleModels, is) == "ensembleModel"))
    testColumnNames(columnNames = columnNames, data = data)
    assignColumnNames(columnNames = columnNames, environment = environment())
    
    ### Fit Models
    fits = lapply(ensembleModels,
        FUN = function(model){
            if(model@level=="commodity"){
                model@model(data = data, value = value, flag = flag,
                    columnNames = columnNames)
            } else if(model@level=="countryCommodity"){
                extendSimpleModel(data = data, model = model@model,
                    value = value, flag = flag, columnNames = columnNames)
            }
        })
    return(fits)
}
