##' Function to compute the fits of all the component models
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point. If
##' NULL, the parameters should have already been assigned (otherwise an error
##' will occur).
##' 
##' @return Returns a list of vectors of the same length as ensembleModels. The
##' ith element of the list represents the fit of the ith model to data.
##' 
##' @export
##' 

computeEnsembleFit = function(data, imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned"))
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    
    ### Fit Models
    fits = lapply(ensembleModels,
        FUN = function(model){
            if(model@level == "commodity"){
                model@model(data = data)
            } else if(model@level == "countryCommodity"){
                extendSimpleModel(data = data, model = model@model)
            }
        })
    return(fits)
}