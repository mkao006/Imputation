##' Function to compute the fitted values from Leave One Out Cross Validation
##'
##' @param data A data.table containing the data.
##' @param cvGroup A vector of the same length as nrow(data).  Entries of the
##' vector should be integers from 1 to the number of cross-validation groups
##' (typically 10).  This should be randomly assigned, and is usually created
##' by ensembleImpute.
##' @param imputationParameters Specifies the imputation parameters to use, see
##' ?defaultImputationParameters.  If NULL, whatever currently exists will be
##' used.
##' 
##' @return A list of the same length as ensembleModels which contains the
##' fitted values corresponding to the loocv procedure.  The ith element of the
##' list corresponds to the fitting of the ith element of ensembleModels.
##' 
##' @export
##' 

computeLoocvFits = function(data, cvGroup, imputationParameters = NULL){
    
    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned)
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureData(data = data)
    stopifnot(length(cvGroup) == nrow(data))
    
    fits = lapply(1:length(ensembleModels), FUN = function(i){
        model = ensembleModels[[i]]
        fit = rep(NA, nrow(data))
        for(j in 1:length(unique(cvGroup[!is.na(cvGroup)]))){
            #Copy x and remove the ith observation to fit the model
            dataTemporary = copy(data)
            setnames(dataTemporary, old = imputationValueColumn,
                     new = "imputationValueColumn")
            dataTemporary[cvGroup == j, imputationValueColumn := NA]
            setnames(dataTemporary, old = "imputationValueColumn",
                     new = imputationValueColumn)
            if(model@level == "commodity"){
                fitTemporary = model@model(data = dataTemporary)
            } else if(model@level == "countryCommodity"){
                fitTemporary = extendSimpleModel(data = dataTemporary,
                                                 model = model@model)
            }
            filter = !is.na(cvGroup) & cvGroup == j
            fit[filter] = fitTemporary[filter]
        }
        return(fit)
    })
    names(fits) = names(ensembleModels)
    return(fits)
}
