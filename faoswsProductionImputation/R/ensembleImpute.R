##' Function to perform ensemble imputation
##'
##' This is an implementation of the ensemble imputation methodology
##' developed for the FAO production domain.
##'
##' @param data A data.table containing the data.
##' @param imputationParameters A list of the parameters necessary to perform
##' the imputation.  See ?defaultImputationParameters.
##'
##' @export
##' 

ensembleImpute = function(data, imputationParameters = NULL){

    ### Data Quality Checks
    if(!exists("parametersAssigned"))
        stopifnot(!is.null(imputationParameters))
    if(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    if(!ensuredData)
        ensureImputationData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    valueMissingIndex = is.na(data[[imputationValueColumn]])
    flagMissingIndex = (data[[imputationFlagColumn]] == missingFlag)
    # Ensure missing values agree with missing flags
    if(!all(valueMissingIndex == flagMissingIndex)){
        cat("Values that are NA: ", sum(valueMissingIndex), "\n")
        cat("Flags with missingFlag value: ", sum(flagMissingIndex), "\n")
        stop("Different missing values from flags/values!  Maybe call remove0M?")
    }
    if(is.null(names(ensembleModels)))
        names(ensembleModels) = paste("Model", 1:length(ensembleModels),
                                      sep = "_")
    if(!any(is.na(data[[imputationValueColumn]]))){
        warning("No missing values in data[[imputationValueColumn]].",
        "Returning data[[imputationValueColumn]]")
        return(data[[imputationValueColumn]])
    }
    
    ensemble = data[[imputationValueColumn]]
    missIndex = is.na(ensemble)
    cvGroup = makeCvGroup(data = data)
    modelFits = computeEnsembleFit(data = data)
    modelWeights = computeEnsembleWeight(data = data,
        cvGroup = cvGroup, fits = modelFits)
    ## print(modelWeights)
    ensembleFit = computeEnsemble(modelFits, modelWeights)
    ensemble[missIndex] = ensembleFit[missIndex]
    if(plotImputation){
        plotEnsemble(data = data, modelFits = modelFits,
                     modelWeights = modelWeights, ensemble = ensemble)
#         plotEnsembleOld(data, modelFits, modelWeights, ensemble)
    }
    ensemble
}
