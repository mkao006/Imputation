##' Function to perform ensemble imputation
##'
##' This is an implementation of the ensemble imputation methodology
##' developed for the FAO production domain.
##'
##' @param data A data.table containing the data.
##' @param columnNames See the same argument at ?imputeProductionDomain.
##' @param value The column name of data which contains the values to be
##' imputed.
##' @param flag The column name of data which contains the flag describing the
##' status of value.
##' @param ensembleModels A list of the models fit to data.  Each element
##' should be of class ensembleModel.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].  See ?computeEnsembleWeight for more details.
##' @param plot Whether the result of the ensemble should be plotted.
##' @param errorType See ?computeErrorRate.
##' @param errorFunction See ?computeEnsembleWeight.  Defaults to MSE.
##' @param missingFlag What value of the flag variable represents a missing
##' value?  Defaults to "M".
##'
##' @export
##' 

ensembleImpute = function(data, imputationParameters = NULL){

    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
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
