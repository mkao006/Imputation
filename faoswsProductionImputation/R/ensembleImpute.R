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

ensembleImpute = function(data, columnNames, value, flag,
    ensembleModels = allDefaultModels(), restrictWeights = TRUE,
    maximumWeights = 0.7, plot = FALSE, errorType = "loocv",
    errorFunction = function(x) mean(x^2), missingFlag = "M"){

    ### Data quality checks
    ensureData(data = data, columnNames = columnNames)
    if(length(ensembleModels) <= 1)
        restrictWeights = FALSE
    valueMissingIndex = is.na(data[[value]])
    flagMissingIndex = (data[[flag]] == missingFlag)
    # Ensure missing values agree with missing flags
    if(!all(valueMissingIndex == flagMissingIndex)){
        cat("Values that are NA: ", sum(valueMissingIndex), "\n")
        cat("Flags with missingFlag value: ", sum(flagMissingIndex), "\n")
        stop("Different missing values from flags/values!  Maybe call remove0M?")
    }
    assignColumnNames(columnNames = columnNames)
    if(is.null(names(ensembleModels)))
        names(ensembleModels) = paste("Model", 1:length(ensembleModels),
                                      sep = "_")
    if(!anyNA(data[[value]])){
        warning("No missing values in data[[value]].  Returning data[[value]]")
        return(data[[value]])
    }
    
    ensemble = data[[value]]
    missIndex = is.na(ensemble)
    cvGroup = makeCvGroup(data = data, value = value, byKey = byKey,
        groupCount = 10)
    modelFits = computeEnsembleFit(data = data, value = value, flag = flag,
        ensembleModels = ensembleModels, columnNames = columnNames)
    modelWeights = computeEnsembleWeight(data = data,
        columnNames = columnNames, value = value, flag = flag,
        ensembleModels = ensembleModels, cvGroup = cvGroup,
        fits = modelFits, restrictWeights = restrictWeights,
        maximumWeights = maximumWeights, errorType = errorType,
        errorFunction = errorFunction)
    ## print(modelWeights)
    ensembleFit = computeEnsemble(modelFits, modelWeights)
    ensemble[missIndex] = ensembleFit[missIndex]
    if(plot){
        plotEnsemble(data, modelFits, modelWeights, ensemble, value,
                     byKey, yearValue)
        plotEnsembleOld(data, modelFits, modelWeights, ensemble, value,
                     byKey)
    }
    ensemble
}
