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
    if(length(ensembleModels)<=1)
        stop("ensembleModels must be a list with at least two elements!")
    valueMissingIndex = is.na(data[[value]])
    flagMissingIndex = (data[[flag]] == missingFlag)
    # Ensure missing values agree with missing flags
    if( !all(valueMissingIndex == flagMissingIndex) ){
        cat("Values that are NA: ", sum(valueMissingIndex), "\n")
        cat("Flags with missingFlag value: ", sum(flagMissingIndex), "\n")
        stop("Different missing values from flags/values!  Maybe call remove0M?")
    }
    
    n.model = length(ensembleModels)
    ensemble = data[[value]]
    missIndex = is.na(ensemble)
    if(anyNA(ensemble)){
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
            modelNames = names(modelFits)
            for(aCode in unique(data[[byKey]])){
                filter = data[[byKey]] == aCode
                # Don't plot this value of byKey if no imputation was done
                if(!anyNA(data[[value]][filter]))
                    next
                plotMax = sapply(modelFits, function(x){
                    max(x[filter])
                })
                plot(ensemble[filter],
                     ylim = c(0, 1.1 * max(plotMax, na.rm = TRUE)),
                     type = "n", xlab = "", ylab = "")
                colPal = brewer.pal(n.model, "Paired")
                for(i in 1:n.model){
                    lines(modelFits[[modelNames[i]]][filter], col = colPal[i])
                }
                yearCount = sum(filter)
                lines(1:yearCount, ensemble[filter],
                      col = "steelblue", lwd = 3)
                points(ensemble[filter], pch = 19)
                points((1:yearCount)[missIndex[filter]],
                       ensemble[filter][missIndex[filter]],
                       col = "steelblue", cex = 1, pch = 19)
                modelWeightsToPlot =
                    weightMatrixToVector(modelWeights[filter,])
                # Reorder so sequence matches modelNames:
                modelWeightsToPlot =
                    as.data.frame(modelWeightsToPlot)[modelNames]
                legend("topleft",
                       legend = c(paste0(modelNames, "(",
                           round(modelWeightsToPlot * 100, 2),
                           "%)"), "Ensemble"),
                       col = c(colPal, "steelblue"),
                       lwd = c(rep(1, n.model), 3),
                       bty = "n")
                readline("Next?")
            }
        }
    } else {
        ensemble = data[[value]]
    }
    ensemble
}
