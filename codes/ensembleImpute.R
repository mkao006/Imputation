##' Function to perform ensemble imputation
##'
##' This is an implementation of the ensemble imputation methodology
##' developed for the FAO production domain.
##'
##' @param x A numeric vector
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param plot Whether the result of the ensemble should be plotted.
##'
##' @export
##' 


ensembleImpute = function(x, restrictWeights = TRUE,
    maximumWeights = 0.7,
    ensembleModel = list(defaultMean, defaultLm, defaultExp,
        defaultLogistic, defaultLoess, defaultSpline, defaultArima,
        defaultMars, defaultNaive), plot = FALSE){
    ensemble = x
    missIndex = is.na(ensemble)
    if(any(is.na(x))){
        if(length(unique(na.omit(x))) == 1){
            ensemble = defaultMean(x)
        } else {
            modelFits = lapply(ensembleModel,
                FUN = function(x, value) x(value), value = x)
            modelWeights = computeEnsembleWeight(x, modelFits,
                restrictWeights = restrictWeights,
                maximumWeights = maximumWeights)
            ## print(modelWeights)
            ensembleFit = computeEnsemble(modelFits, modelWeights)
            ensemble[missIndex] = ensembleFit[missIndex]
            if(plot){
                plot(x, ylim = range(lapply(modelFits, range),
                            na.rm = TRUE))
                lapply(modelFits, lines)
                lines(ensemble, col = "red", lwd = 2)
            }
        }
    } else {
        ensemble = x
    }
    ensemble
}
