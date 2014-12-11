##' Function to compute the weights of the ensemble models
##'
##' @param  x A numeric vector to be imputed.
##' @param ensembleModel The list of ensemble models
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @export


computeEnsembleWeight = function(x, ensembleModel){
    obs = which(!is.na(x))
    modelFits = matrix(NA, nr = length(obs), nc = length(ensembleModel))
    for(i in 1:length(obs)){
        tmp = x
        tmp[obs[i]] = NA
        modelFits[i, ] =
            sapply(computeEnsembleFit(x = tmp, ensembleModel = ensembleModel),
                   FUN = function(x) x[i])
    }
    stackingCoef = coef(nnls(modelFits, matrix(bahrainExample[obs], nc = 1)))
    weights = stackingCoef/sum(stackingCoef)
    weights
}
