##' Function to compute the weights of the ensemble models
##'
##' @param  x A numeric vector to be imputed.
##' @param fits The fitted value from the models.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param errorType See ?computeErrorRate.
##' @param ensembleModel A list of the models fit.  This is only needed if
##' errorType = "loocv".
##' @export


computeEnsembleWeight = function(x, fits, restrictWeights = TRUE,
    maximumWeights = 0.7, errorType = "mse", ensembleModel = NULL){
    
    ### Verify inputs match assumptions:
    if( !all( lapply(fits, length) == length(x) ) )
        stop("All elements of fits must have the same length as x!")
    if( !is.null(ensembleModel) )
        stopifnot( all( names(fits) == names(ensembleModel) ) )
    stopifnot( errorType %in% c("mse", "loocv") )
    stopifnot( maximumWeights <= 1 & maximumWeights >= 0.5 )
    if(errorType=="loocv" & is.null(ensembleModel) )
        stop("ensembleModel must be provided if errorType='loocv'")
    
    ### Run the function:
    benchmark = x
    error = sapply(1:length(fits),
        FUN = function(i){
            computeErrorRate(x = benchmark, fit = fits[[i]], errorType = errorType
                ,model = ensembleModel[[i]] )
            }
        )
    ## NOTE (Michael): Maybe change this to uniform weight
    error[error < 1e-3] = mean(error[error >= 1e-3], na.rm = TRUE)
    weights = (1/error^2)/sum(1/error^2, na.rm = TRUE)
    weights[is.na(weights)] = 0
    if(restrictWeights & any(weights > maximumWeights)){
        weights[weights < maximumWeights] =
            weights[weights < maximumWeights] *
                ((1 - maximumWeights)/
                 sum(weights[weights < maximumWeights]))
        weights[weights > maximumWeights] = maximumWeights
    }
    weights
}    
