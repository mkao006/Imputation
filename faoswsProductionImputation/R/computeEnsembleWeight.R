##' Function to compute the weights of the ensemble models
##'
##' @param  x A numeric vector to be imputed.
##' @param fits The fitted value from the models.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param errorType See ?computeErrorRate.
##' @param errorFunction Function taking a vector of errors and returning a
##' positive value.  Smaller values should indicate a better fit to the data.
##' The default is the mean-squared error (so f(x)=mean(x^2)), but other
##' functions (such as the median absolute deviation, or f(x)=median(abs(x)))
##' can be used.  Note: NA values will be removed prior to calling this
##' function.  Also, the errors are all positive, so the median absolute
##' deviation can also be computed with f(x)=median(x).
##' @param ensembleModel A list of the models fit.  This is only needed if
##' errorType = "loocv".
##' @param modelExtrapolationRange A numeric vector specifying the valid range
##' of extrapolation for each model.  This vector must be the same length as
##' ensembleModel, as it's i-th element gives the extrapolation range for the
##' i-th element of ensembleModel.
##' 
##' @export


computeEnsembleWeight = function(x, fits, restrictWeights = TRUE,
    maximumWeights = 0.7, errorType = "mse",
    errorFunction = function(x) mean(x^2), ensembleModel = NULL,
    modelExtrapolationRange = rep(Inf, length(ensembleModel)) ){
    
    ### Verify inputs match assumptions:
    if( !all( lapply(fits, length) == length(x) ) )
        stop("All elements of fits must have the same length as x!")
    if( !is.null(ensembleModel) )
        stopifnot( all( names(fits) == names(ensembleModel) ) )
    stopifnot( errorType %in% c("raw", "loocv") )
    stopifnot( maximumWeights <= 1 & maximumWeights >= 0.5 )
    if(errorType=="loocv" & is.null(ensembleModel) )
        stop("ensembleModel must be provided if errorType='loocv'")
    stopifnot( length(modelExtrapolationRange)==length(ensembleModel) )
    
    ### Run the function
    benchmark = x
    error = sapply(1:length(fits),
        FUN = function(i){
            computeErrorRate(x = benchmark, fit = fits[[i]],
                model = ensembleModel[[i]], errorType = errorType )
            }
        )
    # Find the columns that have no non-NA values
    NAcolumns = apply(error, 2, function(x){ all( is.na( x ) ) } )
    # Sometimes, a model will fail in the leave-out-one cross-validation.  If
    # that happens, we'll get a missing value (NA) for that error.  To work
    # around this, assign that NA to the highest value of that row.  In other
    # words, assumme the model that failed did as poor as possible.
    error[,!NAcolumns] = apply(error[,!NAcolumns], 1, function(x){
        ifelse(is.na(x), max(x, na.rm=T), x)
    })
    error = apply( error, 2, errorFunction )
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
    weights = weightVectorToMatrix( x = x, w = weights
        ,ensembleModel = ensembleModel
        ,modelExtrapolationRange = modelExtrapolationRange )
    weights
}    
