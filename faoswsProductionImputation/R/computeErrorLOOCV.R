##' LOOCV Error Rate
##' 
##' This function computes the error of different model fits via leave-one-out
##' cross-validation.  However, typically this function will be called via
##' computeErrorRate and not directly.
##'
##' @param x A numeric vector to be imputed.
##' @param model The model fit to x.  If errorType="loocv" then this will be used
##' to compute the leave-one-out cross-validation errors.  If errorType="raw", this
##' is ignored.
##' @note Currently, if a model fails in the cross-validation step, that error
##' is ignored when computing the mean error.  That could be improved by adding
##' a penalty when models fail to fit, or setting the error to some large value.
##' @export

computeErrorLOOCV = function(x, model){
    validObservationIndex = (1:length(x))[!is.na(x)]
    error = sapply( validObservationIndex, function(i){
        #Copy x and remove the ith observation to fit the model
        xTemporary = x
        xTemporary[i] = NA
        fitTemporary = model(xTemporary)
        outOfBagPrediction = fitTemporary[i]
        #Compare fitted value to observed (out of bag) value
        outOfBagError = x[i] - outOfBagPrediction
        return(outOfBagError)
    })
    return(error)
}