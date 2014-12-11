##' Function to compute the error of different model fits
##'
##' @param x A numeric vector to be imputed.
##' @param fit The fitted value from the model.
##' @param model The model fit to x.  If errorType="loocv" then this will be
##' used to compute the leave-one-out cross-validation errors.  If
##' errorType="mse", this is ignored.
##' @param errorType Specifies what type of error to compute.  Currently, "raw"
##' and "loocv" are implemented.  If "raw", then error is computed as the
##' difference between the model and observed data.  "loocv" performs
##' leave-one-out cross-validation to determine the predictive error of the
##' model.  "loocv" is more rigorous but much slower.
##' 
##' @return A vector of (the absolute value of the) errors corresponding to the
##' non-missing entries in x.
##' 
##' @note Currently, if a model fails in the cross-validation step, that error
##' is ignored when computing the mean error.  That could be improved by
##' adding a penalty when models fail to fit.
##' @export

computeErrorRate = function(x, fit, model = NULL, 
    errorType = ifelse( is.null(model), "raw", "loocv") ){
    
    ### Verify inputs match assumptions:
    if(is.null(model) & errorType == "loocv")
        stop("Cannot perform leave-one-out cross-validation without a model!")
    stopifnot( errorType %in% c("raw", "loocv") )
    stopifnot( length(fit) == length(x) )
    
    ### Run the function:
    if(all(is.na(x-fit))){
        er = rep(NA, length.out = sum( !is.na(x) ) )
    } else {
        if(errorType == "raw")
            er = (x - fit)[!is.na(x)]
        if(errorType == "loocv")
            er = computeErrorLOOCV(x, model)
    }
    abs( er )
}