##' Function to compute the error of different model fits
##'
##' @param x A numeric vector to be imputed.
##' @param fit The fitted value from the model.
##' @param model The model fit to x.  If errorType="loocv" then this will be used
##' to compute the leave-one-out cross-validation errors.  If errorType="mse", this
##' is ignored.
##' @param errorType Specifies what type of error to compute.  Currently, "mse"
##' and "loocv" are implemented.  "mse" computes the mean squared error between
##' the model and observed data.  "loocv" performs leave-one-out
##' cross-validation to determine the predictive error of the model.  "loocv"
##' is more rigorous but much slower.
##' @note Currently, if a model fails in the cross-validation step, that error
##' is ignored when computing the mean error.  That could be improved by adding
##' a penalty when models fail to fit.
##' @export

computeErrorRate = function(x, fit, model = NULL, 
    errorType = ifelse( is.null(model), "mse", "loocv")){
    
    ### Verify inputs match assumptions:
    if(is.null(model) & errorType == "loocv")
        stop("Cannot perform leave-one-out cross-validation without a model!")
    stopifnot( errorType %in% c("mse", "loocv") )
    stopifnot( length(fit) == length(x) )
    
    ### Run the function:
    if(errorType == "mse")
        er = mean((x - fit)^2, na.rm = !all(is.na(fit)))    
    if(errorType == "loocv")
        er = computeErrorLOOCV(x, model)
    er
}