##' Function to predict based on the model estimated from shocklme4
##'
##' @param object the fitted model returned by meanlme4
##' @param newdata the new data object
##'
##' @export
##' 

predict.shocklme4 = function(object, newdata){
    if(is.null(object$groupedChange)){
        newPredictData = newdata
    } else {
        newPredictData = data.table(newdata,
            groupedChange = object$groupedChange)
    }
    predict(object$model, newdata = newPredictData,
            allow.new.levels = TRUE)
}
