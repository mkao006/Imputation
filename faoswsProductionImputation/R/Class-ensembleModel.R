##' Class for Ensemble Model
##'
##' Ensemble models contain a definition (function) of the algorithm used to
##' fit the model as well as additional parameters:
##' Extrapolation range: defines how far outside the range of the data this
##' model is valid.
##' Level: defines if this model operates on each country-commodity pair
##' individually ("countryCommodity" level) or all countries for a fixed
##' commodity ("commodity" level) at once.
##'
##' @param model The function defining how the model is fit to the data.  The
##' function should take arguments data, value, flag, and columnNames (see
##' extendSimpleModel).
##' @param extrapolationRange How many time steps outside of the data is this
##' model valid for?  Should be a positive integer (or Inf).
##' @param level The level at which this model is applied.  Currently, must be
##' one of "countryCommodity" or "commodity".
##'
##' @export
##' 

checkEnsembleModel = function(object){
    errors = character()
    if(object@extrapolationRange < 0){
        msg = "extrapolationRange can't be negative!"
        errors = c(errors, msg)
    }
    if(!object@level %in% c("commodity", "countryCommodity")){
        msg = "level must be one of commodity or countryCommodity."
        errors = c(errors, msg)
    }
    modelArguments = names(as.list(args(object@model)))
    if(object@level == "commodity"){
        requiredColumns = c("data", "value", "flag", "columnNames")
        missing = requiredColumns[!requiredColumns %in% modelArguments]
        if(length(missing) > 1){
            msg = paste("model missing required arguments:",
                paste(missing, collapse=", "))
            errors = c(errors, msg)
        }
    } else {
        # modelArguments should be the one argument and "", so length == 2
        if(length(modelArguments) != 2){
            msg = "Model should only contain one argument"
            errors = c(errors, msg)
        }
    }
    if(length(errors) == 0)
        TRUE
    else
        errors
}

ensembleModel = setClass(Class = "ensembleModel",
    representation = representation(model = "function",
                                    extrapolationRange = "numeric",
                                    level = "character"),
    prototype(extrapolationRange = 0,
              level = "countryCommodity"),
    validity = checkEnsembleModel,
    package = "faoswsProductionImputation")