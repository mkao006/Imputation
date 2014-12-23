##' Class for Ensemble Model
##'
##' Ensemble models contain a definition (function) of the algorithm used to
##' fit the model as well as additional parameters:\n
##' Extrapolation range: defines how far outside the range of the data this
##' model is valid.\n
##' Level: defines if this model operates on each country-commodity pair
##' individually ("countryCommodity" level) or all countries for a fixed
##' commodity ("commodity" level) at once.
##'
##' @param model The function defining how the model is fit to the data.  The
##' function should take arguments data, value, flag, columnNames, and
##' missingIndex (see ?extendSimpleModel).
##' @param extrapolationRange How many time steps outside of the data is this
##' model valid for?  Should be a positive integer (or Inf).
##' @param level The level at which this model is applied.  Currently, must be
##' one of "countryCommodity" or "commodity".
##'
##' @export
##' 

checkEnsembleModel = function(object){
    errors = character()
    if( object@extrapolationRange < 0 ){
        msg = "extrapolationRange can't be negative!"
        errors = c(errors, msg)
    }
    if( ! object@level %in% c("commodity", "countryCommodity") ){
        msg = paste("level must be one of commodity or countryCommodity.")
        errors = c(errors, msg)
    }
    if(length(errors) == 0)
        TRUE
    else
        errors
}

ensembleModel = setClass( Class = "ensembleModel",
    representation = representation(model = "function",
                                    extrapolationRange = "numeric",
                                    level = "character"),
    prototype(extrapolationRange = 0,
              level = "countryCommodity"),
    validity = checkEnsembleModel,
    package = "faoswsProductionImputation")