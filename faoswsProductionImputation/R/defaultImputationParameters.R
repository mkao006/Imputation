##' Default Imputation Parameters
##' 
##' This function can be used to generate the input parameters for the
##' ensemble imputation code.  This is a good way to get a list of the required
##' parameters and then modify parameters to match your particular
##' configuration.
##' 
##' @param variable Should be one of "production", "yield", or "seed".  These
##' are currently the three variables for which this imputation package has
##' been used.  It's straightforward to implement another type: simply add a
##' new if statement in the code below to assign the correct column types.
##' However, you will also need to update ensureImputationInputs to accept this
##' new variable.
##' 
##' @return Returns a list of the default parameters used in the ensemble
##' imputation algorithms.
##' 
##' @export
##' 

defaultImputationParameters = function(variable){
    out = list(
         yearValue = "timePointYears",
         byKey = "geographicAreaM49",
         ensembleModels = allDefaultModels(),
         restrictWeights = TRUE,
         maximumWeights = 0.7,
         plotImputation = TRUE,
         errorType = "loocv",
         errorFunction = function(x) mean(x^2),
         groupCount = 10,
         missingFlag = "M",
         imputationFlag = "I",
         newMethodFlag = "E",
         flagTable = faoswsFlagTable)
    out$variable = variable
    if(variable == "production"){
        out$imputationValueColumn = "Value_measuredElement_5510"
        out$imputationFlagColumn = "flagObservationStatus_measuredElement_5510"
        out$imputationMethodColumn = "flagMethod_measuredElement_5510"
    } else if(variable == "yield"){
        out$imputationValueColumn = "Value_measuredElement_5416"
        out$imputationFlagColumn = "flagObservationStatus_measuredElement_5416"
        out$imputationMethodColumn = "flagMethod_measuredElement_5416"
    } else if(variable == "seed"){
        out$imputationValueColumn = "Value_measuredElement_5525"
        out$imputationFlagColumn = "flagObservationStatus_measuredElement_5525"
        out$imputationMethodColumn = "flagMethod_measuredElement_5525"
    } else {
        stop("This variable has not yet been implemented!")
    }
    return(out)
}